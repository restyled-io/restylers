module Freckle.ActivityFeed.Model.StudentActivity.Manipulation
  ( combineActivitiesBySession
  , combineHighlightActivitiesBySession
  ) where

import Freckle.ActivityFeed.Internal.Prelude
import Prelude qualified as Unsafe (foldr1)

import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.Monoid (Any (..), Sum (..))
import Freckle.ActivityFeed.Model.Accuracy.Calculation
import Freckle.ActivityFeed.Model.Score
import Freckle.ActivityFeed.Model.StudentActivity
import Freckle.ActivityFeed.Model.StudentActivity qualified as StudentActivity
import Freckle.ActivityFeed.Model.StudentHighlightActivity
import Freckle.ActivityFeed.Model.StudentHighlightActivity qualified as StudentHighlightActivity

combineActivitiesBySession
  :: [StudentActivity 'Fragment] -> [StudentActivity 'Final]
combineActivitiesBySession =
  sortOn (Down . (.timeCompleted))
    . fmap (finalizeActivity . Unsafe.foldr1 unsafeCombineStudentActivities)
    . groupActivities

combineHighlightActivitiesBySession
  :: [StudentHighlightActivity] -> [StudentHighlightActivity]
combineHighlightActivitiesBySession =
  sortOn (Down . (.timeCompleted))
    . fmap
      ( finalizeHighlightActivity
          . Unsafe.foldr1 unsafeCombineStudentHighlightActivities
      )
    . groupHighlightActivities

-- | Finalize a `StudentActivity`s aggregation
--
-- A fragment of an activity may represent a single session or multiple
-- fragments of a session. A finalized activity represents the session in
-- total. To produce correct downstream aggregates certain metrics must be
-- condensed in the final aggregate.
--
-- Average must be condensed to have correct weight. Otherwise individual
-- fragment weights would cause odd results
finalizeActivity :: StudentActivity 'Fragment -> StudentActivity 'Final
finalizeActivity activity =
  activity
    { StudentActivity.score =
        maybe activity.score (Just . ScorePercentage . averageDatum)
          $ getAverage
          =<< activityToAccuracyPercentage activity
    }

finalizeHighlightActivity
  :: StudentHighlightActivity -> StudentHighlightActivity
finalizeHighlightActivity activity =
  activity
    { StudentHighlightActivity.score =
        maybe activity.score (Just . ScorePercentage . averageDatum)
          $ getAverage
          =<< activityToAccuracyPercentage activity
    }

unsafeCombineStudentActivities
  :: HasCallStack
  => StudentActivity 'Fragment
  -> StudentActivity 'Fragment
  -> StudentActivity 'Fragment
unsafeCombineStudentActivities x y = unwrap $ combineStudentActivities x y
 where
  unwrap (Left err) = error err
  unwrap (Right a) = a

combineStudentActivities
  :: StudentActivity 'Fragment
  -> StudentActivity 'Fragment
  -> Either String (StudentActivity 'Fragment)
combineStudentActivities x y = do
  when (x.studentId /= y.studentId)
    $ Left ("student IDs are not equal: " ++ show (x, y))
  when (x.product /= y.product)
    $ Left ("products are not equal: " ++ show (x, y))
  when (x.sessionId /= y.sessionId)
    $ Left ("session IDs are not equal" ++ show (x, y))
  when (x.topic /= y.topic)
    $ Left ("topics are not equal" ++ show (x, y))
  pure
    StudentActivity
      { studentId = x.studentId
      , gradeLevel = liftA2 max x.gradeLevel y.gradeLevel
      , contentLevel = x.contentLevel <|> y.contentLevel
      , minutesPracticed =
          case (x.minutesPracticed, y.minutesPracticed) of
            (Just a, Just b) -> Just $ a + b
            (ma, mb) -> ma <|> mb
      , numberCorrect =
          getSum <$> (Sum <$> x.numberCorrect) <> (Sum <$> y.numberCorrect)
      , numberQuestions =
          getSum <$> (Sum <$> x.numberQuestions) <> (Sum <$> y.numberQuestions)
      , product = x.product
      , assignmentId = x.assignmentId
      , sessionId = x.sessionId
      , score = do
          ScorePercentage xScore <- x.score
          ScorePercentage yScore <- y.score
          pure $ ScorePercentage (xScore <> yScore)
      , worth = getSum <$> (Sum <$> x.worth) <> (Sum <$> y.worth)
      , timeCompleted = max x.timeCompleted y.timeCompleted
      , topic = x.topic
      , mathSubSkillPracticed =
          getAny
            <$> (Any <$> x.mathSubSkillPracticed)
            <> (Any <$> y.mathSubSkillPracticed)
      , prerequisiteAssignment =
          getAny
            <$> (Any <$> x.prerequisiteAssignment)
            <> (Any <$> y.prerequisiteAssignment)
      }

unsafeCombineStudentHighlightActivities
  :: HasCallStack
  => StudentHighlightActivity
  -> StudentHighlightActivity
  -> StudentHighlightActivity
unsafeCombineStudentHighlightActivities x y = unwrap $ combineStudentHighlightActivities x y
 where
  unwrap (Left err) = error err
  unwrap (Right a) = a

combineStudentHighlightActivities
  :: StudentHighlightActivity
  -> StudentHighlightActivity
  -> Either String StudentHighlightActivity
combineStudentHighlightActivities x y = do
  when (x.studentId /= y.studentId)
    $ Left ("student IDs are not equal: " ++ show (x, y))
  when (x.product /= y.product)
    $ Left ("products are not equal: " ++ show (x, y))
  when (x.sessionId /= y.sessionId)
    $ Left ("session IDs are not equal" ++ show (x, y))
  pure
    StudentHighlightActivity
      { studentId = x.studentId
      , minutesPracticed =
          case (x.minutesPracticed, y.minutesPracticed) of
            (Just a, Just b) -> Just $ a + b
            (ma, mb) -> ma <|> mb
      , numberCorrect =
          getSum <$> (Sum <$> x.numberCorrect) <> (Sum <$> y.numberCorrect)
      , numberQuestions =
          getSum <$> (Sum <$> x.numberQuestions) <> (Sum <$> y.numberQuestions)
      , product = x.product
      , assignmentId = x.assignmentId
      , sessionId = x.sessionId
      , score = do
          ScorePercentage xScore <- x.score
          ScorePercentage yScore <- y.score
          pure $ ScorePercentage (xScore <> yScore)
      , timeCompleted = max x.timeCompleted y.timeCompleted
      }

groupHighlightActivities
  :: [StudentHighlightActivity] -> [NonEmpty StudentHighlightActivity]
groupHighlightActivities =
  HashMap.elems
    . HashMap.fromListWith (<>)
    . fmap (((.product) &&& (.sessionId)) &&& pure)

groupActivities
  :: [StudentActivity 'Fragment] -> [NonEmpty (StudentActivity 'Fragment)]
groupActivities =
  HashMap.elems
    . HashMap.fromListWith (<>)
    . fmap (((.product) &&& (.sessionId)) &&& pure)
