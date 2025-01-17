{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Freckle.Api.Handlers.Auth.RenaissanceSsoSpec
  ( spec
  ) where

import TestImport

import Data.Aeson.KeyMap qualified as KeyMap
import Data.BCP47 qualified as BCP47
import Data.Text qualified as T
import Freckle.Api.Auth.Jwt (ExpiresAt (..), JwtParam (..), encodeJwt)
import Freckle.Api.Auth.RenaissanceSso.AuthPlugin (pluginName)
import Freckle.Api.Dal.Utilities.Persistent
import Freckle.Api.Test.School (setSchoolHeaders)
import Freckle.Core.IdentityProvider (IdentityProvider (RenaissanceIdp))
import Freckle.Core.JwtCredentials (defaultJwtCredentials)
import Freckle.Entities.Course
import Freckle.Entities.CourseMembership
import Freckle.Entities.District
import Freckle.Entities.License
import Freckle.Entities.RenaissanceSchoolClient
import Freckle.Entities.School
import Freckle.Entities.SchoolAdmin
import Freckle.Entities.SchoolAdminSchoolMembership
import Freckle.Entities.Student
import Freckle.Entities.StudentIdentifier
import Freckle.Entities.Teacher
import Freckle.Renaissance.AppTag (AppTag (AppTagFreckle, AppTagOther))
import Freckle.Renaissance.AppTags (AppTags (AppTags))
import Freckle.Renaissance.RPIdentifier (RPIdentifier (..), retagRPIdentifier)
import Freckle.Sis (SisId)
import Test.QuickCheck qualified as QuickCheck
import Yesod.Auth (Route (PluginR))

-- See tests/Freckle/Api/Auth/Jwt.hs for testing of generic JWT
-- functionality, e.g. expiration, audience, malformed payload

spec :: Spec
spec = withApp loadApp $ do
  describe "POST /auth/page/renaissance-sso/session" $ do
    let route = AuthR $ PluginR pluginName ["session"]

    describe "renaissance-sso student login" $ do
      describe "when parsing the student jwt" $ do
        it "rejects a student with no classes" $ do
          studentId <- liftIO $ QuickCheck.generate arbitrary

          postJwt route =<< mkStudent studentId Nothing (Just "1") []

          body <- statusIs 401 >> getJsonBody
          body `shouldBeMessage` ["Student has no classes"]

        it "rejects a student with no grade" $ do
          Ids {..} <- genIds

          postJwt route
            =<< mkStudent
              studentId1
              Nothing
              Nothing
              [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
              ]

          body <- statusIs 401 >> getJsonBody
          body `shouldBeMessage` ["Student has no grade"]

        it "rejects a student with a grade outside the acceptable range" $ do
          Ids {..} <- genIds

          postJwt route
            =<< mkStudent
              studentId1
              Nothing
              (Just "13")
              [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
              ]

          body <- statusIs 401 >> getJsonBody
          body
            `shouldBeMessage` [ "Could not decode token payload:"
                              , "Error in $.grade:"
                              , "Cannot convert \"13\" to SchoolGrade"
                              ]

      describe "when the student does not exist" $ do
        it "rejects the student if none of their schools exist" $ withGraph $ do
          Ids {..} <- genIds

          postJwt route
            =<< mkStudent
              studentId1
              Nothing
              (Just "1")
              [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
              ]

          body <- statusIs 401 >> getJsonBody
          body
            `shouldBeMessage` [ "Login not found: Student "
                              , getRPIdentifier studentId1
                              , " Error: None of the specified schools are known to Freckle."
                              , " Has the lead teacher for your class validated their email?"
                              ]

        it "creates the teacher and student if at least one of their schools exist" $ withGraph $ do
          Ids {..} <- genIds

          Entity frDistrictId _ <- node () mempty

          void
            $ node (only frDistrictId)
            $ edit
            $ setField SchoolRenaissanceRPIdentifier
            $ Just schoolId1

          postJwt route
            =<< mkStudent
              studentId1
              Nothing
              (Just "1")
              [classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing]

          body <- statusIs 200 >> getJsonBody
          body `shouldBeMessage` ["Login Successful"]

          void $ fetchJust $ getBy $ RenaissanceStudentsUnique $ Just studentId1
          void $ fetchJust $ getBy $ RenaissanceTeachersUnique $ Just teacherId1

        it
          "accepts the student and creates a new teacher if their only course is associated with a different teacher"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier (Just schoolId1)

            Entity frTeacherId _ <-
              node (only $ Just frSchoolId)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            void
              $ node (frTeacherId, Just frSchoolId)
              $ edit
              $ setField CourseRenaissanceRPIdentifier (Just courseId1)

            postJwt route
              =<< mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId2 email2 schoolId1 Nothing
                ]

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

            void
              $ fetchJust
              $ getBy
              $ RenaissanceStudentsUnique
              $ Just studentId1

            void
              $ fetchJust
              $ getBy
              $ RenaissanceTeachersUnique
              $ Just teacherId1

        it
          "accepts the student if their only teacher is in one school, but their class is in another school in the same district"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier (Just schoolId1)

            void
              $ node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier (Just schoolId2)

            Entity frTeacherId _ <-
              node (only $ Just frSchoolId)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            void
              $ node (frTeacherId, Just frSchoolId)
              $ edit
              $ setField CourseRenaissanceRPIdentifier (Just courseId1)

            postJwt route
              =<< mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId1 email1 schoolId2 Nothing
                ]

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

        it
          "rejects the student if their only teacher is in one school, but their class is in another school in a different district"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId1 _ <- node () mempty
            Entity frDistrictId2 _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId1)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier (Just schoolId1)

            void
              $ node (only frDistrictId2)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier (Just schoolId2)

            Entity frTeacherId _ <-
              node (only $ Just frSchoolId)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            void
              $ node (frTeacherId, Just frSchoolId)
              $ edit
              $ setField CourseRenaissanceRPIdentifier (Just courseId1)

            postJwt route
              =<< mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId1 email1 schoolId2 Nothing
                ]

            body <- statusIs 401 >> getJsonBody
            body
              `shouldBeMessage` [ "Login not found: Student"
                                , getRPIdentifier studentId1
                                , " Error: Login token is incoherent. Has the lead teacher for your class validated their email?"
                                ]

        it "creates the student if any of their courses exist and are coherent"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier (Just schoolId1)

            Entity frTeacherId _ <-
              node (only $ Just frSchoolId)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            Entity frCourseId _ <-
              node (frTeacherId, Just frSchoolId)
                $ edit
                $ setField CourseRenaissanceRPIdentifier (Just courseId1)

            postJwt route
              =<< mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
                , classFields courseId2 "Class 2" teacherId2 email2 schoolId1 Nothing
                ]

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

            Entity frStudentId _ <-
              fetchJust
                $ getBy
                $ RenaissanceStudentsUnique
                $ Just
                  studentId1

            void
              $ fetchJust
              $ getBy
              $ CourseMembershipsStudentIdCourseIdIdx
                frStudentId
                frCourseId

        it "updates `courses.rl_assigned_products` for existing courses" $ withGraph $ do
          Ids {..} <- genIds

          Entity frDistrictId _ <- node () mempty

          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId1

          Entity frTeacherId _ <-
            node (only $ Just frSchoolId)
              $ edit
              $ setField TeacherRenaissanceRPIdentifier
              $ Just teacherId1

          Entity _frCourseId _ <-
            node (frTeacherId, Just frSchoolId)
              $ edit
              $ setField CourseRenaissanceRPIdentifier (Just courseId1)
              . setField CourseName "Extant Course Name"
              . setField CourseRlAssignedProducts Nothing

          let
            jwtCourseName = "Class 1"
            jwtAppTags = AppTags appTags
            appTags =
              AppTagOther "APPS_AR"
                :| [AppTagOther "APPS_ST", AppTagFreckle Nothing]

          postJwt route
            =<< mkStudent
              studentId1
              Nothing
              (Just "1")
              [ classFields
                  courseId1
                  jwtCourseName
                  teacherId1
                  email1
                  schoolId1
                  (Just jwtAppTags)
              ]

          body <- statusIs 200 >> getJsonBody
          body `shouldBeMessage` ["Login Successful"]

          Entity _courseId course <-
            fetchJust
              $ getBy
              $ RenaissanceCoursesUniqueToTeacher
                frTeacherId
                (pure courseId1)

          courseRlAssignedProducts course `shouldBe` Just (JSONB jwtAppTags)
          courseName course `shouldBe` jwtCourseName

        it
          "creates the student and course if any of their teachers exist and are coherent"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier (Just schoolId1)

            Entity frTeacherId _ <-
              node (only $ Just frSchoolId)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            postJwt route
              =<< mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
                , classFields courseId2 "Class 2" teacherId2 email2 schoolId1 Nothing
                ]

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

            Entity frStudentId _ <-
              fetchJust
                $ getBy
                $ RenaissanceStudentsUnique
                $ Just studentId1

            Entity frCourseId _ <-
              fetchJust
                $ getBy
                $ RenaissanceCoursesUniqueToTeacher frTeacherId
                $ Just courseId1

            void
              $ fetchJust
              $ getBy
              $ CourseMembershipsStudentIdCourseIdIdx frStudentId frCourseId

        it
          "creates the student and courses if any of their teachers exist even if they share courses"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier (Just schoolId1)

            Entity frTeacherId1 _ <-
              node (only $ Just frSchoolId)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            Entity frTeacherId2 _ <-
              node (only $ Just frSchoolId)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier (Just teacherId2)

            postJwt route
              =<< mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
                , classFields courseId1 "Class 1" teacherId2 email2 schoolId1 Nothing
                ]

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

            Entity frStudentId _ <-
              fetchJust
                $ getBy
                $ RenaissanceStudentsUnique
                $ Just studentId1

            Entity frCourseId1 _ <-
              fetchJust
                $ getBy
                $ RenaissanceCoursesUniqueToTeacher frTeacherId1
                $ Just courseId1

            Entity frCourseId2 _ <-
              fetchJust
                $ getBy
                $ RenaissanceCoursesUniqueToTeacher frTeacherId2
                $ Just courseId1

            void
              $ fetchJust
              $ getBy
              $ CourseMembershipsStudentIdCourseIdIdx frStudentId frCourseId1

            void
              $ fetchJust
              $ getBy
              $ CourseMembershipsStudentIdCourseIdIdx frStudentId frCourseId2

        it
          "creates the student and courses if any of their teachers exist even if they are in different schools"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId1 _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier (Just schoolId1)

            Entity frSchoolId2 _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier (Just schoolId2)

            Entity frTeacherId1 _ <-
              node (only $ Just frSchoolId1)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            Entity frTeacherId2 _ <-
              node (only $ Just frSchoolId2)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier (Just teacherId2)

            postJwt route
              =<< mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
                , classFields courseId2 "Class 2" teacherId2 email2 schoolId2 Nothing
                ]

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

            Entity frStudentId _ <-
              fetchJust
                $ getBy
                $ RenaissanceStudentsUnique
                $ Just studentId1

            Entity frCourseId1 _ <-
              fetchJust
                $ getBy
                $ RenaissanceCoursesUniqueToTeacher frTeacherId1
                $ Just courseId1

            Entity frCourseId2 _ <-
              fetchJust
                $ getBy
                $ RenaissanceCoursesUniqueToTeacher frTeacherId2
                $ Just courseId2

            void
              $ fetchJust
              $ getBy
              $ CourseMembershipsStudentIdCourseIdIdx frStudentId frCourseId1

            void
              $ fetchJust
              $ getBy
              $ CourseMembershipsStudentIdCourseIdIdx frStudentId frCourseId2

        it
          "rejects the student if student district ID does not match JWT district"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId1 _ <- node @District () mempty
            Entity frDistrictId2 _ <- node @District () mempty

            void
              $ node @Student (Only Nothing)
              $ edit
              $ (persistFieldLens StudentDistrictId ?~ frDistrictId1)
              . (persistFieldLens StudentRenaissanceRPIdentifier ?~ studentId1)

            void
              $ node @School (Only frDistrictId2)
              $ edit
              $ persistFieldLens SchoolRenaissanceRPIdentifier
              ?~ schoolId1

            postJwt route
              =<< mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
                ]

            body <- statusIs 401 >> getJsonBody
            body
              `shouldBeMessage` [ "Login not found: Student "
                                , getRPIdentifier studentId1
                                , " Error: Current student district ID "
                                , toPathPiece frDistrictId1
                                , " differs from JWT district ID "
                                , toPathPiece frDistrictId2
                                ]
        it
          "rejects the student if any of their teachers exist in different districts"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId1 _ <- node () mempty
            Entity frDistrictId2 _ <- node () mempty

            Entity frSchoolId1 _ <-
              node (only frDistrictId1)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier (Just schoolId1)

            Entity frSchoolId2 _ <-
              node (only frDistrictId2)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier (Just schoolId2)

            void
              $ node (only $ Just frSchoolId1)
              $ edit
              $ setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            void
              $ node (only $ Just frSchoolId2)
              $ edit
              $ setField TeacherRenaissanceRPIdentifier (Just teacherId2)

            postJwt route
              =<< mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
                , classFields courseId2 "Class 2" teacherId2 email2 schoolId2 Nothing
                ]

            body <- statusIs 401 >> getJsonBody
            body
              `shouldBeMessage` [ "Login not found: Student "
                                , getRPIdentifier studentId1
                                , " Error: Multiple district IDs found in JWT - "
                                , T.intercalate ", "
                                    $ map
                                      toPathPiece
                                      [frDistrictId1, frDistrictId2]
                                ]

        it "is idempotent with respect to course[membership] creation"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier (Just schoolId1)

            Entity frTeacherId _ <-
              node (only $ Just frSchoolId)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            payload <-
              mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
                , classFields courseId2 "Class 2" teacherId2 email2 schoolId1 Nothing
                ]

            postJwt route payload
            body1 <- statusIs 200 >> getJsonBody
            body1 `shouldBeMessage` ["Login Successful"]

            Entity frStudentId _ <-
              fetchJust
                $ getBy
                $ RenaissanceStudentsUnique
                $ Just studentId1

            (courseIdsBefore, membershipIdsBefore) <-
              fetchSnapshot frTeacherId frStudentId

            postJwt route payload
            body2 <- statusIs 200 >> getJsonBody
            body2 `shouldBeMessage` ["Login Successful"]

            (courseIdsAfter, membershipIdsAfter) <-
              fetchSnapshot frTeacherId frStudentId

            courseIdsBefore `shouldMatchList` courseIdsAfter
            membershipIdsBefore `shouldMatchList` membershipIdsAfter

      let genRenStudent renaissanceId frTeacherId edits = do
            Entity frStudentId _ <-
              node (only $ Just frTeacherId)
                $ edit
                $ setField StudentRenaissanceRPIdentifier (Just renaissanceId)
                . edits

            let (renaissanceIdType, opaqueRenaissanceId) =
                  untagIdentifier $ TaggedRpIdentifier renaissanceId
            void
              $ node @StudentIdentifier (only frStudentId)
              $ edit
              $ setField StudentIdentifierIdentifierType renaissanceIdType
              . setField StudentIdentifierIdentifier opaqueRenaissanceId

            pure frStudentId

      describe "when the student does exist" $ do
        it
          "accepts the student and creates a new teacher if their only course is associated with a different teacher"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId1

            Entity frTeacherId _ <-
              node (only $ Just frSchoolId)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier
                $ Just teacherId1

            Entity frCourseId _ <-
              node (frTeacherId, Just frSchoolId)
                $ edit
                $ setField
                  CourseRenaissanceRPIdentifier
                  (Just courseId1)

            frStudentId <- genRenStudent studentId1 frTeacherId id

            void $ node @CourseMembership (frStudentId, frCourseId) mempty

            postJwt route
              =<< mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId2 email2 schoolId1 Nothing
                ]

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

            void
              $ fetchJust
              $ getBy
              $ RenaissanceTeachersUnique
              $ Just teacherId1

        it
          "accepts the student if their only teacher is in one school, but their class is in another school in the same district"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId1

            void
              $ node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId2

            Entity frTeacherId _ <-
              node (only $ Just frSchoolId)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier
                $ Just teacherId1

            Entity frCourseId _ <-
              node (frTeacherId, Just frSchoolId)
                $ edit
                $ setField
                  CourseRenaissanceRPIdentifier
                  (Just courseId1)

            frStudentId <- genRenStudent studentId1 frTeacherId id

            void $ node @CourseMembership (frStudentId, frCourseId) mempty

            postJwt route
              =<< mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId1 email1 schoolId2 Nothing
                ]

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

        it
          "allows a student to login if sync fails, but they already had a course from the JWT"
          $ withGraph
          $ do
            -- Specifically, the sync fails here because it would require moving a teacher into
            -- another district. However, we can still let the student log into the course
            -- that already exists.
            Ids {..} <- genIds

            Entity frDistrictId1 _ <- node () mempty
            Entity frDistrictId2 _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId1)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId1

            void
              $ node (only frDistrictId2)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier (Just schoolId2)

            Entity frTeacherId _ <-
              node (only $ Just frSchoolId)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            Entity frCourseId _ <-
              node (frTeacherId, Just frSchoolId)
                $ edit
                $ setField CourseRenaissanceRPIdentifier (Just courseId1)

            frStudentId <- genRenStudent studentId1 frTeacherId id

            void $ node @CourseMembership (frStudentId, frCourseId) mempty

            postJwt route
              =<< mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId1 email1 schoolId2 Nothing
                ]

            Entity _ updatedCourse <-
              fetchJust
                $ getBy
                $ RenaissanceCoursesUniqueToTeacher frTeacherId
                $ Just courseId1
            courseSchoolId updatedCourse `shouldBe` Just frSchoolId

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

        it "creates a course if none of their courses exist" $ withGraph $ do
          Ids {..} <- genIds

          Entity frDistrictId _ <- node () mempty

          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId1

          Entity frTeacherId _ <-
            node (only $ Just frSchoolId)
              $ edit
              $ setField TeacherRenaissanceRPIdentifier
              $ Just teacherId1

          frStudentId <- genRenStudent studentId1 frTeacherId id

          postJwt route
            =<< mkStudent
              studentId1
              Nothing
              (Just "1")
              [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
              ]

          body <- statusIs 200 >> getJsonBody
          body `shouldBeMessage` ["Login Successful"]

          Entity frCourseId _ <-
            fetchJust
              $ getBy
              $ RenaissanceCoursesUniqueToTeacher frTeacherId
              $ Just courseId1

          void
            $ fetchJust
            $ getBy
            $ CourseMembershipsStudentIdCourseIdIdx frStudentId frCourseId

        it "accepts a student if any of their courses exist and are coherent"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId1

            Entity frTeacherId _ <-
              node (only $ Just frSchoolId)
                $ edit
                $ setField TeacherRenaissanceRPIdentifier
                $ Just teacherId1

            Entity frCourseId _ <-
              node (frTeacherId, Just frSchoolId)
                $ edit
                $ setField
                  CourseRenaissanceRPIdentifier
                  (Just courseId2)

            frStudentId <- genRenStudent studentId1 frTeacherId id

            void $ node @CourseMembership (frStudentId, frCourseId) mempty

            postJwt route
              =<< mkStudent
                studentId1
                Nothing
                (Just "1")
                [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
                , classFields courseId2 "Class 2" teacherId1 email1 schoolId1 Nothing
                ]

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

        it "redirects a student to the student application" $ withGraph $ do
          Ids {..} <- genIds

          Entity frDistrictId _ <- node () mempty

          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId1

          Entity frTeacherId _ <-
            node (only $ Just frSchoolId)
              $ edit
              $ setField TeacherRenaissanceRPIdentifier
              $ Just teacherId1

          Entity frCourseId _ <-
            node (frTeacherId, Just frSchoolId)
              $ edit
              $ setField
                CourseRenaissanceRPIdentifier
                (Just courseId2)

          frStudentId <- genRenStudent studentId1 frTeacherId id

          void $ node @CourseMembership (frStudentId, frCourseId) mempty

          postJwtAccept "text/html" route
            =<< mkStudent
              studentId1
              Nothing
              (Just "1")
              [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
              , classFields courseId2 "Class 2" teacherId1 email1 schoolId1 Nothing
              ]

          statusIs 303
          assertHeaderContains "Location" "student"

        it "provides a redirectUrl when a student logs out" $ withGraph $ do
          Ids {..} <- genIds

          Entity frDistrictId _ <- node () mempty

          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId1

          Entity frTeacherId _ <-
            node (only $ Just frSchoolId)
              $ edit
              $ setField TeacherRenaissanceRPIdentifier
              $ Just teacherId1

          Entity frCourseId _ <-
            node (frTeacherId, Just frSchoolId)
              $ edit
              $ setField
                CourseRenaissanceRPIdentifier
                (Just courseId2)

          frStudentId <- genRenStudent studentId1 frTeacherId id

          void $ node @CourseMembership (frStudentId, frCourseId) mempty

          postJwtAccept "text/html" route
            =<< mkStudent
              studentId1
              Nothing
              (Just "1")
              [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
              , classFields courseId2 "Class 2" teacherId1 email1 schoolId1 Nothing
              ]

          request $ do
            addJsonHeaders
            setMethod "DELETE"
            setUrl $ V2P $ StudentsP StudentsSessionsR
          body <- statusIs 200 >> getJsonBody
          body
            `shouldMatchJson` [aesonQQ|{
              redirectUrl: "https://global-lmb2.renaissance-golabs.com/educatorportal/home/logout"
            }|]

        it "exposes the keepalive url to the client" $ withGraph $ do
          Ids {..} <- genIds

          Entity frDistrictId _ <- node () mempty

          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId1

          Entity frTeacherId _ <-
            node (only $ Just frSchoolId)
              $ edit
              $ setField TeacherRenaissanceRPIdentifier
              $ Just teacherId1

          Entity frCourseId _ <-
            node (frTeacherId, Just frSchoolId)
              $ edit
              $ setField
                CourseRenaissanceRPIdentifier
                (Just courseId2)

          frStudentId <- genRenStudent studentId1 frTeacherId id

          void $ node @CourseMembership (frStudentId, frCourseId) mempty

          postJwtAccept "text/html" route
            =<< mkStudent
              studentId1
              Nothing
              (Just "1")
              [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
              , classFields courseId2 "Class 2" teacherId1 email1 schoolId1 Nothing
              ]

          request $ do
            addJsonHeaders
            setUrl $ V2P $ StudentsP SelfStudentR
          body <- statusIs 200 >> getJsonBody
          body
            `shouldMatchJson` [aesonQQ|{
              renaissanceKeepalive: {
                url: "https://global-lmb2.renaissance-golabs.com/identityservice/sso/ping",
                intervalMinutes: 15
              }
            }|]

      describe "when a different student with the same SIS Id exists" $ do
        it "creates a new student" $ withGraph $ do
          Ids {..} <- genIds

          Entity frDistrictId _ <- node () mempty

          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId1

          Entity frTeacherId _ <-
            node (only $ Just frSchoolId)
              $ edit
              $ setField TeacherRenaissanceRPIdentifier
              $ Just teacherId1

          Entity frCourseId _ <-
            node (frTeacherId, Just frSchoolId)
              $ edit
              $ setField
                CourseRenaissanceRPIdentifier
                (Just courseId1)

          Entity frStudentId _ <-
            node (only $ Just frTeacherId)
              $ edit
              $ setField StudentRenaissanceRPIdentifier Nothing
              . setField StudentFirstName "Joe"
              . setField StudentLastName "Bob"
              . setField StudentGrade G1
              . setField StudentSisId (Just sisId)
              . setField StudentDistrictId (Just frDistrictId)
          let (sisIdType, opaqueSisId) = untagIdentifier $ TaggedSisId sisId
          void
            $ node @StudentIdentifier (Only frStudentId)
            $ edit
            $ setField StudentIdentifierIdentifierType sisIdType
            . setField StudentIdentifierIdentifier opaqueSisId

          void $ node @CourseMembership (frStudentId, frCourseId) mempty

          postJwt route
            $ mkStudentNamed
              "Joe"
              "Bob"
              studentId1
              (Just sisId)
              (Just "1")
              [ classFields courseId1 "Class 1" teacherId1 email1 schoolId1 Nothing
              , classFields courseId2 "Class 2" teacherId2 email2 schoolId1 Nothing
              ]

          body <- statusIs 200 >> getJsonBody
          body `shouldBeMessage` ["Login Successful"]

          Entity unmodifiedStudentId _ <-
            fetchJust $ getBy $ RenaissanceStudentsUnique $ Just studentId1

          void
            $ fetchJust
            $ getBy
            $ CourseMembershipsStudentIdCourseIdIdx
              unmodifiedStudentId
              frCourseId
          unmodifiedStudentId `shouldNotBe` frStudentId

    describe "renaissance-sso teacher login" $ do
      describe "when parsing the teacher jwt" $ do
        it "rejects a teacher with no email" $ do
          Ids {..} <- genIds

          postJwt route =<< mkTeacher teacherId1 Nothing [schoolId1]

          body <- statusIs 401 >> getJsonBody
          body `shouldBeMessage` ["Teacher has no email address"]

        it "rejects a teacher with no schools" $ do
          Ids {..} <- genIds

          postJwt route =<< mkTeacher teacherId1 (Just email1) []

          body <- statusIs 401 >> getJsonBody
          body `shouldBeMessage` ["Teacher has no schools"]

      describe "when the teacher does not exist" $ do
        it "rejects the teacher if none of their schools exist" $ do
          Ids {..} <- genIds

          postJwt route =<< mkTeacher teacherId1 (Just email1) [schoolId1]

          body <- statusIs 401 >> getJsonBody
          body
            `shouldBeMessage` [ "Login not found: Teacher"
                              , getRPIdentifier teacherId1
                              , "Error: No schools matching IDs "
                              , getRPIdentifier schoolId1
                              ]

        it "creates the teacher if any of their schools exist" $ withGraph $ do
          Ids {..} <- genIds
          Entity frDistrictId _ <- node () mempty

          void
            $ node (only frDistrictId)
            $ edit
            $ setField SchoolRenaissanceRPIdentifier
            $ Just schoolId1

          postJwt route =<< mkTeacher teacherId1 (Just email1) [schoolId1]

          body <- statusIs 200 >> getJsonBody
          body `shouldBeMessage` ["Login Successful"]

      describe "when the teacher exists" $ do
        it "rejects a teacher associated with a school not in the system"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId1

            void
              $ node (only $ Just frSchoolId)
              $ edit
              $ setField TeacherEmail email1
              . setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            postJwt route =<< mkTeacher teacherId1 (Just email1) [schoolId2]

            body <- statusIs 401 >> getJsonBody
            body
              `shouldBeMessage` [ "Login not found: Teacher"
                                , getRPIdentifier teacherId1
                                , "Error: No schools matching IDs "
                                , getRPIdentifier schoolId2
                                ]

        it "updates a teachers email" $ withGraph $ do
          Ids {..} <- genIds

          Entity frDistrictId _ <- node () mempty

          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId2

          void
            $ node (only $ Just frSchoolId)
            $ edit
            $ setField TeacherEmail email1
            . setField TeacherRenaissanceRPIdentifier (Just teacherId1)

          postJwt route
            =<< mkTeacher teacherId1 (Just email2) [schoolId1, schoolId2]

          body <- statusIs 200 >> getJsonBody
          body `shouldBeMessage` ["Login Successful"]

          Entity _ updatedTeacher <-
            fetchJust $ getBy $ RenaissanceTeachersUnique $ Just teacherId1
          teacherEmail updatedTeacher `shouldBe` email2
          fetchNothing $ getBy $ TeachersLowerEmailUniqueIdx email1

        it "associates a previously unaffiliated teacher with a school"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId1

            void
              $ node (only Nothing)
              $ edit
              $ setField TeacherEmail email1
              . setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            postJwt route =<< mkTeacher teacherId1 (Just email1) [schoolId1]

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

            Entity _ updatedTeacher <-
              fetchJust
                $ getBy
                $ RenaissanceTeachersUnique
                $ Just
                  teacherId1
            teacherSchoolId updatedTeacher `shouldBe` Just frSchoolId
            teacherEnvironment updatedTeacher `shouldBe` Just UsInSchool

        it "can take over a non-RGP teacher with the same email"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId1

            void
              $ node (only Nothing)
              $ edit
              $ setField TeacherEmail email1
              . setField TeacherRenaissanceRPIdentifier Nothing

            postJwt route =<< mkTeacher teacherId1 (Just email1) [schoolId1]

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

            Entity _ updatedTeacher <-
              fetchJust
                $ getBy
                $ RenaissanceTeachersUnique
                $ Just
                  teacherId1
            teacherSchoolId updatedTeacher `shouldBe` Just frSchoolId

        it "moves a teacher from one school to another in the same district"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId1 _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId1

            Entity frSchoolId2 _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId2

            void
              $ node (only $ Just frSchoolId1)
              $ edit
              $ setField TeacherEmail email1
              . setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            postJwt route =<< mkTeacher teacherId1 (Just email1) [schoolId2]

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

            Entity _ updatedTeacher <-
              fetchJust
                $ getBy
                $ RenaissanceTeachersUnique
                $ Just
                  teacherId1
            teacherSchoolId updatedTeacher `shouldBe` Just frSchoolId2

        it
          "does not moves a teacher from one school to another in a different district"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId1 _ <- node () mempty
            Entity frSchoolId1 _ <-
              node (only frDistrictId1)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId1

            Entity frDistrictId2 _ <- node () mempty
            Entity frSchoolId2 _ <-
              node (only frDistrictId2)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId2

            Entity frTeacherId _ <-
              node (only $ Just frSchoolId1)
                $ edit
                $ setField TeacherEmail email1
                . setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            postJwt route =<< mkTeacher teacherId1 (Just email1) [schoolId2]
            redirectsTo
              $ mconcat
                [ "https://classroom.localhost.com"
                , "/sso-mismatched-school?sso-school-id="
                , toPathPiece frSchoolId2
                , "&freckle-school-id="
                , toPathPiece frSchoolId1
                , "&freckle-teacher-id="
                , toPathPiece frTeacherId
                , "&renaissance-teacher-id="
                , getRPIdentifier teacherId1
                ]

        it
          "accepts a teacher if they're associated with one of the specified schools"
          $ withGraph
          $ do
            Ids {..} <- genIds

            Entity frDistrictId _ <- node () mempty

            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId2

            void
              $ node (only $ Just frSchoolId)
              $ edit
              $ setField TeacherEmail email1
              . setField TeacherRenaissanceRPIdentifier (Just teacherId1)

            postJwt route
              =<< mkTeacher
                teacherId1
                (Just email1)
                [schoolId1, schoolId2]

            body <- statusIs 200 >> getJsonBody
            body `shouldBeMessage` ["Login Successful"]

        it "redirects a teacher to the classroom application" $ withGraph $ do
          Ids {..} <- genIds

          Entity frDistrictId _ <- node () mempty

          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId2

          void
            $ node (only $ Just frSchoolId)
            $ edit
            $ setField TeacherEmail email1
            . setField TeacherRenaissanceRPIdentifier (Just teacherId1)

          postJwtAccept "text/html" route
            =<< mkTeacher teacherId1 (Just email1) [schoolId1, schoolId2]

          statusIs 303
          assertHeaderContains "Location" "classroom"

        it "redirects to the JWT redirect URL" $ withGraph $ do
          Ids {..} <- genIds

          Entity frDistrictId _ <- node () mempty

          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId2

          void
            $ node (only $ Just frSchoolId)
            $ edit
            $ setField TeacherEmail email1
            . setField TeacherRenaissanceRPIdentifier (Just teacherId1)

          let
            createTpAssignmentURL =
              "https://classroom.freckle.com/courses/1261999/assignments/targeted?numQuestions=10&subType=practice"
            redirectObj = object [("redirect", createTpAssignmentURL)]

          postJwtAccept "text/html" route
            . (`merge` redirectObj)
            =<< mkTeacher teacherId1 (Just email1) [schoolId1, schoolId2]

          statusIs 303
          assertHeaderContains "Location" createTpAssignmentURL

        it "provides a redirectUrl when teacher logs out" $ withGraph $ do
          Ids {..} <- genIds

          Entity frDistrictId _ <- node () mempty

          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId2

          void
            $ node (only $ Just frSchoolId)
            $ edit
            $ setField TeacherEmail email1
            . setField TeacherRenaissanceRPIdentifier (Just teacherId1)

          postJwtAccept "text/html" route
            =<< mkTeacher teacherId1 (Just email1) [schoolId1, schoolId2]
          request $ do
            addJsonHeaders
            setMethod "DELETE"
            setUrl $ V2P $ TeachersP TeachersSessionsR
          body <- statusIs 200 >> getJsonBody
          body
            `shouldMatchJson` [aesonQQ|{
              redirectUrl: "https://global-lmb2.renaissance-golabs.com/educatorportal/home/logout"
            }|]

        it "exposes the keepalive url to the client" $ withGraph $ do
          Ids {..} <- genIds

          Entity frDistrictId _ <- node () mempty

          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId2

          void
            $ node (only $ Just frSchoolId)
            $ edit
            $ setField TeacherEmail email1
            . setField TeacherRenaissanceRPIdentifier (Just teacherId1)

          postJwtAccept "text/html" route
            =<< mkTeacher teacherId1 (Just email1) [schoolId1, schoolId2]
          request $ do
            addJsonHeaders
            setUrl $ V2P $ TeachersP $ MeP TeachersMeR
          body <- statusIs 200 >> getJsonBody
          body
            `shouldMatchJson` [aesonQQ|{
              renaissanceKeepalive: {
                url: "https://global-lmb2.renaissance-golabs.com/identityservice/sso/ping",
                intervalMinutes: 15
              }
            }|]

        it "redirects home requests to the JWT url" $ withGraph $ do
          Ids {..} <- genIds

          Entity frDistrictId _ <- node () mempty

          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId2

          void
            $ node (only $ Just frSchoolId)
            $ edit
            $ setField TeacherEmail email1
            . setField TeacherRenaissanceRPIdentifier (Just teacherId1)

          postJwtAccept "text/html" route
            =<< mkTeacher teacherId1 (Just email1) [schoolId1, schoolId2]
          request $ setUrl $ V3P $ RenaissanceP RenaissanceHomeR
          redirectsTo
            "https://global-lmb2.renaissance-golabs.com/educatorportal/home"

    describe "renaissance-sso admin login" $ do
      describe "when parsing the admin jwt" $ do
        it "rejects an admin with no email" $ do
          Ids {..} <- genIds

          postJwt route =<< mkAdmin adminId1 Nothing [schoolId1]

          body <- statusIs 401 >> getJsonBody
          body `shouldBeMessage` ["Admin has no email address"]

        it "rejects an admin with no schools" $ do
          Ids {..} <- genIds

          postJwt route =<< mkAdmin adminId1 (Just email1) []

          body <- statusIs 401 >> getJsonBody
          body `shouldBeMessage` ["Admin has no schools"]

      describe "when the admin does not exist" $ do
        it "rejects the admin if none of their schools exist" $ do
          Ids {..} <- genIds

          postJwt route =<< mkAdmin adminId1 (Just email1) [schoolId1]

          body <- statusIs 401 >> getJsonBody
          body
            `shouldBeMessage` [ "Login not found: Admin"
                              , getRPIdentifier adminId1
                              , "Error: No schools matching IDs "
                              , getRPIdentifier schoolId1
                              ]

        it "rejects the admin if none of their schools are licensed"
          $ withGraph
          $ do
            Ids {..} <- genIds
            Entity frDistrictId _ <- node () mempty

            void
              $ node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId1

            postJwt route =<< mkAdmin adminId1 (Just email1) [schoolId1]

            body <- statusIs 401 >> getJsonBody
            body
              `shouldBeMessage` [ "Login not found: Admin "
                                , getRPIdentifier adminId1
                                , "Error: No licensed schools"
                                ]

        it "creates the admin if any of their schools are licensed"
          $ withGraph
          $ do
            Ids {..} <- genIds
            Entity frDistrictId _ <- node () mempty
            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId1
            licenseSchool frSchoolId

            postJwt route =<< mkAdmin adminId1 (Just email1) [schoolId1]
            redirectsTo "https://school.localhost.com/app-selection"

      describe "when the admin exists" $ do
        it "can take over a non-RGP admin with the same email" $ withGraph $ do
          Ids {..} <- genIds
          Entity frDistrictId _ <- node () mempty
          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId2
          licenseSchool frSchoolId

          Entity frAdminId _ <-
            node @SchoolAdmin (Only BCP47.en)
              $ edit
              $ setField SchoolAdminEmail email1
              . setField SchoolAdminVerified True

          void
            $ node @SchoolAdminSchoolMembership (frAdminId, frSchoolId) mempty

          postJwt route
            =<< mkAdmin adminId1 (Just email1) [schoolId1, schoolId2]
          redirectsTo "https://school.localhost.com/app-selection"

          byEmail <-
            fmap entityKey . fetchJust $ getBy $ SchoolAdminsEmailKey email1
          byAdminId <-
            fmap entityKey
              . fetchJust
              $ getBy
              $ RenaissanceSchoolAdminsUnique
              $ Just adminId1
          byEmail `shouldBe` byAdminId

        it "associates a previously unaffiliated admin with a school"
          $ withGraph
          $ do
            Ids {..} <- genIds
            Entity frDistrictId _ <- node () mempty
            Entity frSchoolId1 _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId1

            Entity frSchoolId2 _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId2

            licenseSchool frSchoolId1

            Entity frAdminId _ <-
              node @SchoolAdmin (Only BCP47.en)
                $ edit
                $ setField SchoolAdminEmail email1
                . setField SchoolAdminVerified True
                . setField SchoolAdminRenaissanceRPIdentifier (Just adminId1)

            void
              $ node @SchoolAdminSchoolMembership
                (frAdminId, frSchoolId1)
                mempty

            postJwt route
              =<< mkAdmin adminId1 (Just email1) [schoolId1, schoolId2]
            redirectsTo "https://school.localhost.com/app-selection"

            void
              $ fetchJust
              $ getBy
              $ SchoolAdminSchoolMembershipsSchoolAdminIdSchoolIdKey
                frAdminId
                frSchoolId1
            void
              $ fetchJust
              $ getBy
              $ SchoolAdminSchoolMembershipsSchoolAdminIdSchoolIdKey
                frAdminId
                frSchoolId2

        it "provides a redirectUrl when admin logs out" $ withGraph $ do
          Ids {..} <- genIds
          Entity frDistrictId _ <- node () mempty
          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId1
          licenseSchool frSchoolId

          postJwtAccept "text/html" route
            =<< mkAdmin adminId1 (Just email1) [schoolId1]
          request $ do
            addJsonHeaders
            setMethod "DELETE"
            setUrl $ V2P $ SchoolAdminsP SchoolAdminsSessionsR
          body <- statusIs 200 >> getJsonBody
          body
            `shouldMatchJson` [aesonQQ|{
              redirectUrl: "https://global-lmb2.renaissance-golabs.com/educatorportal/home/logout"
            }|]

        it "exposes the keepalive url to the client" $ withGraph $ do
          Ids {..} <- genIds
          Entity frDistrictId _ <- node () mempty
          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier (Just schoolId1)
              . setField SchoolActiveIdp (Just RenaissanceIdp)
              . setField SchoolSyncIdp True
          licenseSchool frSchoolId

          postJwtAccept "text/html" route
            =<< mkAdmin adminId1 (Just email1) [schoolId1]
          request $ do
            setUrl $ V2P $ SchoolAdminsP $ SchoolAdminsMeP SchoolAdminsMeR
            setSchoolHeaders
          body <- statusIs 200 >> getJsonBody
          body
            `shouldMatchJson` [aesonQQ|{
              renaissanceKeepalive: {
                url: "https://global-lmb2.renaissance-golabs.com/identityservice/sso/ping",
                intervalMinutes: 15
              },
              sharedRosteringEnabled: true,
              renaissanceRPIdentifier: #{adminId1}
            }|]

        it "redirects home requests to the JWT url" $ withGraph $ do
          Ids {..} <- genIds
          Entity frDistrictId _ <- node () mempty
          Entity frSchoolId _ <-
            node (only frDistrictId)
              $ edit
              $ setField SchoolRenaissanceRPIdentifier
              $ Just schoolId1
          licenseSchool frSchoolId

          postJwtAccept "text/html" route
            =<< mkAdmin adminId1 (Just email1) [schoolId1]
          request $ do
            setUrl $ V3P $ RenaissanceP RenaissanceHomeR
            setSchoolHeaders
          redirectsTo
            "https://global-lmb2.renaissance-golabs.com/educatorportal/home"

      describe "when a teacher is also an admin" $ do
        it
          "allows an admin with courses to relogin as a teacher after being redirected to the app selection page"
          $ withGraph
          $ do
            Ids {..} <- genIds
            Entity frDistrictId _ <- node () mempty
            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier (Just schoolId1)
                . setField SchoolActiveIdp (Just RenaissanceIdp)
                . setField SchoolSyncIdp True
            licenseSchool frSchoolId

            -- Login admin
            admin <- mkAdmin adminId1 (Just email1) [schoolId1]
            postJwt route
              $ admin
              `merge` object
                [ "classes"
                    .= [ classFields
                           courseId1
                           "Class 1"
                           adminId1
                           email1
                           schoolId1
                           Nothing
                       ]
                ]
            redirectsTo "https://school.localhost.com/app-selection"

            -- Choose the classroom dashboard on the app selection page
            request $ do
              setSchoolHeaders
              setUrl $ V2P $ SchoolAdminsP SchoolAdminsTeacherSessionsR
              setMethod "POST"
              setRequestBody $ encode Empty

            body1 <- statusIs 200 >> getJsonBody
            body1
              `shouldMatchJson` [aesonQQ|{
                redirectUrl: "https://classroom.localhost.com"
              }|]

            -- Verify that we're logged in as a teacher and our RL keepalive
            -- data still remains
            request $ do
              addJsonHeaders
              setUrl $ V2P $ TeachersP $ MeP TeachersMeR
            body2 <- statusIs 200 >> getJsonBody
            body2
              `shouldMatchJson` [aesonQQ|{
                renaissanceKeepalive: {
                  url: "https://global-lmb2.renaissance-golabs.com/identityservice/sso/ping",
                  intervalMinutes: 15
                },
                renaissanceRPIdentifier: #{adminId1}
              }|]

        it
          "redirects back to school if the teacher id becomes invalid between roundtrips"
          $ withGraph
          $ do
            Ids {..} <- genIds
            Entity frDistrictId _ <- node () mempty
            Entity frSchoolId _ <-
              node (only frDistrictId)
                $ edit
                $ setField SchoolRenaissanceRPIdentifier
                $ Just schoolId1
            licenseSchool frSchoolId

            -- Login admin
            admin <- mkAdmin adminId1 (Just email1) [schoolId1]
            postJwt route
              $ admin
              `merge` object
                [ "classes"
                    .= [ classFields
                           courseId1
                           "Class 1"
                           adminId1
                           email1
                           schoolId1
                           Nothing
                       ]
                ]
            redirectsTo "https://school.localhost.com/app-selection"

            -- Move the teacher out of RL
            runSqlTx
              $ updateWhere
                [ TeacherRenaissanceRPIdentifier
                    ==. Just (retagRPIdentifier adminId1)
                ]
                [TeacherRenaissanceRPIdentifier =. Nothing]

            -- Choose the classroom dashboard on the app selection page
            request $ do
              setSchoolHeaders
              setUrl $ V2P $ SchoolAdminsP SchoolAdminsTeacherSessionsR
              setMethod "POST"
              setRequestBody $ encode Empty

            body1 <- statusIs 200 >> getJsonBody
            body1
              `shouldMatchJson` [aesonQQ|{
                redirectUrl: "https://school.localhost.com"
              }|]

mkStudent
  :: MonadIO m
  => RPIdentifier Student
  -> Maybe (SisId Student)
  -> Maybe Text
  -> [Value]
  -> m Value
mkStudent studentId sisId grade classes = do
  f <- liftIO $ QuickCheck.generate $ mkStudentNamed <$> arbitrary <*> arbitrary
  pure $ f studentId sisId grade classes

mkStudentNamed
  :: NameComponent
  -> NameComponent
  -> RPIdentifier Student
  -> Maybe (SisId Student)
  -> Maybe Text
  -> [Value]
  -> Value
mkStudentNamed firstName lastName studentId sisId grade classes =
  commonFields firstName lastName
    `merge` studentFields studentId sisId grade classes

mkTeacher
  :: MonadIO m
  => RPIdentifier Teacher
  -> Maybe EmailAddress
  -> [RPIdentifier School]
  -> m Value
mkTeacher teacherId email schoolIds = do
  common <-
    liftIO $ QuickCheck.generate $ commonFields <$> arbitrary <*> arbitrary
  pure $ common `merge` teacherFields teacherId email schoolIds

mkAdmin
  :: MonadIO m
  => RPIdentifier SchoolAdmin
  -> Maybe EmailAddress
  -> [RPIdentifier School]
  -> m Value
mkAdmin adminId email schoolIds = do
  common <-
    liftIO $ QuickCheck.generate $ commonFields <$> arbitrary <*> arbitrary
  pure $ common `merge` adminFields adminId email schoolIds

classFields
  :: RPIdentifier Course
  -> Text
  -> RPIdentifier owner
  -> EmailAddress
  -> RPIdentifier School
  -> Maybe AppTags
  -> Value
classFields classId name ownerId email schoolId apptags =
  [aesonQQ|
    { classid: #{classId}
    , classname: #{name}
    , teacherid: #{ownerId}
    , schoolid: #{schoolId}
    , apptags: #{apptags}
    , teacher:
      { given_name: "Teacher"
      , family_name: #{ownerId}
      , email: #{email}
      }
    }
  |]

studentFields
  :: RPIdentifier Student
  -> Maybe (SisId Student)
  -> Maybe Text
  -> [Value]
  -> Value
studentFields studentId sisId grade classes =
  [aesonQQ|
    { roleid: "student"
    , rid: #{studentId}
    , sisid: #{sisId}
    , grade: #{grade}
    , classes: #{classes}
    }
  |]

teacherFields
  :: RPIdentifier Teacher
  -> Maybe EmailAddress
  -> [RPIdentifier School]
  -> Value
teacherFields teacherId email schoolIds =
  [aesonQQ|
    { roleid: "teacher"
    , rid: #{teacherId}
    , email: #{email}
    , schools: #{schoolIds}
    }
  |]

adminFields
  :: RPIdentifier SchoolAdmin
  -> Maybe EmailAddress
  -> [RPIdentifier School]
  -> Value
adminFields adminId email schoolIds =
  [aesonQQ|
  { roleid: "schooladmin"
    , rid: #{adminId}
    , email: #{email}
    , schools: #{schoolIds}
    }
  |]

commonFields :: NameComponent -> NameComponent -> Value
commonFields firstName lastName =
  [aesonQQ|
    { given_name: #{firstName}
    , family_name: #{lastName}
    , jti: "d75f47a1-b9cc-4dd1-ab3c-72845aae2b8b"
    , rpid: "RP-17234488"
    , clientid: "lmb2rd2"
    , homeuri: "https://global-lmb2.renaissance-golabs.com/educatorportal/home"
    , logouturi: "https://global-lmb2.renaissance-golabs.com/educatorportal/home/logout"
    , keepaliveuri: "https://global-lmb2.renaissance-golabs.com/identityservice/sso/ping"
    , timeoutminutes: 30
    , timetorespondminutes: 5
    , keepaliveintervalminutes: 15
    }
  |]

postJwt
  :: (MonadYesodExample site m, RedirectUrl site url) => url -> Value -> m ()
postJwt = postJwtAccept "application/json"

postJwtAccept
  :: (MonadYesodExample site m, RedirectUrl site url)
  => ByteString
  -> url
  -> Value
  -> m ()
postJwtAccept accept route value = do
  now <- getCurrentTime
  let
    later = now `addDays` 1
    Right jwt = encodeJwt (ExpiresAt later) defaultJwtCredentials value
  request $ do
    setUrl route
    addRequestHeader (hAccept, accept)
    addPostParam "jwt" $ toPathPiece $ JwtParam jwt
    setMethod "POST"

shouldBeMessage :: (HasCallStack, MonadIO m) => Value -> [Text] -> m ()
shouldBeMessage body message =
  normalize body
    `shouldMatchJson` normalize [aesonQQ|{message: #{T.unwords message}}|]
 where
  normalize = \case
    Object obj -> Object $ normalize <$> obj
    Array as -> Array $ normalize <$> as
    String text -> String $ T.unwords $ T.words text
    other -> other

merge :: Value -> Value -> Value
merge (Object lhs) (Object rhs) = Object $ KeyMap.unionWithKey explode lhs rhs
 where
  explode key _ _ = error $ "Duplicate values for key " <> show key
merge _ _ = error "Can only merge objects"

fetchJust :: (MonadSqlTx db m, Show a) => db (Maybe a) -> m a
fetchJust body = do
  mEntity <- runSqlTx body
  mEntity `shouldSatisfy` isJust
  let Just entity = mEntity
  pure entity

fetchNothing
  :: (MonadSqlTx db m, Show a)
  => db (Maybe a)
  -> m ()
fetchNothing body = (`shouldSatisfy` isNothing) =<< runSqlTx body

licenseSchool
  :: GraphulaContext m '[License, RenaissanceSchoolClient] => SchoolId -> m ()
licenseSchool schoolId = do
  now <- getCurrentTime
  void
    $ node @License (only schoolId)
    $ edit
    $ setField LicenseStartsAt (now `subtractDays` 7)
    . setField LicenseExpiresAt (now `addDays` 7)
    . setField LicenseSubject SubjectMath
  void $ node @RenaissanceSchoolClient (only schoolId) mempty

data Ids = Ids
  { studentId1 :: RPIdentifier Student
  , studentId2 :: RPIdentifier Student
  , courseId1 :: RPIdentifier Course
  , courseId2 :: RPIdentifier Course
  , teacherId1 :: RPIdentifier Teacher
  , teacherId2 :: RPIdentifier Teacher
  , adminId1 :: RPIdentifier SchoolAdmin
  , adminId2 :: RPIdentifier SchoolAdmin
  , schoolId1 :: RPIdentifier School
  , schoolId2 :: RPIdentifier School
  , email1 :: EmailAddress
  , email2 :: EmailAddress
  , sisId :: SisId Student
  }

genIds :: MonadIO m => m Ids
genIds =
  liftIO
    $ QuickCheck.generate
    $ Ids
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

fetchSnapshot
  :: MonadSqlTx db m
  => TeacherId
  -> StudentId
  -> m ([CourseId], [CourseMembershipId])
fetchSnapshot teacherId studentId =
  runSqlTx $ do
    courses <-
      selectKeysList [CourseTeacherId ==. teacherId] [Asc CourseTeacherId]
    memberships <-
      selectKeysList
        [CourseMembershipStudentId ==. studentId]
        [Asc CourseMembershipStudentId]
    pure (courses, memberships)

redirectsTo :: MonadYesodExample site m => Text -> m ()
redirectsTo expected = do
  statusIs 303
  destination <- followRedirect
  destination `shouldBe` Right expected
