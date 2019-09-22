  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

hindent

  $ run_restyler hindent case.hs case-2.hs
  -example = case x of Just p -> foo bar
  +example =
  +  case x of
  +    Just p -> foo bar
  -example = case x of Just p -> foo bar
  +example =
  +  case x of
  +    Just p -> foo bar
