  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

hindent

  $ run_restyler hindent case.hs
  > run_restyler hindent case-2.hs
  diff --git i/case.hs w/case.hs
  index 08aded8..823b4bf 100644
  --- i/case.hs
  +++ w/case.hs
  @@ -1 +1,3 @@
  -example = case x of Just p -> foo bar
  +example =
  +  case x of
  +    Just p -> foo bar
  diff --git i/case-2.hs w/case-2.hs
  index 08aded8..823b4bf 100644
  --- i/case-2.hs
  +++ w/case-2.hs
  @@ -1 +1,3 @@
  -example = case x of Just p -> foo bar
  +example =
  +  case x of
  +    Just p -> foo bar
