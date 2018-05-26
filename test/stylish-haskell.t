  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

stylish-haskell

  $ run_restyler stylish-haskell --inplace -- pragmas.hs
  diff --git i/pragmas.hs w/pragmas.hs
  index fd4c8c8..a07dc58 100644
  --- i/pragmas.hs
  +++ w/pragmas.hs
  @@ -1,2 +1,2 @@
   {-# LANGUAGE OverloadedStrings #-}
  -{-# LANGUAGE RecordWildCards #-}
  +{-# LANGUAGE RecordWildCards   #-}
