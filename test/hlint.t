  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

hlint

  $ run_restyler hlint redundant-dollar.hs
  diff --git i/redundant-dollar.hs w/redundant-dollar.hs
  index 89d45d5..a701316 100644
  --- i/redundant-dollar.hs
  +++ w/redundant-dollar.hs
  @@ -1,2 +1,2 @@
   main :: IO ()
  -main = putStrLn $ "hello hlint"
  +main = putStrLn "hello hlint"
