  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

hlint

  $ run_restyler hlint redundant-dollar.hs
   main :: IO ()
  -main = putStrLn $ "hello hlint"
  +main = putStrLn "hello hlint"
