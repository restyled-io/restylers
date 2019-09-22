  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

brittany

  $ run_restyler brittany patterns.hs
   func (MyLongFoo abc def) = 1
  -func (Bar a d) = 2
  -func _ = 3
  +func (Bar       a   d  ) = 2
  +func _                   = 3
