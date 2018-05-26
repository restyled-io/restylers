  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

brittany

  $ run_restyler brittany --write-mode inplace patterns.hs
  diff --git i/patterns.hs w/patterns.hs
  index 169b4f8..198bd69 100644
  --- i/patterns.hs
  +++ w/patterns.hs
  @@ -1,3 +1,3 @@
   func (MyLongFoo abc def) = 1
  -func (Bar a d) = 2
  -func _ = 3
  +func (Bar       a   d  ) = 2
  +func _                   = 3
