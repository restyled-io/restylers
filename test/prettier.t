  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

prettier

  $ run_restyler prettier --write -- matrix.js
  * (glob)
  diff --git i/matrix.js w/matrix.js
  index 430121c..811d19c 100644
  --- i/matrix.js
  +++ w/matrix.js
  @@ -1,5 +1 @@
  -matrix(
  -  1, 0, 0,
  -  0, 1, 0,
  -  0, 0, 1
  -)
  +matrix(1, 0, 0, 0, 1, 0, 0, 0, 1);
