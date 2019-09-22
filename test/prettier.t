  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

prettier for JS

  $ run_restyler prettier matrix.js
  * (glob)
  -matrix(
  -  1, 0, 0,
  -  0, 1, 0,
  -  0, 0, 1
  -)
  +matrix(1, 0, 0, 0, 1, 0, 0, 0, 1);
