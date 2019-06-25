  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

prettier for JS

  $ run_restyler prettier matrix.js
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

prettier for YAML

  $ run_restyler prettier example.yaml
  * (glob)
  diff --git i/example.yaml w/example.yaml
  index 5f47e96..015be9a 100644
  --- i/example.yaml
  +++ w/example.yaml
  @@ -1,2 +1,2 @@
   foo: bar
  -baz:   bat
  +baz: bat
