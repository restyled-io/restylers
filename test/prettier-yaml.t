  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

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
