  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

prettier for YAML

  $ run_restyler prettier example.yaml
  * (glob)
   foo: bar
  -baz:   bat
  +baz: bat
