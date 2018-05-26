  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

shfmt

  $ run_restyler shfmt -w -- foo-script
  diff --git i/foo-script w/foo-script
  index bcbb874..2fc3db2 100644
  --- i/foo-script
  +++ w/foo-script
  @@ -1,5 +1,4 @@
   #!/bin/sh
  -if [ 2 -eq 2 ]
  -    then
  -        echo "yup"
  -    fi
  +if [ 2 -eq 2 ]; then
  +\techo "yup" (esc)
  +fi
