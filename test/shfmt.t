  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

shfmt

  $ run_restyler shfmt foo-script
   #!/bin/sh
  -if [ 2 -eq 2 ]
  -    then
  -        echo "yup"
  -    fi
  +if [ 2 -eq 2 ]; then
  +  echo "yup"
  +fi
