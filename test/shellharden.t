  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

shellharden

  $ run_restyler shellharden bash-expansion.sh
   #!/bin/sh
   x=x
  -var=`echo $x`
  +var=`echo "$x"`
