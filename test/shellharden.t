  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

shellharden

  $ run_restyler shellharden bash-expansion.sh
  diff --git i/bash-expansion.sh w/bash-expansion.sh
  index 9a1858f..c187da0 100644
  --- i/bash-expansion.sh
  +++ w/bash-expansion.sh
  @@ -1,3 +1,3 @@
   #!/bin/sh
   x=x
  -var=`echo $x`
  +var=`echo "$x"`
