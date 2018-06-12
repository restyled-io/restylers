  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

php-cs-fixer

  $ run_restyler php-cs-fixer fix tag.php
  Loaded config default.
     1) tag.php
  
  Fixed all files in * seconds, * MB memory used (glob)
  diff --git i/tag.php w/tag.php
  index c7ca2e5..41a4dca 100644
  --- i/tag.php
  +++ w/tag.php
  @@ -1,2 +1,2 @@
  -<?PHP
  +<?php
   $this->foo();
