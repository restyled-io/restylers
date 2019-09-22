  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

php-cs-fixer

  $ run_restyler php-cs-fixer tag.php
  Loaded config default.
     1) tag.php
  
  Fixed all files in * seconds, * MB memory used (glob)
  -<?PHP
  +<?php
   $this->foo();
