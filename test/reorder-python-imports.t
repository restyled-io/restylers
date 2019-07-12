  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

reorder-python-imports

  $ run_restyler reorder-python-imports import_sort.py
  Reordering imports in import_sort.py
  diff --git i/import_sort.py w/import_sort.py
  index 48bfd1c..669f241 100644
  --- i/import_sort.py
  +++ w/import_sort.py
  @@ -1,7 +1,8 @@
  -import os, sys
  +import os
  +import sys
   from argparse import ArgumentParser
   
  -from foo import bar
   from baz import womp
  +from foo import bar
   
   from crazy import example1
