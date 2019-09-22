  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

reorder-python-imports

  $ run_restyler reorder-python-imports import_sort.py
  Reordering imports in import_sort.py
  -import os, sys
  +import os
  +import sys
   from argparse import ArgumentParser
   
  -from foo import bar
   from baz import womp
  +from foo import bar
   
   from crazy import example1
