  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

black

  $ run_restyler black crazy.py
  reformatted crazy.py
  All done! \xe2\x9c\xa8 \xf0\x9f\x8d\xb0 \xe2\x9c\xa8 (esc)
  1 file reformatted.
  diff --git i/crazy.py w/crazy.py
  index 9941de7..ab602ed 100644
  --- i/crazy.py
  +++ w/crazy.py
  @@ -1,9 +1,22 @@
  -import math, sys;
  +import math, sys
  +
  +
   def example1():
       ####This is a long comment. This should be wrapped to fit within 72 characters.
  -    some_tuple=(   1,2, 3,'a'  );
  -    some_variable={'long':'Long code lines should be wrapped within 79 characters.',
  -    'other':[math.pi, 100,200,300,9876543210,'This is a long string that goes on'],
  -    'more':{'inner':'This whole logical line should be wrapped.',some_tuple:[1,
  -    20,300,40000,500000000,60000000000000000]}}
  +    some_tuple = (1, 2, 3, "a")
  +    some_variable = {
  +        "long": "Long code lines should be wrapped within 79 characters.",
  +        "other": [
  +            math.pi,
  +            100,
  +            200,
  +            300,
  +            9876543210,
  +            "This is a long string that goes on",
  +        ],
  +        "more": {
  +            "inner": "This whole logical line should be wrapped.",
  +            some_tuple: [1, 20, 300, 40000, 500000000, 60000000000000000],
  +        },
  +    }
       return (some_tuple, some_variable)
