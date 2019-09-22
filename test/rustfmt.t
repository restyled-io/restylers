  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

rustfmt

  $ run_restyler rustfmt example.rs
   // Attributes should be on their own lines
   struct CRepr {
  -    x: f32,y: f32,
  +    x: f32,
  +    y: f32,
   }

PLACEHOLDER: Runs a version with fixed trailing whitespace bug. This is a known
bug that exists in all released versions. We keep this test as something to link
to when it's reported as a Restyled bug. One day, we'll update and it will be
fixed and this test should start failing -- at that time we can change it to
assert it's fixed instead of showing it broken.

  $ run_restyler rustfmt bug.rs
  error[internal]: left behind trailing whitespace
   --> /code/bug.rs:1:1:30
    |
  1 | a_macro!(name<Param1, Param2>, 
    |                               ^
    |
  
  warning: rustfmt has failed to format. See previous 1 errors.
  
  Restyler rustfmt exited 256

