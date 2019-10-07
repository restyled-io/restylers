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

Runs a version with fixed trailing whitespace bug.

  $ run_restyler rustfmt bug.rs

