  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

rustfmt

  $ run_restyler rustfmt -- example.rs
  diff --git i/example.rs w/example.rs
  index 639567b..a5d4acc 100644
  --- i/example.rs
  +++ w/example.rs
  @@ -1,4 +1,5 @@
   // Attributes should be on their own lines
   struct CRepr {
  -    x: f32,y: f32,
  +    x: f32,
  +    y: f32,
   }
