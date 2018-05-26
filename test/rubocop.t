  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

rubocop

  $ run_restyler rubocop --auto-correct -- four.rb
  Inspecting 1 file
  C
  
  Offenses:
  
  four.rb:3:1: C: [Corrected] Layout/IndentationWidth: Use 2 (not 4) spaces for indentation.
      do_something
  ^^^^
  
  1 file inspected, 1 offense detected, 1 offense corrected
  diff --git i/four.rb w/four.rb
  index bcea85b..04d7c60 100644
  --- i/four.rb
  +++ w/four.rb
  @@ -1,4 +1,4 @@
   # bad - four spaces
   def some_method
  -    do_something
  +  do_something
   end
