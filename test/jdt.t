  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

jdt

  $ run_restyler jdt ./ShouldFormat1.java ./ShouldFormat2.java
  [INFO] Scanning for projects...
  [INFO] 
  [INFO] ------------------------< fooGroup:fooArtifact >------------------------
  [INFO] Building fooArtifact 0.0.0-SNAPSHOT
  [INFO] --------------------------------[ jar ]---------------------------------
  [INFO] 
  [INFO] --- formatter-maven-plugin:2.10.0:format (default-cli) @ fooArtifact ---
  [INFO] Using 'UTF-8' encoding to format source files.
  [INFO] Number of files to be formatted: 2
  [INFO] Successfully formatted:          2 file(s)
  [INFO] Fail to format:                  0 file(s)
  [INFO] Skipped:                         0 file(s)
  [INFO] Read only skipped:               0 file(s)
  [INFO] Approximate time taken:          *s (glob)
  [INFO] ------------------------------------------------------------------------
  [INFO] BUILD SUCCESS
  [INFO] ------------------------------------------------------------------------
  [INFO] Total time:  * (glob)
  [INFO] Finished at: * (glob)
  [INFO] ------------------------------------------------------------------------
  [INFO] Scanning for projects...
  [INFO] 
  [INFO] ------------------------< fooGroup:fooArtifact >------------------------
  [INFO] Building fooArtifact 0.0.0-SNAPSHOT
  [INFO] --------------------------------[ jar ]---------------------------------
  [INFO] 
  [INFO] --- maven-clean-plugin:2.5:clean (default-clean) @ fooArtifact ---
  [INFO] Deleting /code/target
  [INFO] ------------------------------------------------------------------------
  [INFO] BUILD SUCCESS
  [INFO] ------------------------------------------------------------------------
  [INFO] Total time:  * (glob)
  [INFO] Finished at: * (glob)
  [INFO] ------------------------------------------------------------------------
  diff --git i/ShouldFormat1.java w/ShouldFormat1.java
  index c3123f9..6fafba6 100644
  --- i/ShouldFormat1.java
  +++ w/ShouldFormat1.java
  @@ -1,5 +1,5 @@
   public class ShouldFormat1 {
  -int Foo(bool isBar) {
  +    int Foo(bool isBar) {
           if (isBar) {
               bar();
               return 1;
  diff --git i/ShouldFormat2.java w/ShouldFormat2.java
  index c7311a5..2df6745 100644
  --- i/ShouldFormat2.java
  +++ w/ShouldFormat2.java
  @@ -6,7 +6,8 @@ public class ShouldFormat2 {
               bar();
   
               return 1;
  -   } else
  +        } else
   
               return 0;
  -    }}
  +    }
  +}
