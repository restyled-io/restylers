  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

google-java-format

  $ run_restyler google-java-format --replace ./Enum.java
  diff --git i/Enum.java w/Enum.java
  index dd9dfde..756608d 100644
  --- i/Enum.java
  +++ w/Enum.java
  @@ -1,6 +1,10 @@
   private enum Answer {
  -  YES { @Override public String toString() { return "yes";
  +  YES {
  +    @Override
  +    public String toString() {
  +      return "yes";
       }
  -  }, NO,
  +  },
  +  NO,
     MAYBE
   }
