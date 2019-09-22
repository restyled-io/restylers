  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

google-java-format

  $ run_restyler google-java-format ./Enum.java
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
