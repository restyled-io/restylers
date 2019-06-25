  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

dfmt

  $ run_restyler dfmt getopt.d
  diff --git i/getopt.d w/getopt.d
  index 1f4bc4c..8444ad0 100644
  --- i/getopt.d
  +++ w/getopt.d
  @@ -1,8 +1,6 @@
  -void main(string[] args) {
  +void main(string[] args)
  +{
       bool optionOne, optionTwo, optionThree;
   
  -    getopt(args,
  -        "optionOne", &optionOne,
  -        "optionTwo", &optionTwo,
  -        "optionThree", &optionThree);
  +    getopt(args, "optionOne", &optionOne, "optionTwo", &optionTwo, "optionThree", &optionThree);
   }
