  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

dfmt

  $ run_restyler dfmt getopt.d
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
