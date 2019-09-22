  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

clang-format

  $ run_restyler clang-format clang-example.c
   int formatted_code;
  -    void    unformatted_code  ;
  +void unformatted_code;
   void formatted_code_again;
