  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

elm-format

  $ run_restyler elm-format case.elm
  Processing file case.elm
  -homeDirectory = "/root/files"
  -eval boolean = case boolean of
  -    Literal bool -> bool
  -    Not b        -> not (eval b)
  -    And b b_     -> eval b && eval b_
  -    Or b b_      -> eval b   || eval b_
  +module Main exposing (..)
  +
  +
  +homeDirectory =
  +    "/root/files"
  +
  +
  +eval boolean =
  +    case boolean of
  +        Literal bool ->
  +            bool
  +
  +        Not b ->
  +            not (eval b)
  +
  +        And b b_ ->
  +            eval b && eval b_
  +
  +        Or b b_ ->
  +            eval b || eval b_
