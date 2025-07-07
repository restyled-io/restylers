# Restylers

| Restyler | Language(s) | Version | Runs automatically? |
| -------- | ----------- | ------- | ------------------- |
| [astyle](#astyle) | C, C++, C#, Java*, Objective-C | `v3.6.2` | Yes |
| [autopep8](#autopep8) | Python | `v2.3.2` | Yes |
| [black](#black) | Python | `v25.1.0` | Yes |
| [brittany](#brittany) | Haskell | `v0.14.0.2` | No |
| [cabal-fmt](#cabal-fmt) | Haskell | `v0.1.12` | No |
| [clang-format](#clang-format) | C, C++, Java, JavaScript, Objective-C, Protobuf, C# | `v18.1.8` | Yes |
| [cmake-format](#cmake-format) | CMake | `0.6.13-1` | Yes |
| [dart-format](#dart-format) | Dart | `v3.1.0-wip` | Yes |
| [dfmt](#dfmt) | D | `v0.14.2` | Yes |
| [dhall-format](#dhall-format) | Dhall | `1.42.2` | Yes |
| [dotnet-format](#dotnet-format) | C#, VB.NET | `v5.1.250801` | No |
| [elm-format](#elm-format) | Elm | `v0.6.1-alpha-3` | Yes |
| [fantomas](#fantomas) | F# | `v3.3.0` | Yes |
| [fourmolu](#fourmolu) | Haskell | `v0.18.0.0` | No |
| [gn](#gn) | GN | `v2` | Yes |
| [gofmt](#gofmt) | Go | `go1.24.0` | Yes |
| [google-java-format](#google-java-format) | Java | `v1.9` | No |
| [hindent](#hindent) | Haskell | `v6.2.1` | No |
| [hlint](#hlint) | Haskell | `v3.5` | No |
| [isort](#isort) | Python | `v6.0.1` | Yes |
| [jdt](#jdt) | Java, JavaScript*, CSS, HTML, JSON, XML | `v2.13.0` | No |
| [jq](#jq) | JSON | `v1.6-4` | No |
| [luaformatter](#luaformatter) | Lua | `v1.3.6` | Yes |
| [nixfmt](#nixfmt) | Nix | `v0.6.0` | Yes |
| [nixpkgs-fmt](#nixpkgs-fmt) | Nix | `v1.3.0` | No |
| [npm-groovy-lint](#npm-groovy-lint) | Groovy | `v15.0.2` | Yes |
| [ocamlformat](#ocamlformat) | OCaml | `v0.26.2` | Yes |
| [ormolu](#ormolu) | Haskell | `v0.5.3.0` | No |
| [perltidy](#perltidy) | Perl | `v20250616.02` | Yes |
| [pg_format](#pg_format) | PSQL | `v5.6` | Yes |
| [php-cs-fixer](#php-cs-fixer) | PHP | `v3.80.0` | Yes |
| [prettier](#prettier) | JavaScript | `v3.6.2-3` | Yes |
| [prettier-json](#prettier-json) | JSON | `v3.6.2-3` | Yes |
| [prettier-markdown](#prettier-markdown) | Markdown | `v3.6.2-3` | Yes |
| [prettier-ruby](#prettier-ruby) | Ruby | `v3.2.2-1` | No |
| [prettier-yaml](#prettier-yaml) | Yaml | `v3.6.2-3` | Yes |
| [purty](#purty) | PureScript | `v7.0.0` | Yes |
| [pyment](#pyment) | Python | `v0.3.3` | Yes |
| [refmt](#refmt) | Reason | `v3.3.3` | Yes |
| [reorder-python-imports](#reorder-python-imports) | Python | `v3.15.0` | Yes |
| [rubocop](#rubocop) | Ruby | `v1.77.0` | No |
| [rustfmt](#rustfmt) | Rust | `v1.7.1-stable` | Yes |
| [scalafmt](#scalafmt) | Scala | `v3.7.10` | No |
| [shellcheck](#shellcheck) | POSIX sh, Bash | `v0.10.0` | Yes |
| [shellharden](#shellharden) | POSIX sh, Bash | `v4.1.1-3` | Yes |
| [shfmt](#shfmt) | POSIX sh, Bash | `v3.4.3` | Yes |
| [sqlformat](#sqlformat) | SQL, PSQL | `0.5.3` | No |
| [standardrb](#standardrb) | Ruby | `v1.50.0` | Yes |
| [stylish-haskell](#stylish-haskell) | Haskell | `v0.14.3.0` | Yes |
| [taplo](#taplo) | TOML | `0.9.3` | Yes |
| [terraform](#terraform) | Terraform | `v1.12.2` | Yes |
| [verible](#verible) | System Verilog | `v0.0-4007-g98bdb38a` | Yes |
| [whitespace](#whitespace) | * | `v0.2.0.0` | Yes |
| [yapf](#yapf) | Python | `v0.43.0-1` | Yes |

---

## astyle

Restyles _C_, _C++_, _C#_, _Java*_, _Objective-C_, runs automatically.

<details>
<summary>Documentation</summary>

- http://astyle.sourceforge.net/astyle.html

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- astyle:
    arguments: []
    command:
    - astyle
    image: restyled/restyler-astyle:v3.6.2
    include:
    - '**/*.c'
    - '**/*.cc'
    - '**/*.cpp'
    - '**/*.cxx'
    - '**/*.c++'
    - '**/*.C'
    - '**/*.cs'
    - '**/*.h'
    - '**/*.hh'
    - '**/*.hpp'
    - '**/*.hxx'
    - '**/*.h++'
    - '**/*.H'
    - '**/*.m'
    - '**/*.mm'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```c
int Foo(bool isBar)
    {
    if (isBar) {
        bar();
        return 1; }
    else
        return 0;
}

```

**After**

```c
int Foo(bool isBar)
{
    if (isBar) {
        bar();
        return 1;
    }
    else
        return 0;
}

```


**Before**

```c
/* FEOF example */
#include <stdio.h>
int main()
{
   FILE * pFile;
   char buffer [100];
   pFile = fopen ("myfile.txt" , "r");
   if (pFile == NULL) perror ("Error opening file");
   else {
     while ( ! feof (pFile) ) {
       if ( fgets (buffer , 100 , pFile) == NULL ) break;
       fputs (buffer , stdout);
     }
     fclose (pFile);
   }
   return 0;
}

```

**After**

```c
/* FEOF example */
#include <stdio.h>
int main()
{
    FILE * pFile;
    char buffer [100];
    pFile = fopen ("myfile.txt", "r");
    if (pFile == NULL) perror ("Error opening file");
    else {
        while (! feof (pFile) ) {
            if ( fgets (buffer, 100, pFile) == NULL ) break;
            fputs (buffer, stdout);
        }
        fclose (pFile);
    }
    return 0;
}

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-astyle)

## autopep8

Restyles _Python_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/hhatto/autopep8

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- autopep8:
    arguments: []
    command:
    - autopep8
    - --in-place
    image: public.ecr.aws/restyled-io/restyler-autopep8:v2.3.2
    include:
    - '**/*.py'
    interpreters:
    - python

```

</details>

<details>
<summary>Examples</summary>


**Before**

```python
import math, sys;
def example1():
    ####This is a long comment. This should be wrapped to fit within 72 characters.
    some_tuple=(   1,2, 3,'a'  );
    some_variable={'long':'Long code lines should be wrapped within 79 characters.',
    'other':[math.pi, 100,200,300,9876543210,'This is a long string that goes on'],
    'more':{'inner':'This whole logical line should be wrapped.',some_tuple:[1,
    20,300,40000,500000000,60000000000000000]}}
    return (some_tuple, some_variable)

```

**After**

```python
import math
import sys


def example1():
    # This is a long comment. This should be wrapped to fit within 72 characters.
    some_tuple = (1, 2, 3, 'a')
    some_variable = {'long': 'Long code lines should be wrapped within 79 characters.',
                     'other': [math.pi, 100, 200, 300, 9876543210, 'This is a long string that goes on'],
                     'more': {'inner': 'This whole logical line should be wrapped.', some_tuple: [1,
                                                                                                  20, 300, 40000, 500000000, 60000000000000000]}}
    return (some_tuple, some_variable)

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-autopep8)

## black

Restyles _Python_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/python/black

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- black:
    arguments: []
    command:
    - black
    image: public.ecr.aws/restyled-io/restyler-black:v25.1.0
    include:
    - '**/*.py'
    interpreters:
    - python

```

</details>

<details>
<summary>Examples</summary>


**Before**

```python
import math, sys;
def example1():
    ####This is a long comment. This should be wrapped to fit within 72 characters.
    some_tuple=(   1,2, 3,'a'  );
    some_variable={'long':'Long code lines should be wrapped within 79 characters.',
    'other':[math.pi, 100,200,300,9876543210,'This is a long string that goes on'],
    'more':{'inner':'This whole logical line should be wrapped.',some_tuple:[1,
    20,300,40000,500000000,60000000000000000]}}
    return (some_tuple, some_variable)

```

**After**

```python
import math, sys


def example1():
    ####This is a long comment. This should be wrapped to fit within 72 characters.
    some_tuple = (1, 2, 3, "a")
    some_variable = {
        "long": "Long code lines should be wrapped within 79 characters.",
        "other": [
            math.pi,
            100,
            200,
            300,
            9876543210,
            "This is a long string that goes on",
        ],
        "more": {
            "inner": "This whole logical line should be wrapped.",
            some_tuple: [1, 20, 300, 40000, 500000000, 60000000000000000],
        },
    }
    return (some_tuple, some_variable)

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-black)

## brittany

Restyles _Haskell_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://github.com/lspitzner/brittany
- https://github.com/restyled-io/restyler/wiki/Errors#brittany

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- brittany:
    arguments: []
    command:
    - brittany
    - --write-mode=inplace
    image: public.ecr.aws/restyled-io/restyler-brittany:v0.14.0.2
    include:
    - '**/*.hs'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```haskell
func (MyLongFoo abc def) = 1
func (Bar a d) = 2
func _ = 3

```

**After**

```haskell
func (MyLongFoo abc def) = 1
func (Bar       a   d  ) = 2
func _                   = 3

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-brittany)

## cabal-fmt

Restyles _Haskell_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://github.com/phadej/cabal-fmt

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- cabal-fmt:
    arguments: []
    command:
    - cabal-fmt
    - --inplace
    image: public.ecr.aws/restyled-io/restyler-cabal-fmt:v0.1.12
    include:
    - '**/*.cabal'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```haskell
cabal-version: 2.4
name: cabal-fmt
version: 0

-- An example formatter
executable cabal-fmt
    default-language: Haskell2010
    hs-source-dirs: src
    main-is: CabalFmt.hs
    -- build depends will be in
    -- a nice tabular format
    build-depends: base >=4.11 && <4.13, pretty >=1.1.3.6 && <1.2, bytestring, Cabal ^>=2.5, containers ^>=0.5.11.0 || ^>=0.6.0.1
    -- extensions will be sorted
    other-extensions:
      DeriveFunctor FlexibleContexts ExistentialQuantification OverloadedStrings
      RankNTypes

```

**After**

```haskell
cabal-version: 2.4
name:          cabal-fmt
version:       0

-- An example formatter
executable cabal-fmt
  default-language: Haskell2010
  hs-source-dirs:   src
  main-is:          CabalFmt.hs

  -- build depends will be in
  -- a nice tabular format
  build-depends:
    , base        >=4.11      && <4.13
    , bytestring
    , Cabal       ^>=2.5
    , containers  ^>=0.5.11.0 || ^>=0.6.0.1
    , pretty      >=1.1.3.6   && <1.2

  -- extensions will be sorted
  other-extensions:
    DeriveFunctor
    ExistentialQuantification
    FlexibleContexts
    OverloadedStrings
    RankNTypes

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-cabal-fmt)

## clang-format

Restyles _C_, _C++_, _Java_, _JavaScript_, _Objective-C_, _Protobuf_, _C#_, runs automatically.

<details>
<summary>Documentation</summary>

- https://clang.llvm.org/docs/ClangFormat.html

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- clang-format:
    arguments: []
    command:
    - clang-format
    - -i
    image: restyled/restyler-clang-format:v18.1.8
    include:
    - '**/*.c'
    - '**/*.cc'
    - '**/*.cpp'
    - '**/*.cxx'
    - '**/*.c++'
    - '**/*.C'
    - '**/*.cs'
    - '**/*.h'
    - '**/*.hh'
    - '**/*.hpp'
    - '**/*.hxx'
    - '**/*.h++'
    - '**/*.H'
    - '**/*.java'
    - '**/*.js'
    - '**/*.m'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```c
int formatted_code;
    void    unformatted_code  ;
void formatted_code_again;

```

**After**

```c
int formatted_code;
void unformatted_code;
void formatted_code_again;

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-clang-format)

## cmake-format

Restyles _CMake_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/cheshirekow/cmake_format

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- cmake-format:
    arguments: []
    command:
    - cmake-format
    - --in-place
    image: restyled/restyler-cmake-format:0.6.13-1
    include:
    - '**/CMakeLists.txt'
    - '**/*.cmake-format-test'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```cmake
# The following multiple newlines should be collapsed into a single newline




cmake_minimum_required(VERSION 2.8.11)
project(cmakelang_test)

# This multiline-comment should be reflowed
# into a single comment
# on one line

# This comment should remain right before the command call.
# Furthermore, the command call should be formatted
# to a single line.
add_subdirectories(foo bar baz
  foo2 bar2 baz2)

# This very long command should be wrapped
set(HEADERS very_long_header_name_a.h very_long_header_name_b.h very_long_header_name_c.h)

# This command should be split into one line per entry because it has a long argument list.
set(SOURCES source_a.cc source_b.cc source_d.cc source_e.cc source_f.cc source_g.cc source_h.cc)

# The string in this command should not be split
set_target_properties(foo bar baz PROPERTIES COMPILE_FLAGS "-std=c++11 -Wall -Wextra")

# This command has a very long argument and can't be aligned with the command
# end, so it should be moved to a new line with block indent + 1.
some_long_command_name("Some very long argument that really needs to be on the next line.")

# This situation is similar but the argument to a KWARG needs to be on a
# newline instead.
set(CMAKE_CXX_FLAGS "-std=c++11 -Wall -Wno-sign-compare -Wno-unused-parameter -xx")

set(HEADERS header_a.h header_b.h # This comment should
                                  # be preserved, moreover it should be split
                                  # across two lines.
    header_c.h header_d.h)


# This part of the comment should
# be formatted
# but...
# cmake-format: off
# This bunny should remain untouched:
# . 　 ＿　∩
# 　　ﾚﾍヽ| |
# 　　　 (・ｘ・)
# 　　 c( uu}
# cmake-format: on
#          while this part should
#          be formatted again

# This is a paragraph
#
# This is a second paragraph
#
# This is a third paragraph

# This is a comment
# that should be joined but
# TODO(josh): This todo should not be joined with the previous line.
# NOTE(josh): Also this should not be joined with the todo.

if(foo)
if(sbar)
# This comment is in-scope.
add_library(foo_bar_baz foo.cc bar.cc # this is a comment for arg2
                                      # this is more comment for arg2, it should be joined with the first.
    baz.cc) # This comment is part of add_library

other_command(some_long_argument some_long_argument) # this comment is very long and gets split across some lines

other_command(some_long_argument some_long_argument some_long_argument) # this comment is even longer and wouldn't make sense to pack at the end of the command so it gets it's own lines
endif()
endif()


# This very long command should be broken up along keyword arguments
foo(nonkwarg_a nonkwarg_b HEADERS a.h b.h c.h d.h e.h f.h SOURCES a.cc b.cc d.cc DEPENDS foo bar baz)

# This command uses a string with escaped quote chars
foo(some_arg some_arg "This is a \"string\" within a string")

# This command uses an empty string
foo(some_arg some_arg "")

# This command uses a multiline string
foo(some_arg some_arg "
    This string is on multiple lines
")

# No, I really want this to look ugly
# cmake-format: off
add_library(a b.cc
  c.cc         d.cc
           e.cc)
# cmake-format: on

```

**After**

```cmake
# The following multiple newlines should be collapsed into a single newline

cmake_minimum_required(VERSION 2.8.11)
project(cmakelang_test)

# This multiline-comment should be reflowed into a single comment on one line

# This comment should remain right before the command call. Furthermore, the
# command call should be formatted to a single line.
add_subdirectories(foo bar baz foo2 bar2 baz2)

# This very long command should be wrapped
set(HEADERS very_long_header_name_a.h very_long_header_name_b.h
            very_long_header_name_c.h)

# This command should be split into one line per entry because it has a long
# argument list.
set(SOURCES
    source_a.cc
    source_b.cc
    source_d.cc
    source_e.cc
    source_f.cc
    source_g.cc
    source_h.cc)

# The string in this command should not be split
set_target_properties(foo bar baz PROPERTIES COMPILE_FLAGS
                                             "-std=c++11 -Wall -Wextra")

# This command has a very long argument and can't be aligned with the command
# end, so it should be moved to a new line with block indent + 1.
some_long_command_name(
  "Some very long argument that really needs to be on the next line.")

# This situation is similar but the argument to a KWARG needs to be on a newline
# instead.
set(CMAKE_CXX_FLAGS
    "-std=c++11 -Wall -Wno-sign-compare -Wno-unused-parameter -xx")

set(HEADERS
    header_a.h header_b.h # This comment should be preserved, moreover it should
                          # be split across two lines.
    header_c.h header_d.h)

# This part of the comment should be formatted but...
# cmake-format: off
# This bunny should remain untouched:
# . 　 ＿　∩
# 　　ﾚﾍヽ| |
# 　　　 (・ｘ・)
# 　　 c( uu}
# cmake-format: on
# while this part should be formatted again

# This is a paragraph
#
# This is a second paragraph
#
# This is a third paragraph

# This is a comment that should be joined but
# TODO(josh): This todo should not be joined with the previous line.
# NOTE(josh): Also this should not be joined with the todo.

if(foo)
  if(sbar)
    # This comment is in-scope.
    add_library(
      foo_bar_baz
      foo.cc bar.cc # this is a comment for arg2 this is more comment for arg2,
                    # it should be joined with the first.
      baz.cc) # This comment is part of add_library

    other_command(
      some_long_argument some_long_argument) # this comment is very long and
                                             # gets split across some lines

    other_command(
      some_long_argument some_long_argument some_long_argument) # this comment
                                                                # is even longer
                                                                # and wouldn't
                                                                # make sense to
                                                                # pack at the
                                                                # end of the
                                                                # command so it
                                                                # gets it's own
                                                                # lines
  endif()
endif()

# This very long command should be broken up along keyword arguments
foo(nonkwarg_a nonkwarg_b
    HEADERS a.h b.h c.h d.h e.h f.h
    SOURCES a.cc b.cc d.cc
    DEPENDS foo
    bar baz)

# This command uses a string with escaped quote chars
foo(some_arg some_arg "This is a \"string\" within a string")

# This command uses an empty string
foo(some_arg some_arg "")

# This command uses a multiline string
foo(some_arg some_arg "
    This string is on multiple lines
")

# No, I really want this to look ugly
# cmake-format: off
add_library(a b.cc
  c.cc         d.cc
           e.cc)
# cmake-format: on

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-cmake-format)

## dart-format

Restyles _Dart_, runs automatically.

<details>
<summary>Documentation</summary>

- https://pub.dev/packages/dart_style

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- dart-format:
    arguments: []
    command:
    - dart
    - format
    image: public.ecr.aws/restyled-io/restyler-dart-format:v3.1.0-wip
    include:
    - '**/*.dart'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```dart
void example() {
  if (tag=='style'||tag=='script'&&(type==null||type == TYPE_JS
        ||type==TYPE_DART)||
    tag=='link'&&(rel=='stylesheet'||rel=='import')) {}
}

```

**After**

```dart
void example() {
  if (tag == 'style' ||
      tag == 'script' &&
          (type == null || type == TYPE_JS || type == TYPE_DART) ||
      tag == 'link' && (rel == 'stylesheet' || rel == 'import')) {}
}

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-dart-format)

## dfmt

Restyles _D_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/dlang-community/dfmt#readme

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- dfmt:
    arguments: []
    command:
    - dfmt
    - --inplace
    image: public.ecr.aws/restyled-io/restyler-dfmt:v0.14.2
    include:
    - '**/*.d'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```d
void main(string[] args) {
    bool optionOne, optionTwo, optionThree;
    getopt(args,
        "optionOne", &optionOne,
        "optionTwo", &optionTwo,
        "optionThree", &optionThree);
}

```

**After**

```d
void main(string[] args)
{
    bool optionOne, optionTwo, optionThree;
    getopt(args, "optionOne", &optionOne, "optionTwo", &optionTwo, "optionThree", &optionThree);
}

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-dfmt)

## dhall-format

Restyles _Dhall_, runs automatically.

<details>
<summary>Documentation</summary>

- https://docs.dhall-lang.org

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- dhall-format:
    arguments:
    - format
    - --inplace
    command:
    - dhall
    image: public.ecr.aws/restyled-io/restyler-dhall-format:1.42.2
    include:
    - '**/*.dhall'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```dhall
let Fruit_ = < Apple : {} | Banana : {} | Orange : {} >
in let fruiteHandler_ =
            { Apple = \(_ : {}) -> "Apple"
            , Banana = \(_ : {}) -> "Banana"
            , Orange = \(_ : {}) -> "Orange"
            }
        in let f = { Fruit = Fruit_, fruitToText = \(f : Fruit_) -> merge fruiteHandler_ f }
        in let x = { fruit1 = f.fruitToText (f.Fruit.Apple {=}), fruit2 = f.fruitToText (f.Fruit.Banana {=}) }
        in x // { fruit1 = f.fruitToText (f.Fruit.Orange {=}) }

```

**After**

```dhall
let Fruit_ = < Apple : {} | Banana : {} | Orange : {} >

in  let fruiteHandler_ =
          { Apple = \(_ : {}) -> "Apple"
          , Banana = \(_ : {}) -> "Banana"
          , Orange = \(_ : {}) -> "Orange"
          }

    in  let f =
              { Fruit = Fruit_
              , fruitToText = \(f : Fruit_) -> merge fruiteHandler_ f
              }

        in  let x =
                  { fruit1 = f.fruitToText (f.Fruit.Apple {=})
                  , fruit2 = f.fruitToText (f.Fruit.Banana {=})
                  }

            in  x // { fruit1 = f.fruitToText (f.Fruit.Orange {=}) }

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-dhall-format)

## dotnet-format

Restyles _C#_, _VB.NET_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://github.com/dotnet/format

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- dotnet-format:
    arguments: []
    command:
    - dotnet-format-files
    image: restyled/restyler-dotnet-format:v5.1.250801
    include:
    - '**/*.cs'
    - '**/*.vb'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```csharp
int formatted_code;
    void    unformatted_code  ;
void formatted_code_again;

```

**After**

```csharp
int formatted_code;
void unformatted_code;
void formatted_code_again;

```


**Before**

```csharp
int formatted_code;
    void    unformatted_code  ;
void formatted_code_again;

```

**After**

```csharp
int formatted_code;
void unformatted_code;
void formatted_code_again;

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-dotnet-format)

## elm-format

Restyles _Elm_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/avh4/elm-format

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- elm-format:
    arguments: []
    command:
    - elm-format
    - --yes
    image: public.ecr.aws/restyled-io/restyler-elm-format:v0.6.1-alpha-3
    include:
    - '**/*.elm'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```elm
homeDirectory = "/root/files"
eval boolean = case boolean of
    Literal bool -> bool
    Not b        -> not (eval b)
    And b b_     -> eval b && eval b_
    Or b b_      -> eval b   || eval b_

```

**After**

```elm
module Main exposing (eval, homeDirectory)


homeDirectory =
    "/root/files"


eval boolean =
    case boolean of
        Literal bool ->
            bool

        Not b ->
            not (eval b)

        And b b_ ->
            eval b && eval b_

        Or b b_ ->
            eval b || eval b_

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-elm-format)

## fantomas

Restyles _F#_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/fsprojects/fantomas

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- fantomas:
    arguments: []
    command:
    - fantomas
    image: restyled/restyler-fantomas:v3.3.0
    include:
    - '**/*.fs'
    - '**/*.fsi'
    - '**/*.fsx'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```fsharp
type Type
    = TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list
    with override this.ToString () =
            match this with
            | TyLam (t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
            | TyVar a -> a
            | TyCon (s, ts) -> s

```

**After**

```fsharp
type Type =
    | TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list
    override this.ToString() =
        match this with
        | TyLam(t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
        | TyVar a -> a
        | TyCon(s, ts) -> s

```


**Before**

```fsharp
let Multiple9x9 () =
    for i in 1 .. 9 do
        printf "\n";
        for j in 1 .. 9 do
            let k = i * j in
            printf "%d x %d = %2d " i j k;
        done;
    done;;
Multiple9x9 ();;

```

**After**

```fsharp
let Multiple9x9() =
    for i in 1 .. 9 do
        printf "\n"
        for j in 1 .. 9 do
            let k = i * j
            printf "%d x %d = %2d " i j k

Multiple9x9()

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-fantomas)

## fourmolu

Restyles _Haskell_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://fourmolu.github.io/
- https://github.com/fourmolu/fourmolu

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- fourmolu:
    arguments: []
    command:
    - fourmolu
    - --mode
    - inplace
    image: public.ecr.aws/restyled-io/restyler-fourmolu:v0.18.0.0
    include:
    - '**/*.hs'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```haskell
foo
 :: MonadIO m
 -> Text -> Text
 -> SqlPersistT m ()
 foo = undefined

```

**After**

```haskell
foo ::
    MonadIO m ->
    Text ->
    Text ->
    SqlPersistT
        m
        ()
        foo = undefined

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-fourmolu)

## gn

Restyles _GN_, runs automatically.

<details>
<summary>Documentation</summary>

- https://gn.googlesource.com/gn/+/master/docs/reference.md#cmd_format

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- gn:
    arguments: []
    command:
    - gn
    - format
    image: restyled/restyler-gn:v2
    include:
    - '**/*.gn'
    - '**/*.gni'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```gn
sources = ["b", "a"]

```

**After**

```gn
sources = [
  "a",
  "b",
]

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-gn)

## gofmt

Restyles _Go_, runs automatically.

<details>
<summary>Documentation</summary>

- https://golang.org/cmd/gofmt/

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- gofmt:
    arguments: []
    command:
    - gofmt
    - -w
    image: public.ecr.aws/restyled-io/restyler-gofmt:go1.24.0
    include:
    - '**/*.go'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```go
package main
          import "fmt"
// this is demo to format code
            // with gofmt command
 var a int=2;
              var b int=5;
                                  var c string= `hello world`;
       func print(){
                  fmt.Println("Value for a,b and c is : ");
                          fmt.Println(a);
                              fmt.Println((b));
                                 fmt.Println(c);
                        }

```

**After**

```go
package main

import "fmt"

// this is demo to format code
// with gofmt command
var a int = 2
var b int = 5
var c string = `hello world`

func print() {
	fmt.Println("Value for a,b and c is : ")
	fmt.Println(a)
	fmt.Println((b))
	fmt.Println(c)
}

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-gofmt)

## google-java-format

Restyles _Java_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://github.com/google/google-java-format#readme

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- google-java-format:
    arguments: []
    command:
    - google-java-format
    - --replace
    image: public.ecr.aws/restyled-io/restyler-google-java-format:v1.9
    include:
    - '**/*.java'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```java
private enum Answer {
  YES { @Override public String toString() { return "yes";
    }
  }, NO,
  MAYBE
}

```

**After**

```java
private enum Answer {
  YES {
    @Override
    public String toString() {
      return "yes";
    }
  },
  NO,
  MAYBE
}

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-google-java-format)

## hindent

Restyles _Haskell_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://github.com/commercialhaskell/hindent

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- hindent:
    arguments: []
    command:
    - hindent
    image: public.ecr.aws/restyled-io/restyler-hindent:v6.2.1
    include:
    - '**/*.hs'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```haskell
example = case x of Just p -> foo bar

```

**After**

```haskell
example =
  case x of
    Just p -> foo bar

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-hindent)

## hlint

Restyles _Haskell_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://github.com/ndmitchell/hlint#readme
- https://github.com/restyled-io/restyler/wiki/Errors#hlint

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- hlint:
    arguments: []
    command:
    - hlint
    - --refactor
    - --refactor-options=-i
    image: public.ecr.aws/restyled-io/restyler-hlint:v3.5
    include:
    - '**/*.hs'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```haskell
main :: IO ()
main = putStrLn $ "hello hlint"

```

**After**

```haskell
main :: IO ()
main = putStrLn "hello hlint"

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-hlint)

## isort

Restyles _Python_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/timothycrosley/isort/

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- isort:
    arguments: []
    command:
    - isort
    image: public.ecr.aws/restyled-io/restyler-isort:v6.0.1
    include:
    - '**/*.py'
    interpreters:
    - python

```

</details>

<details>
<summary>Examples</summary>


**Before**

```python
from my_lib import Object

import os

from my_lib import Object3

from my_lib import Object2

import sys

from third_party import lib15, lib1, lib2, lib3, lib4, lib5, lib6, lib7, lib8, lib9, lib10, lib11, lib12, lib13, lib14

import sys

from __future__ import absolute_import

from third_party import lib3

print("Hey")
print("yo")

```

**After**

```python
from __future__ import absolute_import

import os
import sys

from my_lib import Object, Object2, Object3
from third_party import (lib1, lib2, lib3, lib4, lib5, lib6, lib7, lib8, lib9,
                         lib10, lib11, lib12, lib13, lib14, lib15)

print("Hey")
print("yo")

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-isort)

## jdt

Restyles _Java_, _JavaScript*_, _CSS_, _HTML_, _JSON_, _XML_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://code.revelc.net/formatter-maven-plugin/

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- jdt:
    arguments: []
    command:
    - formatter
    image: restyled/restyler-jdt:v2.13.0
    include:
    - '**/*.java'
    - '**/*.css'
    - '**/*.html'
    - '**/*.json'
    - '**/*.xml'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```java
public class ShouldFormat1 {
int Foo(bool isBar) {
        if (isBar) {
            bar();
            return 1;
        } else
            return 0;
    }
}

```

**After**

```java
public class ShouldFormat1 {
    int Foo(bool isBar) {
        if (isBar) {
            bar();
            return 1;
        } else
            return 0;
    }
}

```


**Before**

```java
public class ShouldFormat2 {
    int Foo(bool isBar) {

        if (isBar) {

            bar();

            return 1;
   } else

            return 0;
    }}

```

**After**

```java
public class ShouldFormat2 {
    int Foo(bool isBar) {

        if (isBar) {

            bar();

            return 1;
        } else

            return 0;
    }
}

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-jdt)

## jq

Restyles _JSON_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://stedolan.github.io/jq/

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- jq:
    arguments: []
    command:
    - jq-write
    image: public.ecr.aws/restyled-io/restyler-jq:v1.6-4
    include:
    - '**/*.json'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```json
{
   "foo": "bar"
   , "baz":
 "bat" }

```

**After**

```json
{
  "foo": "bar",
  "baz": "bat"
}

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-jq)

## luaformatter

Restyles _Lua_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/Koihik/LuaFormatter

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- luaformatter:
    arguments: []
    command:
    - lua-format
    - --in-place
    image: restyled/restyler-luaformatter:v1.3.6
    include:
    - '**/*.lua'
    interpreters:
    - lua

```

</details>

<details>
<summary>Examples</summary>


**Before**

```lua
matrix = { {1, 0, 0, 0},
   {1, 1, 0,   0},
   {1, 1,  1, 0},
   {1, 1, 1, 1} }

```

**After**

```lua
matrix = {{1, 0, 0, 0}, {1, 1, 0, 0}, {1, 1, 1, 0}, {1, 1, 1, 1}}

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-luaformatter)

## nixfmt

Restyles _Nix_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/NixOS/nixfmt#readme

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- nixfmt:
    arguments: []
    command:
    - nixfmt
    image: public.ecr.aws/restyled-io/restyler-nixfmt:v0.6.0
    include:
    - '**/*.nix'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```nix
let
/*
* What you're seeing here is our nix formatter. It's quite opinionated:
*/
  sample-01 = { lib }:
{
  list = [
    elem1
    elem2
    elem3
  ] ++ lib.optionals stdenv.isDarwin [
    elem4
    elem5
  ]; # and not quite finished
}; # it will preserve your newlines

  sample-02 = { stdenv, lib }:
{
  list =
    [
      elem1
      elem2
      elem3
    ]
    ++ lib.optionals stdenv.isDarwin [ elem4 elem5 ]
    ++ lib.optionals stdenv.isLinux [ elem6 ]
    ;
};
# but it can handle all nix syntax,
# and, in fact, all of nixpkgs in <20s.
# The javascript build is quite a bit slower.
sample-03 = { stdenv, system }:
assert system == "i686-linux";
stdenv.mkDerivation { };
# these samples are all from https://github.com/nix-community/nix-fmt/tree/master/samples
sample-simple = # Some basic formatting
{
  empty_list = [ ];
  inline_list = [ 1 2 3 ];
  multiline_list = [
    1
    2
    3
    4
  ];
  inline_attrset = { x = "y"; };
  multiline_attrset = {
    a = 3;
    b = 5;
  };
  # some comment over here
  fn = x: x + x;
  relpath = ./hello;
  abspath = /hello;
  # URLs get converted from strings
  url = "https://foobar.com";
  atoms = [ true false null ];
  # Combined
  listOfAttrs = [
    {
      attr1 = 3;
      attr2 = "fff";
    }
    {
      attr1 = 5;
      attr2 = "ggg";
    }
  ];

  # long expression
  attrs = {
    attr1 = short_expr;
    attr2 =
      if true then big_expr else big_expr;
  };
}
;
in
[ sample-01 sample-02 sample-03 ]

```

**After**

```nix
let
  # * What you're seeing here is our nix formatter. It's quite opinionated:
  sample-01 =
    { lib }:
    {
      list =
        [
          elem1
          elem2
          elem3
        ]
        ++ lib.optionals stdenv.isDarwin [
          elem4
          elem5
        ]; # and not quite finished
    }; # it will preserve your newlines

  sample-02 =
    { stdenv, lib }:
    {
      list =
        [
          elem1
          elem2
          elem3
        ]
        ++ lib.optionals stdenv.isDarwin [
          elem4
          elem5
        ]
        ++ lib.optionals stdenv.isLinux [ elem6 ];
    };
  # but it can handle all nix syntax,
  # and, in fact, all of nixpkgs in <20s.
  # The javascript build is quite a bit slower.
  sample-03 =
    { stdenv, system }:
    assert system == "i686-linux";
    stdenv.mkDerivation { };
  # these samples are all from https://github.com/nix-community/nix-fmt/tree/master/samples
  sample-simple = # Some basic formatting
    {
      empty_list = [ ];
      inline_list = [
        1
        2
        3
      ];
      multiline_list = [
        1
        2
        3
        4
      ];
      inline_attrset = {
        x = "y";
      };
      multiline_attrset = {
        a = 3;
        b = 5;
      };
      # some comment over here
      fn = x: x + x;
      relpath = ./hello;
      abspath = /hello;
      # URLs get converted from strings
      url = "https://foobar.com";
      atoms = [
        true
        false
        null
      ];
      # Combined
      listOfAttrs = [
        {
          attr1 = 3;
          attr2 = "fff";
        }
        {
          attr1 = 5;
          attr2 = "ggg";
        }
      ];

      # long expression
      attrs = {
        attr1 = short_expr;
        attr2 = if true then big_expr else big_expr;
      };
    };
in
[
  sample-01
  sample-02
  sample-03
]

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-nixfmt)

## nixpkgs-fmt

Restyles _Nix_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://github.com/nix-community/nixpkgs-fmt#readme

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- nixpkgs-fmt:
    arguments: []
    command:
    - nixpkgs-fmt
    image: public.ecr.aws/restyled-io/restyler-nixpkgs-fmt:v1.3.0
    include:
    - '**/*.nix'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```nix
{foo,bar}:
        foo+bar

```

**After**

```nix
{ foo, bar }:
foo + bar

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-nixpkgs-fmt)

## npm-groovy-lint

Restyles _Groovy_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/nvuillam/npm-groovy-lint#readme

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- npm-groovy-lint:
    arguments: []
    command:
    - npm-groovy-lint
    - --fix
    image: public.ecr.aws/restyled-io/restyler-npm-groovy-lint:v15.0.2
    include:
    - '**/*.groovy'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```groovy
def variable = 1;

pipeline {
    agent any
    options{ timestamps() }
    stages{ stage('') {
            steps {
                sh """
                """
            }
        }

        stage('Deploy')
        {
            steps {
                ansiColor('xterm') {
                    sh """
                    """
                }
            } } } }

```

**After**

```groovy
def variable = 1

pipeline {
    agent any
    options { timestamps() }
    stages { stage('') {
            steps {
                sh '''
                '''
            }
    }

        stage('Deploy')
        {
            steps {
                ansiColor('xterm') {
                    sh '''
                    '''
                }
            } } } }

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-npm-groovy-lint)

## ocamlformat

Restyles _OCaml_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/ocaml-ppx/ocamlformat

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- ocamlformat:
    arguments: []
    command:
    - ocamlformat
    - --inplace
    image: restyled/restyler-ocamlformat:v0.26.2
    include:
    - '**/*.ml'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```ocaml
let sum_of_squares num =
  num + 1
  |> List.range 0 |> List.map ~f:square
  |> List.fold_left ~init:0 ~f:( + )

```

**After**

```ocaml
let sum_of_squares num =
  num + 1 |> List.range 0 |> List.map ~f:square
  |> List.fold_left ~init:0 ~f:( + )

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-ocamlformat)

## ormolu

Restyles _Haskell_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://github.com/tweag/ormolu#readme

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- ormolu:
    arguments: []
    command:
    - ormolu
    - --mode
    - inplace
    image: public.ecr.aws/restyled-io/restyler-ormolu:v0.5.3.0
    include:
    - '**/*.hs'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```haskell
foo
 :: MonadIO m
 -> Text -> Text
 -> SqlPersistT m ()
 foo = undefined

```

**After**

```haskell
foo ::
  MonadIO m ->
  Text ->
  Text ->
  SqlPersistT
    m
    ()
    foo = undefined

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-ormolu)

## perltidy

Restyles _Perl_, runs automatically.

<details>
<summary>Documentation</summary>

- https://perltidy.sourceforge.net/

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- perltidy:
    arguments: []
    command:
    - perltidy
    - -st
    image: public.ecr.aws/restyled-io/restyler-perltidy:v20250616.02
    include:
    - '**/*.pl'
    - '**/*.pm'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```perl
my $lines  = 0; # checksum: #lines
my $bytes  = 0; # checksum: #bytes
my $sum  = 0; # checksum: system V sum
my $patchdata = 0; # saw patch data
my $pos  = 0; # start of patch data
my $endkit = 0; # saw end of kit
my $fail  = 0; # failed

```

**After**

```perl
my $lines     = 0;    # checksum: #lines
my $bytes     = 0;    # checksum: #bytes
my $sum       = 0;    # checksum: system V sum
my $patchdata = 0;    # saw patch data
my $pos       = 0;    # start of patch data
my $endkit    = 0;    # saw end of kit
my $fail      = 0;    # failed

```


**Before**

```perl
$_= <<'EOL';
   $url = new URI::URL "http://www/";   die if $url eq "xXx";
EOL
LOOP:{print(" digits"),redo LOOP if/\G\d+\b[,.;]?\s*/gc;print(" lowercase"),
redo LOOP if/\G[a-z]+\b[,.;]?\s*/gc;print(" UPPERCASE"),redo LOOP 
if/\G[A-Z]+\b[,.;]?\s*/gc;print(" Capitalized"),
redo LOOP if/\G[A-Z][a-z]+\b[,.;]?\s*/gc;
print(" MiXeD"),redo LOOP if/\G[A-Za-z]+\b[,.;]?\s*/gc;print(
" alphanumeric"),redo LOOP if/\G[A-Za-z0-9]+\b[,.;]?\s*/gc;print(" line-noise"
),redo LOOP if/\G[^A-Za-z0-9]+/gc;print". That's all!\n";}

```

**After**

```perl
$_ = <<'EOL';
   $url = new URI::URL "http://www/";   die if $url eq "xXx";
EOL
LOOP: {
    print(" digits"),    redo LOOP if /\G\d+\b[,.;]?\s*/gc;
    print(" lowercase"), redo LOOP if /\G[a-z]+\b[,.;]?\s*/gc;
    print(" UPPERCASE"), redo LOOP
      if /\G[A-Z]+\b[,.;]?\s*/gc;
    print(" Capitalized"),  redo LOOP if /\G[A-Z][a-z]+\b[,.;]?\s*/gc;
    print(" MiXeD"),        redo LOOP if /\G[A-Za-z]+\b[,.;]?\s*/gc;
    print(" alphanumeric"), redo LOOP if /\G[A-Za-z0-9]+\b[,.;]?\s*/gc;
    print(" line-noise"),   redo LOOP if /\G[^A-Za-z0-9]+/gc;
    print ". That's all!\n";
}

```


**Before**

```perl
%unitscale=("in",72,"pt",72.27/72,"pc",12,"mm",72/25.4,"cm",72/2.54,
"\\hsize",100,"\\vsize",100,"\\textwidth",100,"\\textheight",100,
"\\pagewidth",100,"\\linewidth",100);

```

**After**

```perl
%unitscale = (
    "in",           72,        "pt",          72.27 / 72,
    "pc",           12,        "mm",          72 / 25.4,
    "cm",           72 / 2.54, "\\hsize",     100,
    "\\vsize",      100,       "\\textwidth", 100,
    "\\textheight", 100,       "\\pagewidth", 100,
    "\\linewidth",  100
);

```


**Before**

```perl
my $a_box = [ [ $a11, $a12, $a13, $a14, $a15, $a16 ], 
[ $a21, $a22, $a23, $a24, $a25, $a26 ], [ $a31, $a32, $a33, $a34, $a35, $a36 ],
[ $a41, $a42, $a43, $a44, $a45, $a46 ], [ $a51, $a52, $a53, $a54, $a55, $a56 ],
	[ $a61, $a62, $a63, $a64, $a65, $a66 ], ];

```

**After**

```perl
my $a_box = [
    [ $a11, $a12, $a13, $a14, $a15, $a16 ],
    [ $a21, $a22, $a23, $a24, $a25, $a26 ],
    [ $a31, $a32, $a33, $a34, $a35, $a36 ],
    [ $a41, $a42, $a43, $a44, $a45, $a46 ],
    [ $a51, $a52, $a53, $a54, $a55, $a56 ],
    [ $a61, $a62, $a63, $a64, $a65, $a66 ],
];

```


**Before**

```perl
%TV=(flintstones=>{series=>"flintstones",nights=>[qw(monday thursday friday)],
members=>[{name=>"fred",role=>"lead",age=>36,},{name=>"wilma",role=>"wife",
age=>31,},{name=>"pebbles",role=>"kid",age=>4,},],},jetsons=>{series=>"jetsons",
nights=>[qw(wednesday saturday)],members=>[{name=>"george",role=>"lead",age=>41,
},{name=>"jane",role=>"wife",age=>39,},{name=>"elroy",role=>"kid",age=>9,},],},
simpsons=>{series=>"simpsons",nights=>[qw(monday)],members=>[{name=>"homer",
role=>"lead",age=>34,},{name=>"marge",role=>"wife",age=>37,},{name=>"bart",
role=>"kid",age=>11,},],},);

```

**After**

```perl
%TV = (
    flintstones => {
        series  => "flintstones",
        nights  => [qw(monday thursday friday)],
        members => [
            { name => "fred", role => "lead", age => 36, },
            {
                name => "wilma",
                role => "wife",
                age  => 31,
            },
            { name => "pebbles", role => "kid", age => 4, },
        ],
    },
    jetsons => {
        series  => "jetsons",
        nights  => [qw(wednesday saturday)],
        members => [
            {
                name => "george",
                role => "lead",
                age  => 41,
            },
            { name => "jane",  role => "wife", age => 39, },
            { name => "elroy", role => "kid",  age => 9, },
        ],
    },
    simpsons => {
        series  => "simpsons",
        nights  => [qw(monday)],
        members => [
            {
                name => "homer",
                role => "lead",
                age  => 34,
            },
            { name => "marge", role => "wife", age => 37, },
            {
                name => "bart",
                role => "kid",
                age  => 11,
            },
        ],
    },
);

```


**Before**

```perl
{
  L9140:
 if ($msccom::obj==$msccom::food) {
 goto L8142;
 }
 if ($msccom::obj==$msccom::bird||$msccom::obj==$msccom::snake||$msccom::obj==$msccom::clam||$msccom::obj==$msccom::oyster||$msccom::obj==$msccom::dwarf||$msccom::obj==$msccom::dragon||$msccom::obj==$msccom::troll||$msccom::obj==$msccom::bear) {
 $msccom::spk=71;
 }
 goto L2011;
 # 
  #  DRINK.  IF NO OBJECT, ASSUME WATER AND LOOK FOR IT HERE.  IF WATER IS 
  #  THE BOTTLE, DRINK THAT, ELSE MUST BE AT A WATER LOC, SO DRINK STREAM. 
  # 
  L9150:
 if ($msccom::obj==0&&$liqloc->($placom::loc)!=$msccom::water&&($liq->(0)!=$msccom::water||!$here->($msccom::bottle))) {
 goto L8000;
 }
 if ($msccom::obj!=0&&$msccom::obj!=$msccom::water) {
 $msccom::spk=110;
 }
 if ($msccom::spk==110||$liq->(0)!=$msccom::water||!$here->($msccom::bottle)) {
 goto L2011;
 }
 $placom::prop->($msccom::bottle)=1;
 $placom::place->($msccom::water)=0;
 $msccom::spk=74;
 goto L2011;
 # 
  #  RUB.  YIELDS VARIOUS SNIDE REMARKS. 
  # 
  L9160:
 if ($msccom::obj!=$placom::lamp) {
 $msccom::spk=76;
 }
 goto L2011;
 # 
  #  THROW.  SAME AS DISCARD UNLESS AXE.  THEN SAME AS ATTACK EXCEPT IGNOR 
  #  AND IF DWARF IS PRESENT THEN ONE MIGHT BE KILLED.  (ONLY WAY TO DO SO 
  #  AXE ALSO SPECIAL FOR DRAGON, BEAR, AND TROLL.  TREASURES SPECIAL FOR 
  # 
  L9170:
 if ($toting->($msccom::rod2)&&$msccom::obj==$msccom::rod&&!$toting->($msccom::rod)) {
 $msccom::obj=$msccom::rod2;
 }
 }

```

**After**

```perl
{
  L9140:
    if ( $msccom::obj == $msccom::food ) {
        goto L8142;
    }
    if (   $msccom::obj == $msccom::bird
        || $msccom::obj == $msccom::snake
        || $msccom::obj == $msccom::clam
        || $msccom::obj == $msccom::oyster
        || $msccom::obj == $msccom::dwarf
        || $msccom::obj == $msccom::dragon
        || $msccom::obj == $msccom::troll
        || $msccom::obj == $msccom::bear )
    {
        $msccom::spk = 71;
    }
    goto L2011;
    #
    #  DRINK.  IF NO OBJECT, ASSUME WATER AND LOOK FOR IT HERE.  IF WATER IS
    #  THE BOTTLE, DRINK THAT, ELSE MUST BE AT A WATER LOC, SO DRINK STREAM.
    #
  L9150:
    if (   $msccom::obj == 0
        && $liqloc->($placom::loc) != $msccom::water
        && ( $liq->(0) != $msccom::water || !$here->($msccom::bottle) ) )
    {
        goto L8000;
    }
    if ( $msccom::obj != 0 && $msccom::obj != $msccom::water ) {
        $msccom::spk = 110;
    }
    if (   $msccom::spk == 110
        || $liq->(0) != $msccom::water
        || !$here->($msccom::bottle) )
    {
        goto L2011;
    }
    $placom::prop->($msccom::bottle) = 1;
    $placom::place->($msccom::water) = 0;
    $msccom::spk                     = 74;
    goto L2011;
    #
    #  RUB.  YIELDS VARIOUS SNIDE REMARKS.
    #
  L9160:
    if ( $msccom::obj != $placom::lamp ) {
        $msccom::spk = 76;
    }
    goto L2011;
    #
    #  THROW.  SAME AS DISCARD UNLESS AXE.  THEN SAME AS ATTACK EXCEPT IGNOR
    #  AND IF DWARF IS PRESENT THEN ONE MIGHT BE KILLED.  (ONLY WAY TO DO SO
    #  AXE ALSO SPECIAL FOR DRAGON, BEAR, AND TROLL.  TREASURES SPECIAL FOR
    #
  L9170:
    if (   $toting->($msccom::rod2)
        && $msccom::obj == $msccom::rod
        && !$toting->($msccom::rod) )
    {
        $msccom::obj = $msccom::rod2;
    }
}

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-perltidy)

## pg_format

Restyles _PSQL_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/darold/pgFormatter#readme

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- pg_format:
    arguments: []
    command:
    - pg_format
    - --inplace
    image: public.ecr.aws/restyled-io/restyler-pg_format:v5.6
    include:
    - '**/*.sql'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```psql
SELECT * from
students
WHERE students.age > 10;

```

**After**

```psql
SELECT
    *
FROM
    students
WHERE
    students.age > 10;


```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-pg_format)

## php-cs-fixer

Restyles _PHP_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/FriendsOfPHP/PHP-CS-Fixer

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- php-cs-fixer:
    arguments: []
    command:
    - php-cs-fixer
    - fix
    image: public.ecr.aws/restyled-io/restyler-php-cs-fixer:v3.80.0
    include:
    - '**/*.php'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```php
<?PHP
$this->foo();

```

**After**

```php
<?php

$this->foo();

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-php-cs-fixer)

## prettier

Restyles _JavaScript_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/restyled-io/restyler/wiki/Errors#prettier
- https://prettier.io/docs/en/

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- prettier:
    arguments: []
    command:
    - prettier
    - --write
    image: public.ecr.aws/restyled-io/restyler-prettier:v3.6.2-3
    include:
    - '**/*.js'
    - '**/*.jsx'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```javascript
matrix(
  1, 0, 0,
  0, 1, 0,
  0, 0, 1
)

```

**After**

```javascript
matrix(1, 0, 0, 0, 1, 0, 0, 0, 1);

```


**Before**

```javascript
matrix(
  1, 0, 0,
  0, 1, 0,
  0, 0, 1
)

```

**After**

```javascript
matrix(1, 0, 0, 0, 1, 0, 0, 0, 1);

```


**Before**

```javascript
import c from 'moduleC';
import a from 'moduleA';
import b from 'moduleB';

```

**After**

```javascript
import a from "moduleA";
import b from "moduleB";
import c from "moduleC";

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-prettier)

## prettier-json

Restyles _JSON_, runs automatically.

<details>
<summary>Documentation</summary>

- https://prettier.io/docs/en/options.html#parser

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- prettier-json:
    arguments: []
    command:
    - prettier
    - --write
    image: public.ecr.aws/restyled-io/restyler-prettier:v3.6.2-3
    include:
    - '**/*.json'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```json
{
   "foo": "bar"
   , "baz":
 "bat" }

```

**After**

```json
{
  "foo": "bar",
  "baz": "bat"
}

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-prettier-json)

## prettier-markdown

Restyles _Markdown_, runs automatically.

<details>
<summary>Documentation</summary>

- https://prettier.io/blog/2017/11/07/1.8.0.html
- https://prettier.io/docs/en/index.html

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- prettier-markdown:
    arguments: []
    command:
    - prettier
    - --write
    image: public.ecr.aws/restyled-io/restyler-prettier:v3.6.2-3
    include:
    - '**/*.md'
    - '**/*.markdown'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```markdown
Voilà! In view, a humble vaudevillian veteran cast vicariously as both victim and villain by the vicissitudes of Fate. This visage, no mere veneer of vanity, is a vestige of the vox populi, now vacant, vanished. However, this valourous visitation of a bygone vexation stands vivified and has vowed to vanquish these venal and virulent vermin vanguarding vice and vouchsafing the violently vicious and voracious violation of volition! The only verdict is vengeance; a vendetta held as a votive, not in vain, for the value and veracity of such shall one day vindicate the vigilant and the virtuous. Verily, this vichyssoise of verbiage veers most verbose, so let me simply add that it's my very good honour to meet you and you may call me V.

```

**After**

```markdown
Voilà! In view, a humble vaudevillian veteran cast vicariously as both victim
and villain by the vicissitudes of Fate. This visage, no mere veneer of vanity,
is a vestige of the vox populi, now vacant, vanished. However, this valourous
visitation of a bygone vexation stands vivified and has vowed to vanquish these
venal and virulent vermin vanguarding vice and vouchsafing the violently vicious
and voracious violation of volition! The only verdict is vengeance; a vendetta
held as a votive, not in vain, for the value and veracity of such shall one day
vindicate the vigilant and the virtuous. Verily, this vichyssoise of verbiage
veers most verbose, so let me simply add that it's my very good honour to meet
you and you may call me V.

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-prettier-markdown)

## prettier-ruby

Restyles _Ruby_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://prettier.io/docs/en/
- https://github.com/prettier/plugin-ruby

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- prettier-ruby:
    arguments: []
    command:
    - rbprettier
    - --write
    image: restyled/restyler-prettier-ruby:v3.2.2-1
    include:
    - '**/*.rb'
    interpreters:
    - ruby

```

</details>

<details>
<summary>Examples</summary>


**Before**

```ruby
       d=[30644250780,9003106878,
   30636278846,66641217692,4501790980,
671_24_603036,131_61973916,66_606629_920,
  30642677916,30643069058];a,s=[],$*[0]
     s.each_byte{|b|a<<("%036b"%d[b.
        chr.to_i]).scan(/\d{6}/)}
         a.transpose.each{ |a|
           a.join.each_byte{\
            |i|print i==49?\
              ($*[1]||"#")\
                :32.chr}
                  puts
                   }

```

**After**

```ruby
d = [
  30_644_250_780,
  9_003_106_878,
  30_636_278_846,
  66_641_217_692,
  4_501_790_980,
  671_24_603036,
  131_61973916,
  66_606629_920,
  30_642_677_916,
  30_643_069_058
]
a, s = [], $*[0]
s.each_byte { |b| a << ("%036b" % d[b.chr.to_i]).scan(/\d{6}/) }
a.transpose.each do |a|
  a.join.each_byte { |i| print i == 49 ? ($*[1] || "#") : 32.chr }
  puts
end

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-prettier-ruby)

## prettier-yaml

Restyles _Yaml_, runs automatically.

<details>
<summary>Documentation</summary>

- https://prettier.io/docs/en/
- https://prettier.io/blog/2018/07/29/1.14.0.html

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- prettier-yaml:
    arguments: []
    command:
    - prettier
    - --write
    image: public.ecr.aws/restyled-io/restyler-prettier:v3.6.2-3
    include:
    - '**/*.yml'
    - '**/*.yaml'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```yaml
foo: bar
baz:   bat

```

**After**

```yaml
foo: bar
baz: bat

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-prettier-yaml)

## purty

Restyles _PureScript_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/joneshf/purty#readme

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- purty:
    arguments: []
    command:
    - purty
    - --write
    image: public.ecr.aws/restyled-io/restyler-purty:v7.0.0
    include:
    - '**/*.purs'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```purescript
module Ado where

foo = ado
  let w = 0
  x <- pure 1
  y <- do
    pure 2
    pure 2
  z <- do pure 3
  in w + x + y + z

bar = ado in 12

```

**After**

```purescript
module Ado where

foo = ado
  let
    w = 0
  x <- pure 1
  y <- do
    pure 2
    pure 2
  z <- do pure 3
  in w + x + y + z

bar = ado in 12

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-purty)

## pyment

Restyles _Python_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/dadadel/pyment

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- pyment:
    arguments: []
    command:
    - pyment
    - -w
    image: restyled/restyler-pyment:v0.3.3
    include:
    - '**/*.py'
    interpreters:
    - python

```

</details>

<details>
<summary>Examples</summary>


**Before**

```python
def func(param1=True, param2: str = 'default val'):
  '''Description of func with docstring groups style.

  Params:
      param1 - descr of param1 that has True for default value.
      param2 - descr of param2

  Returns:
      some value

  Raises:
      keyError: raises key exception
      TypeError: raises type exception

  '''
  pass

class A:
    def method(self, param1, param2=None) -> int:
        pass

```

**After**

```python
def func(param1=True, param2: str = 'default val'):
    """Description of func with docstring groups style.

    :param param1: descr of param1 that has True for default value
    :param param2: descr of param2
    :param param2: str:  (Default value = 'default val')
    :returns: some value
    :raises keyError: raises key exception
    :raises TypeError: raises type exception

    """
  pass

class A:
    """ """
    def method(self, param1, param2=None) -> int:
        """

        :param param1: 
        :param param2:  (Default value = None)

        """
        pass

```


**Before**

```python
def func(param1=True, param2: str = 'default val'):
  '''Description of func with docstring groups style.

  Params:
      param1 - descr of param1 that has True for default value.
      param2 - descr of param2

  Returns:
      some value

  Raises:
      keyError: raises key exception
      TypeError: raises type exception

  '''
  pass

class A:
    def method(self, param1, param2=None) -> int:
        pass

```

**After**

```python
def func(param1=True, param2: str = 'default val'):
    """Description of func with docstring groups style.

    :param param1: descr of param1 that has True for default value
    :param param2: descr of param2
    :param param2: str:  (Default value = 'default val')
    :returns: some value
    :raises keyError: raises key exception
    :raises TypeError: raises type exception

    """
  pass

class A:
    """ """
    def method(self, param1, param2=None) -> int:
        """

        :param param1: 
        :param param2:  (Default value = None)

        """
        pass

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-pyment)

## refmt

Restyles _Reason_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/reasonml/reason-cli#reason-cli

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- refmt:
    arguments: []
    command:
    - refmt
    - --in-place
    image: public.ecr.aws/restyled-io/restyler-refmt:v3.3.3
    include:
    - '**/*.re'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```reason
type schoolPerson = Teacher
        | Director | Student(string);

let greeting = person =>
  switch (person){
      | Teacher => "Hey Professor!"
      | Director => "Hello Director."
      | Student("Richard") => "Still here Ricky?"
      | Student(anyOtherName) => "Hey, " ++ anyOtherName
          ++ "."
  };

```

**After**

```reason
type schoolPerson =
  | Teacher
  | Director
  | Student(string);

let greeting = person =>
  switch (person) {
  | Teacher => "Hey Professor!"
  | Director => "Hello Director."
  | Student("Richard") => "Still here Ricky?"
  | Student(anyOtherName) => "Hey, " ++ anyOtherName ++ "."
  };

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-refmt)

## reorder-python-imports

Restyles _Python_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/asottile/reorder_python_imports

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- reorder-python-imports:
    arguments: []
    command:
    - reorder-python-imports
    - --exit-zero-even-if-changed
    image: public.ecr.aws/restyled-io/restyler-reorder-python-imports:v3.15.0
    include:
    - '**/*.py'
    interpreters:
    - python

```

</details>

<details>
<summary>Examples</summary>


**Before**

```python
import os, sys
from argparse import ArgumentParser

from foo import bar
from baz import womp

from crazy import example1

```

**After**

```python
import os
import sys
from argparse import ArgumentParser

from baz import womp
from crazy import example1
from foo import bar

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-reorder-python-imports)

## rubocop

Restyles _Ruby_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://rubocop.readthedocs.io/en/latest/

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- rubocop:
    arguments: []
    command:
    - rubocop
    - --auto-correct
    - --fail-level
    - fatal
    image: public.ecr.aws/restyled-io/restyler-rubocop:v1.77.0
    include:
    - '**/*.rb'
    interpreters:
    - ruby

```

</details>

<details>
<summary>Examples</summary>


**Before**

```ruby
def some_method
    do_something
end

```

**After**

```ruby
def some_method
  do_something
end

```


**Before**

```ruby
case foo
when *[1, 2, 3, 4]
  bar
when 5
  baz
end

```

**After**

```ruby
case foo
when 1, 2, 3, 4
  bar
when 5
  baz
end

```


**Before**

```ruby
class User < ApplicationRecord
  MissingAdminAccount = Class.new(RuntimeError)

  include Wisper::Publisher

  # Include default devise modules. Others available are:
  # :confirmable, :lockable, :timeoutable and :omniauthable
  devise :database_authenticatable, :registerable, :confirmable,
         :recoverable, :rememberable, :trackable, :validatable
  has_many :radars, foreign_key: "owner_id", dependent: :destroy
  has_many :created_topics, foreign_key: "creator_id", class_name: "Topic", dependent: :nullify
  has_many :blips, through: :radars

  attr_accessor :login

  validates :name, presence: true
  validates :username,
            presence: true,
            uniqueness: {
              case_sensitive: false
            }

  # For Devise
  def self.find_for_database_authentication(warden_conditions)
    conditions = warden_conditions.dup
    if (login = conditions.delete(:login))
      where(conditions).find_by(["lower(username) = :value OR lower(email) = :value", { value: login.downcase }])
    else
      # :nocov:
      find_by(conditions)
      # :nocov:
    end
  end

  # Override Devise to use ActiveJob
  # https://github.com/plataformatec/devise#activejob-integration
  def send_devise_notification(notification, *args)
    devise_mailer.send(notification, self, *args).deliver_later
  end

  def find_radar(uuid:)
    radars.find_by!(uuid: uuid)
  end

  def new_radar(params)
    radars.new(params)
  end

  def add_radar(params)
    new_radar(params).tap(&:save!)
  end

  def first_sign_in?
    sign_in_count == 1
  end

  after_create do |user|
    publish(:user_created, user)
  end

  def self.admin
    admin = find_by(admin: true)
    admin || raise(MissingAdminAccount)
  end
end

```

**After**

```ruby
class User < ApplicationRecord
  MissingAdminAccount = Class.new(RuntimeError)

  include Wisper::Publisher

  # Include default devise modules. Others available are:
  # :confirmable, :lockable, :timeoutable and :omniauthable
  devise :database_authenticatable, :registerable, :confirmable,
         :recoverable, :rememberable, :trackable, :validatable
  has_many :radars, foreign_key: 'owner_id', dependent: :destroy
  has_many :created_topics, foreign_key: 'creator_id', class_name: 'Topic', dependent: :nullify
  has_many :blips, through: :radars

  attr_accessor :login

  validates :name, presence: true
  validates :username,
            presence: true,
            uniqueness: {
              case_sensitive: false
            }

  # For Devise
  def self.find_for_database_authentication(warden_conditions)
    conditions = warden_conditions.dup
    if (login = conditions.delete(:login))
      where(conditions).find_by(['lower(username) = :value OR lower(email) = :value', { value: login.downcase }])
    else
      # :nocov:
      find_by(conditions)
      # :nocov:
    end
  end

  # Override Devise to use ActiveJob
  # https://github.com/plataformatec/devise#activejob-integration
  def send_devise_notification(notification, *args)
    devise_mailer.send(notification, self, *args).deliver_later
  end

  def find_radar(uuid:)
    radars.find_by!(uuid: uuid)
  end

  def new_radar(params)
    radars.new(params)
  end

  def add_radar(params)
    new_radar(params).tap(&:save!)
  end

  def first_sign_in?
    sign_in_count == 1
  end

  after_create do |user|
    publish(:user_created, user)
  end

  def self.admin
    admin = find_by(admin: true)
    admin || raise(MissingAdminAccount)
  end
end

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-rubocop)

## rustfmt

Restyles _Rust_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/rust-lang-nursery/rustfmt#readme
- https://github.com/restyled-io/restyler/wiki/Errors#prettier

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- rustfmt:
    arguments: []
    command:
    - rustfmt
    image: restyled/restyler-rustfmt:v1.7.1-stable
    include:
    - '**/*.rs'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```rust
// Attributes should be on their own lines
struct CRepr {
    x: f32,y: f32,
}

```

**After**

```rust
// Attributes should be on their own lines
struct CRepr {
    x: f32,
    y: f32,
}

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-rustfmt)

## scalafmt

Restyles _Scala_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://scalameta.org/scalafmt/
- https://github.com/restyled-io/restyler/wiki/Errors#scalafmt

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- scalafmt:
    arguments: []
    command:
    - scalafmt
    - --non-interactive
    image: restyled/restyler-scalafmt:v3.7.10
    include:
    - '**/*.scala'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```scala
object a {
  def c(b: List[Int]): List[Int] =
    for {
      a <- b
      if ((a))
    } yield a
}

```

**After**

```scala
object a {
  def c(b: List[Int]): List[Int] =
    for {
      a <- b
      if a
    } yield a
}

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-scalafmt)

## shellcheck

Restyles _POSIX sh_, _Bash_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/koalaman/shellcheck/wiki

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- shellcheck:
    arguments: []
    command:
    - shellcheck-fix
    image: public.ecr.aws/restyled-io/restyler-shellcheck:v0.10.0
    include:
    - '**/*.sh'
    - '**/*.bash'
    interpreters:
    - sh
    - bash

```

</details>

<details>
<summary>Examples</summary>


**Before**

```sh
echo $foo

```

**After**

```sh
echo "$foo"

```


**Before**

```sh
## Example of a broken script.
for f in $(ls *.m3u)
do
  grep -qi hq.*mp3 $f \
    && echo -e 'Playlist $f contains a HQ file in mp3 format'
done

```

**After**

```sh
## Example of a broken script.
for f in $(ls *.m3u)
do
  grep -qi hq.*mp3 "$f" \
    && echo -e 'Playlist $f contains a HQ file in mp3 format'
done

```


**Before**

```sh
if (( $n > 1 )); then
  echo yeah
fi

```

**After**

```sh
if (( $n > 1 )); then
  echo yeah
fi

```


**Before**

```sh
echo "$foo"

```

**After**

```sh
echo "$foo"

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-shellcheck)

## shellharden

Restyles _POSIX sh_, _Bash_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/anordal/shellharden#readme

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- shellharden:
    arguments: []
    command:
    - shellharden
    - --replace
    image: restyled/restyler-shellharden:v4.1.1-3
    include:
    - '**/*.sh'
    - '**/*.bash'
    interpreters:
    - sh
    - bash

```

</details>

<details>
<summary>Examples</summary>


**Before**

```sh
#!/bin/sh
x=x
var=`echo $x`

```

**After**

```sh
#!/bin/sh
x=x
var=`echo "$x"`

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-shellharden)

## shfmt

Restyles _POSIX sh_, _Bash_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/mvdan/sh#shfmt

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- shfmt:
    arguments:
    - -i
    - '2'
    - -ci
    command:
    - shfmt
    - -w
    image: public.ecr.aws/restyled-io/restyler-shfmt:v3.4.3
    include:
    - '**/*.sh'
    - '**/*.bash'
    interpreters:
    - sh
    - bash

```

</details>

<details>
<summary>Examples</summary>


**Before**

```sh
#!/bin/sh
if [ 2 -eq 2 ]
    then
        echo "yup"
    fi

```

**After**

```sh
#!/bin/sh
if [ 2 -eq 2 ]; then
  echo "yup"
fi

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-shfmt)

## sqlformat

Restyles _SQL_, _PSQL_, must be explicitly enabled.

<details>
<summary>Documentation</summary>

- https://github.com/andialbrecht/sqlparse#readme

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- sqlformat:
    arguments: []
    command:
    - sqlformat
    - --reindent
    - --keywords=upper
    image: public.ecr.aws/restyled-io/restyler-sqlformat:0.5.3
    include:
    - '**/*.sql'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```sql
-- hi there
select * from students WHERE students.age > 10;

```

**After**

```sql
-- hi there

SELECT *
FROM students
WHERE students.age > 10;

```


**Before**

```sql
INSERT
  INTO x VALUES ()

```

**After**

```sql
INSERT INTO x
VALUES ()

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-sqlformat)

## standardrb

Restyles _Ruby_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/testdouble/standard

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- standardrb:
    arguments: []
    command:
    - standardrb
    - --fix
    image: public.ecr.aws/restyled-io/restyler-standardrb:v1.50.0
    include:
    - '**/*.rb'
    interpreters:
    - ruby

```

</details>

<details>
<summary>Examples</summary>


**Before**

```ruby
def some_method
    do_something
end

```

**After**

```ruby
def some_method
  do_something
end

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-standardrb)

## stylish-haskell

Restyles _Haskell_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/jaspervdj/stylish-haskell
- https://github.com/restyled-io/restyler/wiki/Errors#stylish-haskell

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- stylish-haskell:
    arguments: []
    command:
    - stylish-haskell
    - --inplace
    image: public.ecr.aws/restyled-io/restyler-stylish-haskell:v0.14.3.0
    include:
    - '**/*.hs'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

```

**After**

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

```


**Before**

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.App
    (
    -- * Application environment
      App(..)
    , AppT
    , runAppT

    -- * Application errors
    , AppError(..)
    , mapAppError
    )
where

import Restyler.Prelude

import Conduit (runResourceT, sinkFile)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import GitHub.Endpoints.Issues.Comments
import GitHub.Endpoints.PullRequests
import GitHub.Endpoints.Repos.Statuses
import GitHub.Request
import Network.HTTP.Client.TLS
import Network.HTTP.Simple hiding (Request)
import Restyler.Capabilities.Docker
import Restyler.Capabilities.Git
import Restyler.Capabilities.GitHub
import Restyler.Capabilities.RemoteFile
import Restyler.Capabilities.System
import Restyler.Model.Config
import Restyler.Model.RemoteFile
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.Process

-- | Application environment
data App = App
    { appLogLevel :: LogLevel
    , appLogColor :: Bool
    , appAccessToken :: Text
    , appPullRequest :: PullRequest
    -- ^ The @'PullRequest'@ we are restyling
    , appConfig :: Config
    -- ^ Configuration loaded from @.restyled.yaml@
    , appRestyledPullRequest :: Maybe SimplePullRequest
    -- ^ Existing restyled @'PullRequest'@ if it exists
    }

-- | All possible application error conditions
data AppError
    = PullRequestFetchError Error
    -- ^ We couldn't fetch the @'PullRequest'@ to restyle
    | PullRequestCloneError IOException
    -- ^ We couldn't clone or checkout the PR's branch
    | ConfigurationError String
    -- ^ We couldn't load a @.restyled.yaml@
    | DockerError IOException
    -- ^ Error running a @docker@ operation
    | GitError IOException
    -- ^ Error running a @git@ operation
    | GitHubError Error
    -- ^ We encountered a GitHub API error during restyling
    | SystemError IOException
    -- ^ Trouble reading a file or etc
    | RemoteFileError IOException
    -- ^ Trouble performing some HTTP request
    | OtherError IOException
    -- ^ A minor escape hatch for @'IOException'@s
    deriving Show

-- | Run a computation, and modify any thrown @'AppError'@s
mapAppError :: MonadError AppError m => (AppError -> AppError) -> m a -> m a
mapAppError f = (`catchError` throwError . f)

newtype AppT m a = AppT
    { runAppT :: ReaderT App (LoggingT (ExceptT AppError m)) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError AppError
        , MonadReader App
        , MonadLogger
        )

-- | Run an @'IO'@ computation and capture @'IOException'@s to the given type
appIO :: MonadIO m => (IOException -> AppError) -> IO a -> AppT m a
appIO err f = AppT $ do
    result <- liftIO $ tryIO f
    either (throwError . err) pure result

instance MonadIO m => MonadGit (AppT m) where
    cloneRepository url dir = do
        logDebugN $ "git clone " <> tshow [masked, dir]
        appIO GitError $ callProcess "git" ["clone", unpack url, dir]
      where
        masked = T.unpack $ scheme <> "://<creds>" <> T.dropWhile (/= '@') rest
        (scheme, rest) = T.breakOn "://" url

    checkoutBranch b branch = do
        logDebugN $ "git checkout " <> branch
        appIO GitError $ callProcess "git" $ ["checkout"] ++ [ "-b" | b ] ++ [unpack branch]

    changedPaths branch = do
        logDebugN $ "git diff --name-only " <> branch
        appIO GitError $ lines <$> readProcess "git" ["diff", "--name-only", unpack branch] ""

    commitAll msg = do
        logDebugN "git commit"
        appIO GitError $ callProcess "git" ["commit", "-am", unpack msg]

    fetchOrigin remoteRef localRef = do
        logDebugN $ "git fetch origin " <> remoteRef <> ":" <> localRef
        appIO GitError $ callProcess "git" ["fetch", "origin", unpack $ remoteRef <> ":" <> localRef]

    pushOrigin branch = do
        logDebugN $ "git push origin " <> branch
        appIO GitError $ callProcess "git" ["push", "origin", unpack branch]

    forcePushOrigin branch = do
        logDebugN $ "git push origin " <> branch
        appIO GitError $ callProcess "git" ["push", "--force-with-lease", "origin", unpack branch]

instance MonadIO m => MonadGitHub (AppT m) where
    getPullRequest owner name num = runGitHub $ pullRequestR owner name num

    findPullRequest owner name base head = do
        results <- runGitHub $ pullRequestsForR owner name
            (optionsBase base <> optionsHead head) FetchAll
        pure $ results V.!? 0

    createPullRequest owner name create =
        runGitHub $ createPullRequestR owner name create

    updatePullRequest owner name id' edit =
        runGitHub $ updatePullRequestR owner name id' edit

    getComments owner name id' = runGitHub $ commentsR owner name id' FetchAll

    createComment owner name id' body = runGitHub_ $ createCommentR
        owner
        name
        id'
        body

    deleteComment owner name id' = runGitHub_ $ deleteCommentR owner name id'

    createStatus owner name sha status = runGitHub_ $ createStatusR owner name sha status

instance MonadIO m => MonadSystem (AppT m) where
    getCurrentDirectory = do
        logDebugN "getCurrentDirectory"
        appIO SystemError Directory.getCurrentDirectory

    doesFileExist path = do
        logDebugN $ "doesFileExist: " <> tshow path
        appIO SystemError $ Directory.doesFileExist path

    setCurrentDirectory path = do
        logDebugN $ "setCurrentDirectory: " <> tshow path
        appIO SystemError $ Directory.setCurrentDirectory path

    readFile path = do
        logDebugN $ "readFile: " <> tshow path
        appIO SystemError $ T.readFile path

    exitSuccess = appIO SystemError Exit.exitSuccess

instance MonadIO m => MonadDocker (AppT m) where
    dockerRun args = do
        logDebugN $ "docker run " <> tshow args
        appIO DockerError $ callProcess "docker" $ "run" : args

instance MonadIO m => MonadRemoteFile (AppT m) where
    fetchRemoteFile RemoteFile{..} = do
        let url = getUrl rfUrl
        logInfoN $ "Fetching " <> tshow rfPath <> " from " <> tshow url
        appIO RemoteFileError $ do
            request <- parseRequest $ unpack url
            runResourceT $ httpSink request $ \_ -> sinkFile rfPath

-- | Run a GitHub @'Request'@
runGitHub :: MonadIO m => Request k a -> AppT m a
runGitHub req = do
    logDebugN $ "GitHub request: " <> showGitHubRequest req
    auth <- asks $ OAuth . encodeUtf8 . appAccessToken
    result <- appIO OtherError $ do
        mgr <- getGlobalManager
        executeRequestWithMgr mgr auth req

    either (throwError . GitHubError) pure result

-- | @'runGitHub'@ but discard the result
runGitHub_ :: MonadIO m => Request k a -> AppT m ()
runGitHub_ = void . runGitHub

-- | Show a GitHub @'Request'@, useful for debugging
-- brittany-disable-next-binding
showGitHubRequest :: Request k a -> Text
showGitHubRequest (SimpleQuery (Query ps qs)) = mconcat
    [ "[GET] "
    , "/" <> T.intercalate "/" ps
    , "?" <> T.intercalate "&" (queryParts qs)
    ]
showGitHubRequest (SimpleQuery (PagedQuery ps qs fc)) = mconcat
    [ "[GET] "
    , "/" <> T.intercalate "/" ps
    , "?" <> T.intercalate "&" (queryParts qs)
    , " (" <> tshow fc <> ")"
    ]
showGitHubRequest (SimpleQuery (Command m ps _body)) = mconcat
    [ "[" <> T.toUpper (tshow m) <> "] "
    , "/" <> T.intercalate "/" ps
    ]
showGitHubRequest (StatusQuery _ _) = "<status query>"
showGitHubRequest (HeaderQuery _ _) = "<header query>"
showGitHubRequest (RedirectQuery _) = "<redirect query>"

queryParts :: QueryString -> [Text]
queryParts = map $ \(k, mv) -> decodeUtf8 k <> "=" <> maybe "" decodeUtf8 mv

```

**After**

```haskell
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Restyler.App
    (
    -- * Application environment
      App(..)
    , AppT
    , runAppT

    -- * Application errors
    , AppError(..)
    , mapAppError
    )
where

import           Restyler.Prelude

import           Conduit                          (runResourceT, sinkFile)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import qualified Data.Vector                      as V
import           GitHub.Endpoints.Issues.Comments
import           GitHub.Endpoints.PullRequests
import           GitHub.Endpoints.Repos.Statuses
import           GitHub.Request
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple              hiding (Request)
import           Restyler.Capabilities.Docker
import           Restyler.Capabilities.Git
import           Restyler.Capabilities.GitHub
import           Restyler.Capabilities.RemoteFile
import           Restyler.Capabilities.System
import           Restyler.Model.Config
import           Restyler.Model.RemoteFile
import qualified System.Directory                 as Directory
import qualified System.Exit                      as Exit
import           System.Process

-- | Application environment
data App = App
    { appLogLevel            :: LogLevel
    , appLogColor            :: Bool
    , appAccessToken         :: Text
    , appPullRequest         :: PullRequest
    -- ^ The @'PullRequest'@ we are restyling
    , appConfig              :: Config
    -- ^ Configuration loaded from @.restyled.yaml@
    , appRestyledPullRequest :: Maybe SimplePullRequest
    -- ^ Existing restyled @'PullRequest'@ if it exists
    }

-- | All possible application error conditions
data AppError
    = PullRequestFetchError Error
    -- ^ We couldn't fetch the @'PullRequest'@ to restyle
    | PullRequestCloneError IOException
    -- ^ We couldn't clone or checkout the PR's branch
    | ConfigurationError String
    -- ^ We couldn't load a @.restyled.yaml@
    | DockerError IOException
    -- ^ Error running a @docker@ operation
    | GitError IOException
    -- ^ Error running a @git@ operation
    | GitHubError Error
    -- ^ We encountered a GitHub API error during restyling
    | SystemError IOException
    -- ^ Trouble reading a file or etc
    | RemoteFileError IOException
    -- ^ Trouble performing some HTTP request
    | OtherError IOException
    -- ^ A minor escape hatch for @'IOException'@s
    deriving Show

-- | Run a computation, and modify any thrown @'AppError'@s
mapAppError :: MonadError AppError m => (AppError -> AppError) -> m a -> m a
mapAppError f = (`catchError` throwError . f)

newtype AppT m a = AppT
    { runAppT :: ReaderT App (LoggingT (ExceptT AppError m)) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError AppError
        , MonadReader App
        , MonadLogger
        )

-- | Run an @'IO'@ computation and capture @'IOException'@s to the given type
appIO :: MonadIO m => (IOException -> AppError) -> IO a -> AppT m a
appIO err f = AppT $ do
    result <- liftIO $ tryIO f
    either (throwError . err) pure result

instance MonadIO m => MonadGit (AppT m) where
    cloneRepository url dir = do
        logDebugN $ "git clone " <> tshow [masked, dir]
        appIO GitError $ callProcess "git" ["clone", unpack url, dir]
      where
        masked = T.unpack $ scheme <> "://<creds>" <> T.dropWhile (/= '@') rest
        (scheme, rest) = T.breakOn "://" url

    checkoutBranch b branch = do
        logDebugN $ "git checkout " <> branch
        appIO GitError $ callProcess "git" $ ["checkout"] ++ [ "-b" | b ] ++ [unpack branch]

    changedPaths branch = do
        logDebugN $ "git diff --name-only " <> branch
        appIO GitError $ lines <$> readProcess "git" ["diff", "--name-only", unpack branch] ""

    commitAll msg = do
        logDebugN "git commit"
        appIO GitError $ callProcess "git" ["commit", "-am", unpack msg]

    fetchOrigin remoteRef localRef = do
        logDebugN $ "git fetch origin " <> remoteRef <> ":" <> localRef
        appIO GitError $ callProcess "git" ["fetch", "origin", unpack $ remoteRef <> ":" <> localRef]

    pushOrigin branch = do
        logDebugN $ "git push origin " <> branch
        appIO GitError $ callProcess "git" ["push", "origin", unpack branch]

    forcePushOrigin branch = do
        logDebugN $ "git push origin " <> branch
        appIO GitError $ callProcess "git" ["push", "--force-with-lease", "origin", unpack branch]

instance MonadIO m => MonadGitHub (AppT m) where
    getPullRequest owner name num = runGitHub $ pullRequestR owner name num

    findPullRequest owner name base head = do
        results <- runGitHub $ pullRequestsForR owner name
            (optionsBase base <> optionsHead head) FetchAll
        pure $ results V.!? 0

    createPullRequest owner name create =
        runGitHub $ createPullRequestR owner name create

    updatePullRequest owner name id' edit =
        runGitHub $ updatePullRequestR owner name id' edit

    getComments owner name id' = runGitHub $ commentsR owner name id' FetchAll

    createComment owner name id' body = runGitHub_ $ createCommentR
        owner
        name
        id'
        body

    deleteComment owner name id' = runGitHub_ $ deleteCommentR owner name id'

    createStatus owner name sha status = runGitHub_ $ createStatusR owner name sha status

instance MonadIO m => MonadSystem (AppT m) where
    getCurrentDirectory = do
        logDebugN "getCurrentDirectory"
        appIO SystemError Directory.getCurrentDirectory

    doesFileExist path = do
        logDebugN $ "doesFileExist: " <> tshow path
        appIO SystemError $ Directory.doesFileExist path

    setCurrentDirectory path = do
        logDebugN $ "setCurrentDirectory: " <> tshow path
        appIO SystemError $ Directory.setCurrentDirectory path

    readFile path = do
        logDebugN $ "readFile: " <> tshow path
        appIO SystemError $ T.readFile path

    exitSuccess = appIO SystemError Exit.exitSuccess

instance MonadIO m => MonadDocker (AppT m) where
    dockerRun args = do
        logDebugN $ "docker run " <> tshow args
        appIO DockerError $ callProcess "docker" $ "run" : args

instance MonadIO m => MonadRemoteFile (AppT m) where
    fetchRemoteFile RemoteFile{..} = do
        let url = getUrl rfUrl
        logInfoN $ "Fetching " <> tshow rfPath <> " from " <> tshow url
        appIO RemoteFileError $ do
            request <- parseRequest $ unpack url
            runResourceT $ httpSink request $ \_ -> sinkFile rfPath

-- | Run a GitHub @'Request'@
runGitHub :: MonadIO m => Request k a -> AppT m a
runGitHub req = do
    logDebugN $ "GitHub request: " <> showGitHubRequest req
    auth <- asks $ OAuth . encodeUtf8 . appAccessToken
    result <- appIO OtherError $ do
        mgr <- getGlobalManager
        executeRequestWithMgr mgr auth req

    either (throwError . GitHubError) pure result

-- | @'runGitHub'@ but discard the result
runGitHub_ :: MonadIO m => Request k a -> AppT m ()
runGitHub_ = void . runGitHub

-- | Show a GitHub @'Request'@, useful for debugging
-- brittany-disable-next-binding
showGitHubRequest :: Request k a -> Text
showGitHubRequest (SimpleQuery (Query ps qs)) = mconcat
    [ "[GET] "
    , "/" <> T.intercalate "/" ps
    , "?" <> T.intercalate "&" (queryParts qs)
    ]
showGitHubRequest (SimpleQuery (PagedQuery ps qs fc)) = mconcat
    [ "[GET] "
    , "/" <> T.intercalate "/" ps
    , "?" <> T.intercalate "&" (queryParts qs)
    , " (" <> tshow fc <> ")"
    ]
showGitHubRequest (SimpleQuery (Command m ps _body)) = mconcat
    [ "[" <> T.toUpper (tshow m) <> "] "
    , "/" <> T.intercalate "/" ps
    ]
showGitHubRequest (StatusQuery _ _) = "<status query>"
showGitHubRequest (HeaderQuery _ _) = "<header query>"
showGitHubRequest (RedirectQuery _) = "<redirect query>"

queryParts :: QueryString -> [Text]
queryParts = map $ \(k, mv) -> decodeUtf8 k <> "=" <> maybe "" decodeUtf8 mv

```


**Before**

```haskell
module Foo where
import FrontRow.Renaissance.RenaissancePlatform
import FrontRow.Renaissance.RPID
import FrontRow.Renaissance.RPIdentifier

```

**After**

```haskell
module Foo where
import           FrontRow.Renaissance.RenaissancePlatform
import           FrontRow.Renaissance.RPID
import           FrontRow.Renaissance.RPIdentifier

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-stylish-haskell)

## taplo

Restyles _TOML_, runs automatically.

<details>
<summary>Documentation</summary>

- https://taplo.tamasfe.dev/cli/usage/formatting.html

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- taplo:
    arguments: []
    command:
    - taplo
    - fmt
    image: restyled/restyler-taplo:0.9.3
    include:
    - '**/*.toml'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```toml
var   = true
list = [
  'hi',
    'there'
  , 'now'
 ]

```

**After**

```toml
var = true
list = ['hi', 'there', 'now']

```


**Before**

```toml
long_list = [ 'hi', 'there' , 'now' , 'now' , 'now' , 'now' , 'now'
  , 'now'
  , 'now'
  , 'now'
  , 'now'
  , 'now'
  , 'now'
  , 'now'
 ]

```

**After**

```toml
long_list = [
  'hi',
  'there',
  'now',
  'now',
  'now',
  'now',
  'now',
  'now',
  'now',
  'now',
  'now',
  'now',
  'now',
  'now',
]

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-taplo)

## terraform

Restyles _Terraform_, runs automatically.

<details>
<summary>Documentation</summary>

- https://www.terraform.io/docs/commands/fmt.html

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- terraform:
    arguments: []
    command:
    - terraform
    - fmt
    image: public.ecr.aws/restyled-io/restyler-terraform:v1.12.2
    include:
    - '**/*.tf'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```terraform
locals = {
  short = 1
  this_is_longer = true
  this_is_really_longer_than_it_needs_to_be = "bazzle"
}

```

**After**

```terraform
locals = {
  short                                     = 1
  this_is_longer                            = true
  this_is_really_longer_than_it_needs_to_be = "bazzle"
}

```


**Before**

```terraform
variable "policy_definition_name" {
    description  = "Policy definition name must only contain lowercase letters, digits or dashes, cannot use dash as the first two or last one characters, cannot contain consecutive dashes, and is limited between 2 and 60 characters in length."
    default    = "demoPolicy"
}

```

**After**

```terraform
variable "policy_definition_name" {
  description = "Policy definition name must only contain lowercase letters, digits or dashes, cannot use dash as the first two or last one characters, cannot contain consecutive dashes, and is limited between 2 and 60 characters in length."
  default     = "demoPolicy"
}

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-terraform)

## verible

Restyles _System Verilog_, runs automatically.

<details>
<summary>Documentation</summary>

- https://google.github.io/verible/verilog_format.html

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- verible:
    arguments: []
    command:
    - verible-verilog-format
    - --inplace
    image: public.ecr.aws/restyled-io/restyler-verible:v0.0-4007-g98bdb38a
    include:
    - '**/*.sv'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```verilog
module debounce(
  input wire logic clk, output logic debounced
   );
stateType ns;

  always_comb
            begin
             ns = ERR;
       end

  logic timerDone;
     logic clrTimer;
   endmodule

```

**After**

```verilog
module debounce (
    input wire logic clk,
    output logic debounced
);
  stateType ns;

  always_comb begin
    ns = ERR;
  end

  logic timerDone;
  logic clrTimer;
endmodule

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-verible)

## whitespace

Restyles _*_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/restyled-io/restyler-whitespace/blob/master/README.md

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- whitespace:
    arguments: []
    command:
    - whitespace
    image: restyled/restyler-whitespace:v0.2.0.0
    include:
    - '**/*'
    - '!**/*.gif'
    - '!**/*.ico'
    - '!**/*.jpeg'
    - '!**/*.jpg'
    - '!**/*.pdf'
    - '!**/*.png'
    - '!**/fonts/**/*'
    interpreters: []

```

</details>

<details>
<summary>Examples</summary>


**Before**

```
line one  
line two 
  
line three \
line four

```

**After**

```
line one
line two

line three \
line four

```


**Before**

```
line one
line two

line three



```

**After**

```
line one
line two

line three

```


**Before**

```
line one
line two

line three
```

**After**

```
line one
line two

line three

```


**Before**

```
line one
line two
line three

```

**After**

```
line one
line two
line three

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-whitespace)

## yapf

Restyles _Python_, runs automatically.

<details>
<summary>Documentation</summary>

- https://github.com/google/yapf

</details>

<details>
<summary>Configuration</summary>

```yaml
restylers:
- yapf:
    arguments: []
    command:
    - yapf
    - --in-place
    image: restyled/restyler-yapf:v0.43.0-1
    include:
    - '**/*.py'
    interpreters:
    - python

```

</details>

<details>
<summary>Examples</summary>


**Before**

```python
import math, sys;
def example1():
    ####This is a long comment. This should be wrapped to fit within 72 characters.
    some_tuple=(   1,2, 3,'a'  );
    some_variable={'long':'Long code lines should be wrapped within 79 characters.',
    'other':[math.pi, 100,200,300,9876543210,'This is a long string that goes on'],
    'more':{'inner':'This whole logical line should be wrapped.',some_tuple:[1,
    20,300,40000,500000000,60000000000000000]}}
    return (some_tuple, some_variable)

```

**After**

```python
import math, sys


def example1():
    ####This is a long comment. This should be wrapped to fit within 72 characters.
    some_tuple = (1, 2, 3, 'a')
    some_variable = {
        'long':
        'Long code lines should be wrapped within 79 characters.',
        'other': [
            math.pi, 100, 200, 300, 9876543210,
            'This is a long string that goes on'
        ],
        'more': {
            'inner': 'This whole logical line should be wrapped.',
            some_tuple: [1, 20, 300, 40000, 500000000, 60000000000000000]
        }
    }
    return (some_tuple, some_variable)

```


</details>

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-yapf)

