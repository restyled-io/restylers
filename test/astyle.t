  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

astyle

  $ run_restyler astyle ./Foo.java ./Foo.cpp
  Formatted  ./Foo.java
  Formatted  ./Foo.cpp
  diff --git i/Foo.cpp w/Foo.cpp
  index f5680da..df055a4 100644
  --- i/Foo.cpp
  +++ w/Foo.cpp
  @@ -2,16 +2,16 @@
   #include <stdio.h>
   int main()
   {
  -   FILE * pFile;
  -   char buffer [100];
  -   pFile = fopen ("myfile.txt" , "r");
  -   if (pFile == NULL) perror ("Error opening file");
  -   else {
  -     while ( ! feof (pFile) ) {
  -       if ( fgets (buffer , 100 , pFile) == NULL ) break;
  -       fputs (buffer , stdout);
  -     }
  -     fclose (pFile);
  -   }
  -   return 0;
  +    FILE * pFile;
  +    char buffer [100];
  +    pFile = fopen ("myfile.txt", "r");
  +    if (pFile == NULL) perror ("Error opening file");
  +    else {
  +        while ( ! feof (pFile) ) {
  +            if ( fgets (buffer, 100, pFile) == NULL ) break;
  +            fputs (buffer, stdout);
  +        }
  +        fclose (pFile);
  +    }
  +    return 0;
   }
  diff --git i/Foo.java w/Foo.java
  index 805238c..45b86bf 100644
  --- i/Foo.java
  +++ w/Foo.java
  @@ -1,8 +1,9 @@
   int Foo(bool isBar)
  -    {
  +{
       if (isBar) {
           bar();
  -        return 1; }
  +        return 1;
  +    }
       else
  -    \treturn 0; (esc)
  +        return 0;
   }
