  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

pg_format

  $ run_restyler_cmd pg_format pg_format-inplace ./bad.sql
  diff --git i/bad.sql w/bad.sql
  index d4cb666..71d12cc 100644
  --- i/bad.sql
  +++ w/bad.sql
  @@ -1,3 +1,7 @@
  -SELECT * from
  -students
  -WHERE students.age > 10;
  +SELECT
  +    *
  +FROM
  +    students
  +WHERE
  +    students.age > 10;
  +
