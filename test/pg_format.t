  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

pg_format

  $ run_restyler pg_format ./bad.sql
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
