# Clean up diff noise like:
#
#   diff --git i/matrix.js w/matrix.js
#   index 430121c..811d19c 100644
#   --- i/matrix.js
#   +++ w/matrix.js
#   @@ -1,5 +1 @@
#
#
clean_diff() {
  sed '
  /^diff --git.*$/d;
  /^index [0-9a-f]\{7\}\.\.[0-9a-f]\{7\} .*$/d;
  /^--- .\/.*$/d;
  /^+++ .\/\(.*\)$/d;
  /^@@ .* @@.*$/d
  '
}

run_restyler() {
  local name=$1
  shift

  "$TESTDIR"/../build/restyler-meta run "$name" "$@" || exit 1

  git diff "$@" | clean_diff
}

set -e

mkdir repo
cd repo
cp "$TESTDIR"/fixtures/* .

{
  git init
  git add .
  git commit -m "Add fixture files"
} >/dev/null

echo "If you don't see this, setup failed"
set +e
