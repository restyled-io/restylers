run_restyler() {
  local name=$1
  shift

  "$TESTDIR"/../build/restyler-meta run "$name" "$@" || exit 1

  git diff "$@"
}

set -e

mkdir repo
cd repo
cp -r "$TESTDIR"/fixtures/* .

{
  git init
  git add .
  git commit -m "Add fixture files"
} >/dev/null

echo "If you don't see this, setup failed"
set +e
