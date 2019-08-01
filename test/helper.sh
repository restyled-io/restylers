run_restyler() {
  local name=$1
  shift

  "$TESTDIR"/../build/restyler-meta run "$name" "$@" || exit 1

  git diff --src-prefix=i/ --dst-prefix=w/ "$@"
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
