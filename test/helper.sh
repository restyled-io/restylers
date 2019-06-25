run_restyler() {
  local name=$1
  shift

  local paths=()
  local path

  "$TESTDIR"/../build/restyler-meta run "$name" "$@" || exit 1

  for path; do
    if [ -e "$path" ]; then
      paths+=("$path")
    fi
  done

  git diff "${paths[@]}"
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
