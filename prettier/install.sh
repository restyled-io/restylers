#!/usr/bin/env bash
set -euo pipefail

plugins=(
  tailwindcss
  @trivago/prettier-plugin-sort-imports
)

for plugin in "${plugins[@]}"; do
  (cd node_modules/"$plugin" && yarn link)
done

cat >/usr/local/bin/prettier <<EOM
#!/usr/bin/env bash
if ! yarn --offline link ${plugins[*]} >/dev/null; then
  echo "Failed to link yarn modules" >&2
  echo "Please report this as an issue" >&2
  echo "https://github.com/restyled-io/restylers/issues" >&2
  exit 1
fi

trap 'yarn --offline unlink ${plugins[*]} >/dev/null || true' EXIT

node_modules/.bin/prettier "\$@"
EOM

chmod +x /usr/local/bin/prettier

# Copy as a legacy name we may see as CMD in the wild
cp /usr/local/bin/prettier{,-with-tailwindcss}
