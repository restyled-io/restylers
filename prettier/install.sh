#!/usr/bin/env bash
set -euo pipefail

plugins=(
  tailwindcss
  @trivago/prettier-plugin-sort-imports
)

for plugin in "${plugins[@]}"; do
  (cd /app/node_modules/"$plugin" && yarn link)
done

cat >/usr/local/bin/prettier <<EOM
#!/usr/bin/env bash
yarn --offline link ${plugins[*]} >/dev/null
trap 'yarn --offline unlink ${plugins[*]} >/dev/null' EXIT
/app/node_modules/.bin/prettier "\$@"
EOM

chmod +x /usr/local/bin/prettier

# Copy as a legacy name we may see as CMD in the wild
cp /usr/local/bin/prettier{,-with-tailwindcss}
