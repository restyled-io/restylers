#!/usr/bin/env bash
set -euo pipefail

plugins=(
  tailwindcss
  @trivago/prettier-plugin-sort-imports
)

for plugin in "${plugins[@]}"; do
  (cd /app/node_modules/"$plugin" && yarn link)
done

cat >/usr/local/bin/prettier-with-tailwindcss <<EOM
#!/usr/bin/env bash
#
# NB. this executable handles link/unlink of all plugins, not just tailwind, but
# we're keeping it named as is to avoid errors for users who may be using
# command in their .restyled.yaml
#
###
yarn --offline link ${plugins[*]} >/dev/null
trap 'yarn --offline unlink ${plugins[*]} >/dev/null' EXIT
/app/node_modules/.bin/prettier "\$@"
EOM

chmod +x /usr/local/bin/prettier-with-tailwindcss
