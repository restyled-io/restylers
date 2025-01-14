# Restylers

Language-specific restylers.

For a rendered list of all restylers with documentation, configuration, and examples
see [here](./_docs/restylers.md).

## Tests

1. Install [`restyle`][install-restyle]
2. Install [The Haskell Tool Stack][install-stack]
3. Run `just test <restyler>`

[install-restyle]: https://github.com/restyled-io/restyler?tab=readme-ov-file#installation
[install-stack]: https://docs.haskellstack.org/en/stable/#how-to-install-stack

## Releasing

Merges to `main` automatically tag versions, release a manifest at that
tag, and update the `dev` manifest.

The Promote workflow can be run manually to promote any tag to another,
typically to promote `dev` to `stable`. It also runs twice a month.

## LICENSE

Restylers themselves are released under the same license as the project they
package. In most cases, the appropriate license file has been copied into the
Restyler sub-directory. In cases where it's not: patches welcome.

All other code in this project is licensed AGPLv3. See [COPYING](./COPYING).
