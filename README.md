# Restylers

Language-specific restylers.

For a rendered list of all restylers with documentation, configuration, and
examples see [here](./_docs/restylers.md).

## Contributing

### Documentation

- [Adding a Restyler](https://github.com/restyled-io/restylers/wiki/Adding-a-Restyler)
- [Restyler Info Schema](https://github.com/restyled-io/restylers/wiki/Restyler-Info-Schema)

### Running tests

1. Install [`restyle`][install-restyle]
2. Install [The Haskell Tool Stack][install-stack]
3. Run `just test <restyler>`

[install-restyle]: https://github.com/restyled-io/restyler?tab=readme-ov-file#installation
[install-stack]: https://docs.haskellstack.org/en/stable/#how-to-install-stack

### Commit Messages

To ensure CI runs properly, use a [conventional commit][conventionalcommits]
message with the name of the restyler as the `scope`. For example,

- `feat(ruff): add Ruff restyler`
- `fix(prettier): fix configuration handling`

[conventionalcommits]: https://www.conventionalcommits.org/en/v1.0.0/#summary

### Release

Changes are promoted to the `dev` channel immediately, which can be used by
adding:

```yaml
restylers_version: dev
```

To your `.restyled.yaml`.

`dev` is promoted to `stable` on the 1st and 15th of every month.

If a change is not available when expected by the above description, that is
usually a bug in our CI/CD pipelines. Please raise an Issue.

## LICENSE

Restylers themselves are released under the same license as the project they
package. In most cases, the appropriate license file has been copied into the
Restyler sub-directory. In cases where it's not: patches welcome.

All other code in this project is licensed AGPLv3. See [COPYING](./COPYING).
