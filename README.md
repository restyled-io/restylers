# Restylers

Language-specific restylers.

See [restyler](https://github.com/restyled-io/restyler) for more details.

## Adding a Restyler

1. Create a new directory named for the tool, which houses a `Dockerfile`

   [Example commit](https://github.com/restyled-io/restylers/commit/06e93f4ec4ccf611158943b4999ab4ff53312d58)

1. Build the restyler's Docker image locally

   ```console
   make ALL_RESTYLERS=...
   ```

1. Updated restyler to know about the new tool

   - Add to `allRestylers`
   - Add a test case in `RunSpec.hs`

   [Example commit](https://github.com/restyled-io/restyler/commit/c539db204e9169029fc9e4134fa61ee9b41ddeb9)

1. Add the new tool to the [documentation](https://restyled.io/docs#restylers)

   [HTML source](https://github.com/restyled-io/restyled.io/blob/master/templates/docs.hamlet)

## Releasing

```console
make restylers.release ALL_RESTYLERS=...
```
