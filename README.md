# Restylers

Language-specific restylers.

## Contributing

See:

- https://github.com/restyled-io/restyled.io/wiki/Contributing-to-Restyled
- https://github.com/restyled-io/restyled.io/wiki/Adding-a-Restyler
- https://github.com/restyled-io/restyled.io/wiki/Restyler-Info-Yaml

## Releasing

Requires the [Restyled SDK](https://github.com/restyled-io/sdk#installation).

Build, test, lint, and push any new images:

```console
restyled restylers {name}/info.yaml
```

Update the `dev` channel:

```console
restyled restylers-release-dev
```

Promote `dev` to `stable`:

```console
restyled promote --yes dev stable
```

## LICENSE

What minimal code and configuration there is in this repository is itself
[Commons Claused][cc], MIT licensed, as is all of Restyled's codebase. Projects
installed into the Docker images defined here are governed by their own
licenses.

For a detailed description of another project's rationale for this licensing
model, one with which I agree, see [here][level].

[cc]: https://commonsclause.com/
[level]: https://leveljournal.com/source-available-licensing
