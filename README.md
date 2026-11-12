# Hypergraph

[Paclet Repository](https://resources.wolframcloud.com/PacletRepository/resources/WolframInstitute/Hypergraph/)

Install development version:

```WL
PacletInstall["https://wolfr.am/Hypergraph.paclet"]
```
## Development

The paclet itself is in `Hypergraph/`; load a checkout with

```WL
PacletDirectoryLoad["path/to/Hypergraph/Hypergraph"]
Needs["WolframInstitute`Hypergraph`"]
```

The documentation is the markdown in `docs/`: one page per symbol in `docs/Symbols`, the guide,
the tutorial, and `docs/ResourceDefinition.md`, the paclet resource's definition.
[PacletPage](https://github.com/WolframInstitute/PacletPage) builds it into
`Hypergraph/Documentation/English` and `Hypergraph/ResourceDefinition.nb`, which are generated
and not committed:

```bash
wl -f build.wls               # changed pages only; --no-cache evaluates every example afresh
wl -f release.wls --dry-run   # build and check the paclet resource without deploying it
wl -f submit.wls --dry-run    # check a Paclet Repository submission without submitting
```
