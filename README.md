# Prune Juice

Prune unused Haskell dependencies from a Stack/Hpack project. Uses `hpack` to parse the project `package.yaml` files,
and `ghc-pkg` to load the `exposed-modules` fields of all the direct dependencies of the packages. Parses imports of
each source file, compares against the exposed modules, and errors if any dependency listed in `package.yaml` is never
imported by a source file.

## Usage

```bash
stack install
prune-juice --help
```
