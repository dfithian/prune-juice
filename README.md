# Prune Juice

[![Build Status](https://travis-ci.com/dfithian/prune-juice.svg?branch=main)](https://travis-ci.com/dfithian/prune-juice)

Prune unused Haskell dependencies from a Haskell project. Parses `.cabal` files using either `stack.yaml` or
`cabal.project` (whichever is available), and `ghc-pkg` to load the `exposed-modules` fields of all the direct
dependencies of the packages. Parses imports of each source file, compares against the exposed modules, and errors if
any dependency listed in `<package>.cabal` is never imported by a source file in that package.

## Usage

```bash
$ stack install
$ prune-juice --help
Usage: prune-juice [--project-root PROJECT_ROOT] [--default-ignore]
                   [--ignore IGNORE] [--package PACKAGE]
  Prune a Haskell project's dependencies

Available options:
  -h,--help                Show this help text
  --project-root PROJECT_ROOT
                           Project root (default: ".")
  --default-ignore         Use the default ignore list (base, hspec)
  --ignore IGNORE          Dependencies(s) to ignore (overrides the default
                           list)
  --package PACKAGE        Package name(s)
```

## Notes

In order for `prune-juice` to correctly detect local dependency usage, you have to run `stack build` (or other `ghc-pkg`
registration mechanism). `prune-juice` does not do this for you.
