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
                   [--no-ignore-self] [--extra-ignore EXTRA_IGNORE]
                   [--package PACKAGE] [--verbose]
  Prune a Haskell project's dependencies

Available options:
  -h,--help                Show this help text
  --project-root PROJECT_ROOT
                           Project root (default: ".")
  --no-default-ignore      Don't use the default ignore list
                           ([base,hspec,tasty])
  --no-ignore-self         Error if an executable doesn't use the library it's
                           defined with
  --extra-ignore EXTRA_IGNORE
                           Dependencies(s) to ignore in addition to the default
                           ignore list
  --package PACKAGE        Package name(s)
  --verbose                Turn on verbose logging
```

## Known issues

In order for `prune-juice` to correctly detect local dependency usage, you have to run `stack build` (or other `ghc-pkg`
registration mechanism). `prune-juice` does not do this for you.

`prune-juice` doesn't know about test discovery or `Paths_`-type files, so there is a mechanism to ignore these types of
packages.

`prune-juice` doesn't know about package imports, so if a file has something like `import "cryptohash" Crypto.Hash` it
will interpret this as an import from both `cryptonite` and `cryptohash`, if both dependencies are available.
