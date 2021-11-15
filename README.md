# Prune Juice

![Build Status](https://github.com/dfithian/prune-juice/actions/workflows/workflow.yml/badge.svg)

Prune unused Haskell dependencies from a Haskell project. Parses `.cabal` files using either `stack.yaml` or
`cabal.project` (whichever is available), and `ghc-pkg` to load the `exposed-modules` fields of all the direct
dependencies of the packages. Parses imports of each source file, compares against the exposed modules, and errors if
any dependency listed in `<package>.cabal` is never imported by a source file in that package.

## Quickstart

To install `prune-juice` from source, clone this repo and run `stack install prune-juice`.

From there, you can run it with `prune-juice`.

If you'd like `prune-juice` to apply changes, run it as `prune-juice --apply`.  This will prompt you for
confirmation each time it writes to a file. If you'd like to bypass that confirmation, run it as
`prune-juice --apply --no-verify`. If you find that the applied changes are not working correctly, you can apply
them in "safe" mode with `prune-juice --apply --strategy safe`, though this will likely change your `.cabal` file
in other unexpected ways.

## Usage

```
$ stack install
$ prune-juice --help
Usage: prune-juice [--project-root PROJECT_ROOT] [--no-default-ignore]
                   [--no-ignore-self] [--extra-ignore EXTRA_IGNORE]
                   [--package PACKAGE] [--verbosity VERBOSITY]
                   [--build-system BUILD_SYSTEM] [--apply] [--no-verify]
                   [--strategy ARG]
  Prune a Haskell project's dependencies

Available options:
  -h,--help                Show this help text
  --project-root PROJECT_ROOT
                           Project root (default: ".")
  --no-default-ignore      Don't use the default ignore list
                           ([base,hedgehog,hspec,hspec-discover,tasty,tasty-hedgehog,tasty-hspec,tasty-hunit])
  --no-ignore-self         Error if an executable doesn't use the library it's
                           defined with
  --extra-ignore EXTRA_IGNORE
                           Dependencies(s) to ignore in addition to the default
                           ignore list
  --package PACKAGE        Package name(s)
  --verbosity VERBOSITY    Set the verbosity level (one of
                           [silent,error,info,debug]) (default: error)
  --build-system BUILD_SYSTEM
                           Build system to use instead of inference (one of
                           [stack,cabal-project,cabal])
  --apply                  Apply changes
  --no-verify              Do not ask for verification when applying (implies
                           --apply)
  --strategy ARG           Strategy to use to apply (one of [safe,smart])
                           (default: smart)
  --version                Show version and exit
```

## Known issues

In order for `prune-juice` to correctly detect local dependency usage, you have to run `stack build` (or other `ghc-pkg`
registration mechanism). `prune-juice` does not do this for you.

`prune-juice` doesn't know about test discovery or `Paths_`-type files, so there is a mechanism to ignore these types of
packages.

`prune-juice` doesn't know about package imports, so if a file has something like `import "cryptohash" Crypto.Hash` it
will interpret this as an import from both `cryptonite` and `cryptohash`, if both dependencies are available.

`--apply` and `--no-verify` only work on `.cabal` files, so `hpack`/`package.yaml` are not supported.
