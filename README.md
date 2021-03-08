# Prune Juice

[![Build Status](https://travis-ci.com/dfithian/prune-juice.svg?branch=main)](https://travis-ci.com/dfithian/prune-juice)

Prune unused Haskell dependencies from a Stack/Hpack project. Uses `hpack` to parse the project `package.yaml` files,
and `ghc-pkg` to load the `exposed-modules` fields of all the direct dependencies of the packages. Parses imports of
each source file, compares against the exposed modules, and errors if any dependency listed in `package.yaml` is never
imported by a source file in that package.

## Usage

```bash
$ stack install
$ prune-juice --help
Usage: prune-juice [--stack-yaml-file STACK_YAML_FILE] [--package PACKAGE]
  Prune a Stack project's dependencies

Available options:
  -h,--help                Show this help text
  --stack-yaml-file STACK_YAML_FILE
                           Location of stack.yaml (default: "stack.yaml")
  --package PACKAGE        Package name(s)
```

## Notes

In order for `prune-juice` to correctly detect local dependency usage, you must have run `stack build` (or other
`ghc-pkg` registration mechanism). `prune-juice` does not do this for you.
