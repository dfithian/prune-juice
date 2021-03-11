## 0.6

* Better support for Cabal
* Support `.hs-boot` files

## 0.5

* Use `cabal` files instead of `hpack` as a way to support more users
* More command-line options
  * Ignore some classes of unused dependencies like test discovery and `Paths_`-type modules
  * Verbose logging
* Bug fixes
  * Inferring common dependencies for packages without a base library
  * Loading main modules
  * Loading literate haskell files
  * Detecting imports that correspond to multiple dependencies

## 0.4

* Performance enhancements for calling `ghc-pkg`

## 0.3

* Initial release (prior releases were duds)
