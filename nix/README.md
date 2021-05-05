## Setup

```bash
niv init
niv add input-output-hk/haskell.nix -n haskellNix
nix-build -A prune-juice.components.exes.prune-juice
```
