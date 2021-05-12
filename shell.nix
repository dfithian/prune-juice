let
  project = import ./.;
  config = import ./config.nix;
in
  project.project.shellFor {
    tools = {
      cabal = config.cabalVersion;
      ghcid = config.ghcidVersion;
    };
    buildInputs = [
      project.tools.hls
    ];
    exactDeps = true;
  }
