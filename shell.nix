let
  project = import ./.;
  config = import ./config.nix;
in
  project.project.shellFor {
    tools = {
      cabal = config.cabalVersion;
      ghcid = config.ghcidVersion;
      hasktags = config.hasktagsVersion;
      haskdogs = config.haskdogsVersion;
    };
    buildInputs = [
      project.tools.hls
    ];
    exactDeps = true;
  }
