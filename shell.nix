let
  project = import ./.;
in
  project.project.shellFor {
    tools = {
      cabal = "3.2.0.0";
    };
    buildInputs = [
      project.tools.hls
    ];
    exactDeps = true;
  }
