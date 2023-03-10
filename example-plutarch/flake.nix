{
  description = "Template plutarch project";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };

  inputs = {
    # TODO: nixpkg follows?
    tooling.url = github:mlabs-haskell/mlabs-tooling.nix;
    ply.url = github:mlabs-haskell/ply?ref=0.4.0;
    flake-utils.url = "github:numtide/flake-utils";
    plutarch.url = "github:Plutonomicon/plutarch-plutus?ref=95e40b42a1190191d0a07e3e4e938b72e6f75268";
  };

  outputs = inputs@{ self, tooling, nixpkgs, flake-utils, ... }:
    tooling.lib.mkFlake { inherit self; }
      {
        imports = [
          (tooling.lib.mkHaskellFlakeModule1 {
            project.src = ./.;
            # project.compiler-nix-name = "ghc8107";
            project.extraHackage = [
              "${inputs.ply}/ply-core"
              "${inputs.ply}/ply-plutarch"
              "${inputs.plutarch}"
              "${inputs.plutarch}/plutarch-extra"
            ];
          })
        ];
      };

  # exported-scripts =
  #   let
  #     exporter = self'.packages."mlabs-plutus-template-onchain:exe:exporter";
  #   in
  #     pkgs.runCommand "exported-scripts" { }
  #       ''
  #         set -e
  #         mkdir $out
  #         ${exporter}/bin/exporter
  #       ''; 
}
