{
  description = "Nix flake to build and export the flipU R Package";

  inputs = {
    # requires you setup your ssh keys with github https://github.com/settings/keys
    nixr.url = "git+ssh://git@github.com/Displayr/NixR?ref=untracked-add-util-output";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixr, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixr.pkgs.${system};
        extractRVersion = nixr.extractRVersion;

        descriptionFile = builtins.readFile ./DESCRIPTION;
        packageVersion = extractRVersion descriptionFile;
      in {
        packages.flipU = pkgs.callPackage ./default.nix { 
          inherit pkgs packageVersion;
        };
        defaultPackage = self.packages.${system}.flipU;
      }
    );
}