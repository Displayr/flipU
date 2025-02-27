{ pkgs ? import <nixpkgs> {} , displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipU";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = "Utility functions used across multiple flip packages.";
  propagatedBuildInputs = with pkgs.rPackages; [ R_utils ];
}