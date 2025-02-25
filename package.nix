{ pkgs ? import <nixpkgs> {}, packageVersion }:

pkgs.rPackages.buildRPackage {
  name = "flipU";
  version = packageVersion;
  src = ./.;
  description = "Utility functions used across multiple flip packages.";
  depends = [ ];
  Rtests = with pkgs.rPackages; [ testthat ];
  lazyData = true;
}