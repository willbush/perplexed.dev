{ pkgs ? import ./nix { } }:

let
  haskell = import pkgs.sources."haskell.nix" { inherit pkgs; };

  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import ./nix/pkgs.nix;
    pkg-def-extras = [ ];
    modules = [ ];
  };

in pkgSet.config.hsPkgs
