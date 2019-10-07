{ pkgs ? import ./nix { } }:

let
  haskell = import pkgs.sources."haskell.nix" { inherit pkgs; };

  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import ./nix/pkgs.nix;
    pkg-def-extras = [ ];
    modules = [ ];
  };
in {
  hsPkgs = pkgSet.config.hsPkgs;
  site = import ./nix/site.nix {
    nixpkgs = pkgs;
    hspkgs = pkgSet.config.hsPkgs;
  };
}
