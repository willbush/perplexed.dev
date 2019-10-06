{ pkgs ? import ./nix { } }:
let
  hsPkgs = import ./default.nix { inherit pkgs; };
in hsPkgs.shellFor {
  # Include only the local packages of this project.
  packages = ps: with ps; [ perplexed-dev ];

  # When true this builds a Hoogle documentation index of all dependencies, and
  # provides a "hoogle" command to search the index.
  withHoogle = true;

  buildInputs = with pkgs; [
    haskell.compiler.ghc865
    haskellPackages.cabal-install
  ];
}
