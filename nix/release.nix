{ withHoogle ? false }:
let
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-unstable.json; };

  customHaskellPkgs = pinnedPkgs.haskellPackages.override (old: {
    overrides = pinnedPkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      project1 = self.callPackage ../default.nix { };
      # addditional overrides go here
    });
  });

  hoogleAugmentedPackages =
    if withHoogle
      then customHaskellPkgs.override (old: {
             overrides = pinnedPkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
               ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
               ghcWithPackages = self.ghc.withPackages;
             });
           })
      else customHaskellPkgs;
in
  { project1 = hoogleAugmentedPackages.project1;
  }
