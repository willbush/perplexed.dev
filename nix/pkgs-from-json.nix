{ json }:
let
  bootstrapPkgs = import <nixpkgs> {};
  nixpkgs = builtins.fromJSON (builtins.readFile json);
  src = bootstrapPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs-channels";
    inherit (nixpkgs) rev sha256;
  };
in
  import src {}
