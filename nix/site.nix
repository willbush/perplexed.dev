{ nixpkgs, hspkgs }:
nixpkgs.stdenv.mkDerivation {
  name = "perplexed-dev-site";
  src = nixpkgs.lib.cleanSource ../.;
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";

  buildInputs = [ hspkgs.perplexed-dev.components.exes.site ];

  preConfigure = ''
    export LANG="en_US.UTF-8";
  '';

  buildPhase = ''
    site build
  '';

  installPhase = ''
    cp -r _site $out
  '';
}
