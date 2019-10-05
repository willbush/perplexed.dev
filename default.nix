{ mkDerivation, base, hakyll, stdenv }:
mkDerivation {
  pname = "perplexed-dev";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
