{ mkDerivation, base, hakyll, stdenv }:
mkDerivation {
  pname = "perplexed-dev";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll ];
  homepage = "www.perplexed.dev";
  description = "Will Bush blog";
  license = stdenv.lib.licenses.mit;
}
