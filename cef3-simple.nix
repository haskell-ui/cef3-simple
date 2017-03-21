{ mkDerivation, base, cef3-raw, stdenv }:
mkDerivation {
  pname = "cef3-simple";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base cef3-raw ];
  description = "Simple wrapper around cef3-raw";
  license = stdenv.lib.licenses.bsd3;
}
