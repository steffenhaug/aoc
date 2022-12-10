{ mkDerivation, base, lib, parsec }:
mkDerivation {
  pname = "aoc";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base parsec ];
  license = lib.licenses.mit;
}
