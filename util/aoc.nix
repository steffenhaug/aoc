{ mkDerivation, base, containers, hip, lib, parsec }:
mkDerivation {
  pname = "aoc";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base containers hip parsec ];
  license = lib.licenses.mit;
}
