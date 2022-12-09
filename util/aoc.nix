{ mkDerivation, base, lib }:
mkDerivation {
  pname = "aoc";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base ];
  license = lib.licenses.mit;
}
