{ pkgs ? import <nixpkgs> {} }:
let
    # Override the haskell packages to include local libs.
    hs = pkgs.haskellPackages.extend(self: super: {
        aoc = self.callPackage ./util/aoc.nix {};
    });

    ghc = hs.ghcWithPackages(pkg: [
        pkg.aoc
        pkg.matrix
        pkg.vector
    ]);
in
pkgs.mkShell {
    # Find locales on Fedora/Debian/Arch/...
    LOCALE_ARCHIVE="/usr/lib/locale/locale-archive";

    nativeBuildInputs = [
        ghc
        pkgs.cabal2nix
        pkgs.haskell-language-server
    ];
}
