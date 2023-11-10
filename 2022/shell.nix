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
        pkg.hip
        pkg.sbv
    ]);
in
pkgs.mkShell {
    # Find locales on Fedora/Debian/Arch/...
    LOCALE_ARCHIVE="/usr/lib/locale/locale-archive";

    nativeBuildInputs = [
        ghc
        pkgs.cabal-install
        pkgs.cabal2nix
        pkgs.stylish-haskell
        pkgs.haskell-language-server
    ];
}
