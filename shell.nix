{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
    # Find locales on Fedora/Debian/Arch/...
    LOCALE_ARCHIVE="/usr/lib/locale/locale-archive";

    nativeBuildInputs = [
        pkgs.ghc
        pkgs.haskell-language-server
        pkgs.cabal-install
    ];
}
