{ pkgs ? import <nixpkgs> {} }:
let
    ghc = pkgs.ghc.withPackages(pkg: []);
in
pkgs.mkShell {
    # Find locales on Fedora/Debian/Arch/...
    LOCALE_ARCHIVE="/usr/lib/locale/locale-archive";

    nativeBuildInputs = [
        ghc
        pkgs.haskell-language-server
    ];
}
