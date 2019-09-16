{ nixpkgs ? import (fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) {} }:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    # NOTE: This is required so cabal finds zlib (pkgs.zlib does not suffice)
    # https://github.com/haskell/cabal/issues/5858
    zlib
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
    pkgs.cabal-install
    pkgs.zlib
    pkgs.pkgconfig
    haskellPackages.ghcid
    haskellPackages.hlint
    pkgs.cabal2nix
  ];

in pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
