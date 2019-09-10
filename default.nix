{ nixpkgs ? import (fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) {} }:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    zlib
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
    pkgs.cabal-install
    pkgs.zlib
    pkgs.pkgconfig
    haskellPackages.ghcid
  ];

in pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
