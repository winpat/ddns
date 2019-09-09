{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.zlib
    pkgs.pkgconfig
  ];

  #extraCmds = ''
  #  export LD_LIBRARY_PATH+=:${pkgs.zlib}/lib
  #'';
}
