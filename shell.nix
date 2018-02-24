{ pkgs ? import <nixpkgs> {} }:

with { op = pkgs.ocamlPackages_latest; };

pkgs.stdenv.mkDerivation {
  name = "c4b";

  src = ./.;

  buildInputs = [
    (pkgs.buildEnv { name = "clp"; paths = [ pkgs.coinutils pkgs.clp ]; })
    pkgs.ncurses
    op.cil
    op.ocaml
    op.findlib 
  ];

  installPhase = ''
    make libinstall
  '';
}

