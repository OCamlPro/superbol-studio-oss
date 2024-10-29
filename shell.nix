{ pkgs ? import ./nix {} }:

let 
  inherit (pkgs) ocamlPackages;
in

pkgs.mkShell {
  nativeBuildInputs = with ocamlPackages; [
    menhir
  ];
}
