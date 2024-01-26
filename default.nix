{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    dune_3
    ocamlPackages.ocaml
    ocamlPackages.lsp
    ocamlPackages.ocamlformat_0_26_0
    ocamlPackages.odoc
  ];
}
