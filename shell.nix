with import <nixpkgs> {};
mkShell {
    buildInputs = [
        ocaml
        ocamlPackages.ocp-indent
    ];
    shellHook = ''
    '';
}
