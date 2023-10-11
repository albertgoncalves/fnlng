with import <nixpkgs> {};
mkShell {
    buildInputs = [
        ghc
        hlint
        mold
        ormolu
    ];
    shellHook = ''
    '';
}
