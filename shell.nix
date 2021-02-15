with import <nixpkgs> {};
mkShell {
  buildInputs = [
    texlive.combined.scheme-full
  ];
}
