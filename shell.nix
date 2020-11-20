(import <nixpkgs> {}).mkShell {
  shellHook = ''
    ${(import ./default.nix).pre-commit-check.shellHook}
  '';
}
