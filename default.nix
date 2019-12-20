{ pkgs ? import <nixpkgs> (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz))
}:
  pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  }
