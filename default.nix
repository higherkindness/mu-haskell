{ nixpkgs ? (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.09.tar.gz)
, system ? builtins.currentSystem
}:

let
  haskellnix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz);
  overlay = _: pkgs:
    let
      hnPkgs = pkgs.haskell-nix.stackProject {
        src = ./.;
        #inherit stackYaml;
        modules = [];
      };
    in {
      mu-grpc-server = hnPkgs.mu-grpc-server.components.all;
    };
in (import nixpkgs {
  overlays = haskellnix.overlays ++ [ overlay ];
  config = haskellnix.config // {};
  inherit system;
}).mu-grpc-server
