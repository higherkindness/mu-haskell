{ nixpkgs ? (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.09.tar.gz)
, system ? builtins.currentSystem
}:

let
  haskellnix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz);
  overlay = _: pkgs:
    let
      hnPkgs = pkgs.haskell-nix.stackProject {
        src = ./.;
        modules = [];
      };
    in {
      mu-grpc-server = hnPkgs.mu-grpc-server.components.all;
      mu-rpc = hnPkgs.mu-rpc.components.library;
      mu-schema = hnPkgs.mu-schema.components.library;
      mu-grpc-client = hnPkgs.mu-grpc-client.components.library;
      compendium-client = hnPkgs.compendium-client.components.library;
      mu-persistent = hnPkgs.mu-persistent.components.library;
      mu-avro = hnPkgs.mu-avro.components.all;
      mu-protobuf = hnPkgs.mu-protobuf.components.all;
      mu-example-with-persistent = hnPkgs.mu-example-with-persistent.components.all;
      mu-example-todolist = hnPkgs.mu-example-todolist.components.all;
      mu-example-seed = hnPkgs.mu-example-seed.components.all;
      mu-example-route-guide = hnPkgs.mu-example-route-guide.components.all;
      mu-example-health-check = hnPkgs.mu-example-health-check.components.all;
    };
in (import nixpkgs {
  overlays = haskellnix.overlays ++ [ overlay ];
  config = haskellnix.config // {};
  inherit system;
})
