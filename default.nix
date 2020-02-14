{ nixpkgs ? (fetchTarball https://github.com/NixOS/nixpkgs/archive/484ea7bbac5bf7f958b0bb314a3c130b6c328800.tar.gz)
, pkgs ? import nixpkgs (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/659d80f.tar.gz))
}:

let
  hnPkgs = pkgs.haskell-nix.stackProject {
   src = ./.;
   modules = [];
  };
in {
  compendium-client = hnPkgs.compendium-client.components.library;
  mu-avro = hnPkgs.mu-avro.components.all;
  mu-example-health-check-avro = hnPkgs.mu-example-health-check-avro.components.all;
  mu-example-health-check-protobuf = hnPkgs.mu-example-health-check-protobuf.components.all;
  mu-example-route-guide = hnPkgs.mu-example-route-guide.components.all;
  mu-example-seed-avro = hnPkgs.mu-example-seed-avro.components.all;
  mu-example-seed-protobuf = hnPkgs.mu-example-seed-protobuf.components.all;
  mu-example-todolist = hnPkgs.mu-example-todolist.components.all;
  mu-example-with-persistent = hnPkgs.mu-example-with-persistent.components.all;
  mu-grpc-common = hnPkgs.mu-grpc-common.components.library;
  mu-grpc-client = hnPkgs.mu-grpc-client.components.library;
  mu-grpc-server = hnPkgs.mu-grpc-server.components.all;
  mu-persistent = hnPkgs.mu-persistent.components.library;
  mu-protobuf = hnPkgs.mu-protobuf.components.all;
  mu-rpc = hnPkgs.mu-rpc.components.library;
  mu-schema = hnPkgs.mu-schema.components.library;
  mu-optics = hnPkgs.mu-optics.components.library;
  mu-kafka = hnPkgs.mu-kafka.components.library;
}
