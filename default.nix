{ nixpkgs ? (fetchTarball https://github.com/NixOS/nixpkgs/archive/3578bb5.tar.gz)
, pkgs ? import nixpkgs (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/350b594.tar.gz))
}:

let
  hnPkgs = pkgs.haskell-nix.stackProject {
    src = ./.;
    modules = [];
  };
in {
  compendium-client = hnPkgs.compendium-client.components.library;
  mu-avro = hnPkgs.mu-avro.components.all;
  mu-example-health-check = hnPkgs.mu-example-health-check.components.all;
  mu-example-route-guide = hnPkgs.mu-example-route-guide.components.all;
  mu-example-seed = hnPkgs.mu-example-seed.components.all;
  mu-example-todolist = hnPkgs.mu-example-todolist.components.all;
  mu-example-with-persistent = hnPkgs.mu-example-with-persistent.components.all;
  mu-graphql = hnPkgs.mu-graphql.components.library;
  mu-grpc-client = hnPkgs.mu-grpc-client.components.library;
  mu-grpc-common = hnPkgs.mu-grpc-common.components.library;
  mu-grpc-server = hnPkgs.mu-grpc-server.components.all;
  mu-kafka = hnPkgs.mu-kafka.components.library;
  mu-optics = hnPkgs.mu-optics.components.library;
  mu-persistent = hnPkgs.mu-persistent.components.library;
  mu-protobuf = hnPkgs.mu-protobuf.components.all;
  mu-rpc = hnPkgs.mu-rpc.components.library;
  mu-schema = hnPkgs.mu-schema.components.library;
}
