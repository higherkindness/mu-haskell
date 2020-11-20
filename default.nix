let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/31029c1.tar.gz) {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
{ pkgs ? import nixpkgsSrc nixpkgsArgs
}:
let
  hnPkgs = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "mu-haskell";
      src = ./.;
    };
  };
in {
  compendium-client = hnPkgs.compendium-client.components.library;
  mu-avro = hnPkgs.mu-avro.components.library;
  mu-example-health-check = hnPkgs.mu-example-health-check.components.exes;
  mu-example-library = hnPkgs.mu-example-library.components.exes;
  mu-example-route-guide = hnPkgs.mu-example-route-guide.components.exes;
  mu-example-seed = hnPkgs.mu-example-seed.components.exes;
  mu-example-todolist = hnPkgs.mu-example-todolist.components.exes;
  mu-example-with-persistent = hnPkgs.mu-example-with-persistent.components.exes;
  mu-graphql = hnPkgs.mu-graphql.components.library;
  mu-grpc-client = hnPkgs.mu-grpc-client.components.library;
  mu-grpc-common = hnPkgs.mu-grpc-common.components.library;
  mu-grpc-server = hnPkgs.mu-grpc-server.components.library;
  mu-kafka = hnPkgs.mu-kafka.components.library;
  mu-lens = hnPkgs.mu-lens.components.library;
  mu-optics = hnPkgs.mu-optics.components.library;
  mu-persistent = hnPkgs.mu-persistent.components.library;
  mu-prometheus = hnPkgs.mu-prometheus.components.library;
  mu-protobuf = hnPkgs.mu-protobuf.components.library;
  mu-rpc = hnPkgs.mu-rpc.components.library;
  mu-schema = hnPkgs.mu-schema.components.library;
  mu-servant-server = hnPkgs.mu-servant-server.components.library;
  mu-tracing = hnPkgs.mu-tracing.components.library;
}
