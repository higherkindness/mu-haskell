self: pkgs:
with pkgs;
let
  python3-packages = python-packages: with python-packages; [
    avro
  ];
  python3 = pkgs.python36Packages.python.withPackages python3-packages;
  python2-packages = python-packages: with python-packages; [
    protobuf
  ];
  python2 = pkgs.python27Packages.python.withPackages python2-packages;
  stack = pkgs.stack;
in
{
  test-schema = writeShellScriptBin "test-schema.sh" ''
  #!/bin/sh
  echo "BUILDING"
  ${stack}/bin/stack build mu-avro mu-protobuf
  mkdir -p dist

  echo "\nAVRO\n====\n"
  echo "python/generate"
  ${python3}/bin/python adapter/avro/test/avro/generate.py adapter/avro/test/avro/example.avsc dist/avro-python.avro
  ${stack}/bin/stack test-avro dist/avro-haskell.avro dist/avro-python.avro
  echo "ptyhon/consume"
  ${python3}/bin/python adapter/avro/test/avro/consume.py adapter/avro/test/avro/example.avsc dist/avro-haskell.avro

  echo "\nPROTOBUF\n========\n"
  echo "python/generate"
  ${python2}/bin/python adapter/protobuf/test/protobuf/generate.py dist/protobuf-python.pbuf
  ${stack}/bin/stack exec test-protobuf dist/protobuf-haskell.pbuf dist/protobuf-python.pbuf
  echo "python/consume"
  ${python2}/bin/python adapter/protobuf/test/protobuf/consume.py dist/protobuf-haskell.pbuf
  '';
}
