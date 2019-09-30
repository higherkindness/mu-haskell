#!/bin/sh

stack build
mkdir -p dist
echo "python/generate"
python3 schema/test/avro/generate.py schema/test/avro/example.avsc dist/avro-python.avro
stack exec test-avro dist/avro-haskell.avro dist/avro-python.avro
echo "ptyhon/consume"
python3 schema/test/avro/consume.py schema/test/avro/example.avsc dist/avro-haskell.avro