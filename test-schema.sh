#!/bin/sh

mkdir -p dist
python3 schema/test/avro/generate.py schema/test/avro/example.avsc dist/avro-python.avro
stack exec test-avro dist/avro-python.avro dist/avro-haskell.avro
python3 schema/test/avro/consume.py schema/test/avro/example.avsc dist/avro-haskell.avro