#!/bin/sh

# In order to run this script you need
# - avro for Python 3 https://avro.apache.org/releases.html
# - protobuf for Python 2 https://github.com/protocolbuffers/protobuf/releases/
#   follow https://github.com/protocolbuffers/protobuf/tree/master/python

echo "BUILDING"
stack build
mkdir -p dist

echo "\nAVRO\n====\n"
echo "python/generate"
python3 schema/test/avro/generate.py schema/test/avro/example.avsc dist/avro-python.avro
stack exec test-avro dist/avro-haskell.avro dist/avro-python.avro
echo "ptyhon/consume"
python3 schema/test/avro/consume.py schema/test/avro/example.avsc dist/avro-haskell.avro

echo "\nPROTOBUF\n========\n"
echo "python/generate"
python schema/test/protobuf/generate.py dist/protobuf-python.pbuf
stack exec test-protobuf dist/protobuf-haskell.pbuf dist/protobuf-python.pbuf
echo "python/consume"
python schema/test/protobuf/consume.py dist/protobuf-haskell.pbuf