#!/bin/sh

# In order to run this script you need
# - avro for Python 3 https://avro.apache.org/releases.html
# - protobuf for Python 2 https://github.com/protocolbuffers/protobuf/releases/
#   follow https://github.com/protocolbuffers/protobuf/tree/master/python

echo "BUILDING"
stack build mu-avro mu-protobuf
mkdir -p dist

echo "\nAVRO\n====\n"
echo "python/generate"
python3 adapter/avro/test/avro/generate.py adapter/avro/test/avro/example.avsc dist/avro-python.avro
stack exec test-avro dist/avro-haskell.avro dist/avro-python.avro
echo "ptyhon/consume"
python3 adapter/avro/test/avro/consume.py adapter/avro/test/avro/example.avsc dist/avro-haskell.avro

echo "\nPROTOBUF\n========\n"
echo "python/generate"
python adapter/protobuf/test/protobuf/generate.py dist/protobuf-python.pbuf
stack exec test-protobuf dist/protobuf-haskell.pbuf dist/protobuf-python.pbuf
echo "python/consume"
python adapter/protobuf/test/protobuf/consume.py dist/protobuf-haskell.pbuf