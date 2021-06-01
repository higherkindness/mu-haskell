#!/bin/sh
echo "BUILDING"
stack build mu-avro mu-protobuf
mkdir -p dist

echo "\nAVRO\n====\n"
echo "python/generate"
python3 adapter/avro/test/avro/generate.py adapter/avro/test/avro/example.avsc dist/avro-python.avro
stack test-avro dist/avro-haskell.avro dist/avro-python.avro
echo "ptyhon/consume"
python3 adapter/avro/test/avro/consume.py adapter/avro/test/avro/example.avsc dist/avro-haskell.avro

# if protobuf is not installed, do so with 'pip install protobuf'
echo "\nPROTOBUF\n========\n"
echo "python/generate"
python2 adapter/protobuf/test/protobuf/generate.py dist/protobuf-python.pbuf
stack exec test-protobuf dist/protobuf-haskell.pbuf dist/protobuf-python.pbuf
echo "python/consume"
python2 adapter/protobuf/test/protobuf/consume.py dist/protobuf-haskell.pbuf
