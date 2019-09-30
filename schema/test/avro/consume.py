import avro.schema
from avro.datafile import DataFileReader
from avro.io import DatumReader
import sys

schema = avro.schema.Parse(open(sys.argv[1], "rb").read())

reader = DataFileReader(open(sys.argv[2], "rb"), DatumReader())
for person in reader:
    print(person)
reader.close()