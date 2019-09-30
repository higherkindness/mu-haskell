import avro.schema
from avro.datafile import DataFileWriter
from avro.io import DatumWriter
import sys

example_address = { "postcode": "0000AA", "country": "Nederland"}
example_person1 = {"firstName": "Pythonio", "lastName": "van Gogh", "age": 30, "gender": "male", "address": example_address}
example_person2 = {"firstName": "Fortyseveriano", "lastName": "van Gogh", "address": example_address}

print(example_person1)
print(example_person2)

schema = avro.schema.Parse(open(sys.argv[1], "rb").read())

writer = DataFileWriter(open(sys.argv[2], "wb"), DatumWriter(), schema)
writer.append(example_person1)
writer.append(example_person2)
writer.flush()
writer.close()