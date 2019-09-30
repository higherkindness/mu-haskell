from example_pb2 import *

example_address = address()
example_address.postcode = "0000AA"
example_address.country = "Nederland"

example_person = person()
example_person.firstName = "Pythonio"
example_person.lastName = "van Gogh"
example_person.age = 30
example_person.gender = male
example_person.address.CopyFrom(example_address)

f = open(sys.argv[1], "wb")
f.write(example_person.SerializeToString())
f.close()
print(example_person)