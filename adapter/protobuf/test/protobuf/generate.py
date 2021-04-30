from example_pb2 import *
import sys

example_address = address()
example_address.postcode = "0000AA"
example_address.country = "Nederland"

example_foo = Foo()
example_foo.foo_string = "blah"

example_person = person()
example_person.firstName = "Pythonio"
example_person.lastName = "van Gogh"
example_person.age = 30
example_person.gender = male
for i in [1,2,3]:
  example_person.lucky_numbers.append(i)
example_person.address.CopyFrom(example_address)
example_person.things["hola"]  = 1
example_person.things["hello"] = 2
example_person.things["hallo"] = 3
example_person.foo.CopyFrom(example_foo)

f = open(sys.argv[1], "wb")
f.write(example_person.SerializeToString())
f.close()
print(example_person)
