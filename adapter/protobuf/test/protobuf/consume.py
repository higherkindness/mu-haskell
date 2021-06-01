from example_pb2 import *
import sys

f = open(sys.argv[1], "rb")
example_person = person()
example_person.ParseFromString(f.read())
f.close()
print(example_person)
