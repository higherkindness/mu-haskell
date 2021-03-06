# -*- coding: utf-8 -*-
# Generated by the protocol buffer compiler.  DO NOT EDIT!
# source: example.proto
"""Generated protocol buffer code."""
from google.protobuf.internal import enum_type_wrapper
from google.protobuf import descriptor as _descriptor
from google.protobuf import message as _message
from google.protobuf import reflection as _reflection
from google.protobuf import symbol_database as _symbol_database
# @@protoc_insertion_point(imports)

_sym_db = _symbol_database.Default()




DESCRIPTOR = _descriptor.FileDescriptor(
  name='example.proto',
  package='',
  syntax='proto3',
  serialized_options=None,
  create_key=_descriptor._internal_create_key,
  serialized_pb=b'\n\rexample.proto\"\xf0\x01\n\x06person\x12\x11\n\tfirstName\x18\x01 \x01(\t\x12\x10\n\x08lastName\x18\x02 \x01(\t\x12\x0b\n\x03\x61ge\x18\x03 \x01(\x05\x12\x17\n\x06gender\x18\x04 \x01(\x0e\x32\x07.gender\x12\x19\n\x07\x61\x64\x64ress\x18\x05 \x01(\x0b\x32\x08.address\x12\x19\n\rlucky_numbers\x18\x06 \x03(\x05\x42\x02\x10\x01\x12#\n\x06things\x18\x07 \x03(\x0b\x32\x13.person.ThingsEntry\x12\x11\n\x03\x66oo\x18\x08 \x01(\x0b\x32\x04.Foo\x1a-\n\x0bThingsEntry\x12\x0b\n\x03key\x18\x01 \x01(\t\x12\r\n\x05value\x18\x02 \x01(\x05:\x02\x38\x01\",\n\x07\x61\x64\x64ress\x12\x10\n\x08postcode\x18\x01 \x01(\t\x12\x0f\n\x07\x63ountry\x18\x02 \x01(\t\"5\n\x03\x46oo\x12\x11\n\x07\x66oo_int\x18\x01 \x01(\x05H\x00\x12\x14\n\nfoo_string\x18\x02 \x01(\tH\x00\x42\x05\n\x03\x46oo*&\n\x06gender\x12\x06\n\x02nb\x10\x00\x12\x08\n\x04male\x10\x01\x12\n\n\x06\x66\x65male\x10\x02\x62\x06proto3'
)

_GENDER = _descriptor.EnumDescriptor(
  name='gender',
  full_name='gender',
  filename=None,
  file=DESCRIPTOR,
  create_key=_descriptor._internal_create_key,
  values=[
    _descriptor.EnumValueDescriptor(
      name='nb', index=0, number=0,
      serialized_options=None,
      type=None,
      create_key=_descriptor._internal_create_key),
    _descriptor.EnumValueDescriptor(
      name='male', index=1, number=1,
      serialized_options=None,
      type=None,
      create_key=_descriptor._internal_create_key),
    _descriptor.EnumValueDescriptor(
      name='female', index=2, number=2,
      serialized_options=None,
      type=None,
      create_key=_descriptor._internal_create_key),
  ],
  containing_type=None,
  serialized_options=None,
  serialized_start=361,
  serialized_end=399,
)
_sym_db.RegisterEnumDescriptor(_GENDER)

gender = enum_type_wrapper.EnumTypeWrapper(_GENDER)
nb = 0
male = 1
female = 2



_PERSON_THINGSENTRY = _descriptor.Descriptor(
  name='ThingsEntry',
  full_name='person.ThingsEntry',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='key', full_name='person.ThingsEntry.key', index=0,
      number=1, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='value', full_name='person.ThingsEntry.value', index=1,
      number=2, type=5, cpp_type=1, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=b'8\001',
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=213,
  serialized_end=258,
)

_PERSON = _descriptor.Descriptor(
  name='person',
  full_name='person',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='firstName', full_name='person.firstName', index=0,
      number=1, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='lastName', full_name='person.lastName', index=1,
      number=2, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='age', full_name='person.age', index=2,
      number=3, type=5, cpp_type=1, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='gender', full_name='person.gender', index=3,
      number=4, type=14, cpp_type=8, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='address', full_name='person.address', index=4,
      number=5, type=11, cpp_type=10, label=1,
      has_default_value=False, default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='lucky_numbers', full_name='person.lucky_numbers', index=5,
      number=6, type=5, cpp_type=1, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=b'\020\001', file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='things', full_name='person.things', index=6,
      number=7, type=11, cpp_type=10, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='foo', full_name='person.foo', index=7,
      number=8, type=11, cpp_type=10, label=1,
      has_default_value=False, default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[_PERSON_THINGSENTRY, ],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=18,
  serialized_end=258,
)


_ADDRESS = _descriptor.Descriptor(
  name='address',
  full_name='address',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='postcode', full_name='address.postcode', index=0,
      number=1, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='country', full_name='address.country', index=1,
      number=2, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=260,
  serialized_end=304,
)


_FOO = _descriptor.Descriptor(
  name='Foo',
  full_name='Foo',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  create_key=_descriptor._internal_create_key,
  fields=[
    _descriptor.FieldDescriptor(
      name='foo_int', full_name='Foo.foo_int', index=0,
      number=1, type=5, cpp_type=1, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
    _descriptor.FieldDescriptor(
      name='foo_string', full_name='Foo.foo_string', index=1,
      number=2, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=b"".decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR,  create_key=_descriptor._internal_create_key),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
    _descriptor.OneofDescriptor(
      name='Foo', full_name='Foo.Foo',
      index=0, containing_type=None,
      create_key=_descriptor._internal_create_key,
    fields=[]),
  ],
  serialized_start=306,
  serialized_end=359,
)

_PERSON_THINGSENTRY.containing_type = _PERSON
_PERSON.fields_by_name['gender'].enum_type = _GENDER
_PERSON.fields_by_name['address'].message_type = _ADDRESS
_PERSON.fields_by_name['things'].message_type = _PERSON_THINGSENTRY
_PERSON.fields_by_name['foo'].message_type = _FOO
_FOO.oneofs_by_name['Foo'].fields.append(
  _FOO.fields_by_name['foo_int'])
_FOO.fields_by_name['foo_int'].containing_oneof = _FOO.oneofs_by_name['Foo']
_FOO.oneofs_by_name['Foo'].fields.append(
  _FOO.fields_by_name['foo_string'])
_FOO.fields_by_name['foo_string'].containing_oneof = _FOO.oneofs_by_name['Foo']
DESCRIPTOR.message_types_by_name['person'] = _PERSON
DESCRIPTOR.message_types_by_name['address'] = _ADDRESS
DESCRIPTOR.message_types_by_name['Foo'] = _FOO
DESCRIPTOR.enum_types_by_name['gender'] = _GENDER
_sym_db.RegisterFileDescriptor(DESCRIPTOR)

person = _reflection.GeneratedProtocolMessageType('person', (_message.Message,), {

  'ThingsEntry' : _reflection.GeneratedProtocolMessageType('ThingsEntry', (_message.Message,), {
    'DESCRIPTOR' : _PERSON_THINGSENTRY,
    '__module__' : 'example_pb2'
    # @@protoc_insertion_point(class_scope:person.ThingsEntry)
    })
  ,
  'DESCRIPTOR' : _PERSON,
  '__module__' : 'example_pb2'
  # @@protoc_insertion_point(class_scope:person)
  })
_sym_db.RegisterMessage(person)
_sym_db.RegisterMessage(person.ThingsEntry)

address = _reflection.GeneratedProtocolMessageType('address', (_message.Message,), {
  'DESCRIPTOR' : _ADDRESS,
  '__module__' : 'example_pb2'
  # @@protoc_insertion_point(class_scope:address)
  })
_sym_db.RegisterMessage(address)

Foo = _reflection.GeneratedProtocolMessageType('Foo', (_message.Message,), {
  'DESCRIPTOR' : _FOO,
  '__module__' : 'example_pb2'
  # @@protoc_insertion_point(class_scope:Foo)
  })
_sym_db.RegisterMessage(Foo)


_PERSON_THINGSENTRY._options = None
_PERSON.fields_by_name['lucky_numbers']._options = None
# @@protoc_insertion_point(module_scope)
