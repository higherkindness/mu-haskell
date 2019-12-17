---
layout: docs
title: Compendium
permalink: benchmarks/
---


# What is compendium?
compendium is a standalone solution, implemented as an HTTP service, that provides **storage, conversion and client generation for your schemas in a format-agnostic fashion**.

## Features and comparison with Schema Registry

|Feature                          |compendium |Schema Registry|
|-------------------------------  |-----------|---------------|
|RESTful interface                |yes        |yes            |
|Schema storage                   |yes        |yes            |
|Schema versioning                |yes        |yes            |
|Pluggable storage backends       |yes        |no (Kafka)     |
|Format-agnostic                  |yes        |no (Avro)      |
|Conversion between formats       |yes        |no             |
|Client code generation           |yes        |no             |

Pluggable storage backends means that you can use whatever storage fits your needs, being filesystem and PostgreSQL the current out of the box backends provided by compendium at the moment.

Format-agnostic ensures that you can use any of the most popular schema formats, ranging from binaries like Avro and Protocol Buffers, to more web related like OpenAPI, in a transparent manner.

Conversion between formats saves a ton of work trying to manually adapt schemas that you are already using, while retaining type safety and ensuring formats compatibility.

Finally, code generation (currently only Scala) is a very interesting feature that allows putting into use all those stored schemas without worrying about their future changes and evolutions.

# How to use compendium

compendium can be used calling the RESTful interface directly, just as you would use other schema solutions like Schema Registry.

## SBT plugin (Scala)

In order to use code generation, an SBT plugin is also provided [here](https://github.com/higherkindness/sbt-compendium). This plugin handles all the hassle of calling the compendium server, leaving Scala files in the managed sources folders for your SBT project, ready to be compiled.

# Examples

Take a look at this [example](https://github.com/higherkindness/compendium-example) to get a grasp on how to integrate your projects with compendium, making use of SBT plugin to generate Scala code from schemas.

# Contributing

Please review the [Contribution document](https://github.com/higherkindness/compendium/blob/master/CONTRIBUTING.md) for information on how to get started contributing to the project.
