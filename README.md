[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-0.6.17.svg)](https://www.scala-js.org)
[![Build Status](https://travis-ci.org/lucidd/rpm4s.svg?branch=master)](https://travis-ci.org/lucidd/rpm4s)
[![codecov](https://codecov.io/gh/lucidd/rpm4s/branch/master/graph/badge.svg)](https://codecov.io/gh/lucidd/rpm4s)

# rpm4s

rpm4s is a library for working with rpm files in a easy and strongly typed way.
It fully implemented in scala and does not require librpm as dependency.

# Current state

This project is still under heavy development and will probably change a lot.
While parsing rpms was already successfully tested on 100k+ rpms, there are still
some unimplemented codepaths and missing headers that are not implemented yet.
Most of those things will be implemented on demand once i encounter them in an rpm.
In case you give this libaray a try and encounter an error parsing some rpm,
please let me know by opening an issue. :smile:

# Goals

## Easy to use, hard to mess up

The main focus of this library is to make it hard for the user to do the wrong thing.
In other rpm libraries version, names, licenses, etc.. are all just strings and most
programms just pass them around in this raw form. The problem with this is that
those strings are often not validated and even if they are validated in one part of the
programm, once you pass them along each function would need to revalidate or blindly assume
they are valid. This blind assumption is what leads to bug and in the worst case security risks.
In this model nothing stops the user from accidentaly passing a rpm name value to a function
requiring an rpm version. The worst part about this is that in most cases this would not even fail
since most rpm names could be interpreted as a version.
Furthermore passing around raw string representations of potentally complex structured data like
EVR or the rpm license header (which can contains a dsl more complex license setups),
makes it hard to do anything meaningful without parsing it first.

The way rpm4s trys to avoid those issues is by providing a type for each rpm concept.
The only way to get hold of a value of those types is to construct them in a valid way
or pass the raw rpm repesentation through some validation and parsing step.
Either way once you have an instance of the type it will be valid.
In addition those types should expose the complex structure of those values in an easy to use way
so there is no additional validation or parsing required.

## Rich functionality

Given individual types for all rpm related concepts we want to provide a rich set of
functionality for working with them. For example things like comparing and or getting compatibility
information for architectures or licenses will be explored in the future.

## Performance

While performance is not a primary goal its still important to provide the
best possible performance within the constrains of our other goals.
