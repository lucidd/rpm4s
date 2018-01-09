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

- Provide a strongly typed interface to work with rpm and related data.
