Version Information
===================

All APIs must provide functionality for retrieving the following
version information.

- The GTIRB version corresponding to the API, as a string.
- The Protobuf version supported by the API, as an integer.  This must
  match the value of the CMake `GTIRB_PROTOBUF_VERSION` variable.


API Implementations
-------------------

The required functionality is provided as follows.

| Language    | GTIRB version               | Protobuf version               |
|:------------|:----------------------------|:-------------------------------|
| C++         | macro GTIRB_VERSION_STRING  | macro GTIRB_PROTOBUF_VERSION   |
| Python      | gtirb.version.API_VERSION   | gtirb.version.PROTOBUF_VERSION |
| Common Lisp | constant **gtirb-version**  | constant **protobuf-version**  |
