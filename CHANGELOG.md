# Unreleased

* Added a new API for accessing GTIRB, written in Java. This API is not yet
  released, and as such, has missing features, is not yet documented, and may
  change at any time. For more information, look at the contents of the `java`
  directory.

# 1.0.0

This is a major backwards-incompatible release.  The protobuf
specification has changed significantly resulting in protobuf version
1 which is now tracked in `version.txt` in the base of this
repository.  (The original protobuf version was version 0.)  The
changes in this release are primarily intended to enable *binary
rewriting* use cases on GTIRB.  Other changes to the protobuf
specification are for more general cleanup, simplification, and
clarification.  In addition, a new Common Lisp GTIRB API is now
included along with the C++ and Python APIs.  A list of specific
changes follows.  Complete documentation of all new objects and
structures is provided in the GTIRB manual.

* A `version` field is now present on GTIRB IR instances.  The value
  of this field is now `1`.  The old value of `0` is the protobuf
  default for a missing field.
* The control flow graph (CFG) is now a child of the IR instead of
  living under a specific module.  This means that a multi-module IR
  now has a single pan-module CFG.
* The `Block` object has been renamed to `CodeBlock` and the
  `DataObject` to `DataBlock`.
* A new object has been added to the GTIRB `Section`s named
  `ByteInterval`s.  This replaces the `ByteMap` in the previous GTIRB
  version.  A `ByteInterval` has:
  * An *optional* fixed address indicating its location in memory.
    Without an address the location of the `ByteInterval` is not
    specified allowing it to float to enable easier binary rewriting.
  * A `size` specifying the extend of the `ByteInterval` in memory.
    If this size is larger than the contents of the `ByteInterval`
    then the extension of the `ByteInterval` in memory beyond the end
    of the contents is un-allocated.
  * A byte vector named `contents` holding the contents of the `ByteInterval`.
  * A map from offsets to symbolic expressions.
  * A list of blocks holding `CodeBlock`s and `DataBlock`s.
* The `address` field has been removed from `Block`s.
* The `address` and `size` fields have been removed from `Section`s.
* An offset from the start of their `ByteInterval` have been added to blocks.
* The following fields have been removed from `Module`s:
  * `image_byte_map`
  * `symbolic_operands`
  * `blocks` and
  * `data`.
* An entry point stored as a `CodeBlock` has been added to `Module`s.
* A list of `ByteIntervals` has been added to `Section`s.
* The `ISAID` enumeration on module is renamed to `ISA`.
* Instead of an `AuxDataContainer` object we now hold a
  `map<string, AuxData>` on modules and IRs.
* The following GTIRB enumerations are modified: `ISA`, `FileFormat`,
  `SymbolKind`, and `SectionFlag`.  The goals of these modifications
  is to simplify the enumerations and ensure that all included options
  are both necessary and orthogonal.
* Sections now include have `SectionFlag`s to store common properties
  such as `readable`, `writeable`, or `executable`.

# 0.3.0

* You can now enable and disable the building of certain APIs when calling CMake,
  via the following flags:
  * `GTIRB_CXX_API` to control the building of the C++ API, on by default
  * `GTIRB_PY_API` to control the building of the Python API, on by default if `python3` is installed on your system
* The following changes have been made to the Python API:
  * `Serialization.decode` can now take a `bytes` object
    in addition to a `BytesIO` object.
  * If an unknwon type is encountered while decoding `AuxData`,
    it will be placed in `data` as a `bytes`-like object
    instead of throwing a `DecodeError`.
    Unknown data decoded this way can be then encoded again.
    It is still an error to encode unknown types of auxdata
    not in the manner described above.
* ImageByteMap::setData() has been extended to support arbitrary iterator types.
* We now build documentation for the Python API using
  [Sphinx](https://www.sphinx-doc.org/en/master/). To generate all
  documentation locally, call `make doc` after calling `cmake`; this will
  generate both C++ and Python API documentation. To only make one or the
  other, call `make doxy` or `make sphinx`, respectively.
  * Making the Sphinx documentation will require the following Python packages:
    ```bash
    pip3 install sphinx sphinx-autodoc-typehints
    ```

# 0.2.0

* Added a new Python API, meant to be a high-level wrapper over the
  Protobuf-generated one. To make use of it, add the `python` folder
  from your build directory to your `PYTHONPATH`. The package is named `gtirb`.
* CMake now won't automatically download and install its dependencies,
  so that the user has control over which versions are in use.
  The version requirements for Boost and Protobuf are listed in `README.md`.
* Updated the sanctioned AuxData definitions.
* Fix for build issue when using Boost 1.71.0.

# 0.1.1

* Initial public release.
