
# 2.1.0 (Unreleased)

* Stop generating debian metapackages and packages with the version attached
  to the package name. Updates in the apt-repository now support multiple
  package versions and upgrading `gtirb` with `apt-get upgrade`.

# 2.0.0

* The Java API has been substantially reworked. Including:
  * Most of the core API classes now have a more polished interface.
  * The API for handling AuxData has been completely redesigned.
  * New testing infrastructure based on JUnit5 has been added.
  * More extensive test cases have been added.
* Add elfStackExec and elfStackSize AuxData definitions
* Add `IR.modules_named` helper method to Python API.
* Add `IR.findModules(String name)` helper method to Java API.

# 1.12.0

* Add elfDynamicInit and elfDynamicFini AuxData definitions

# 1.11.0

Note that this release, due to changes to the protobuf definitions of symbolic
expression attributes, is backwards-incompatible with previous GTIRB files.

* Replace symbolic expression attributes with composable labels.

# 1.10.9

* Added support for bool values in AuxData.
* Added elfSymbolVersions provisional AuxData definition.
* The GTIRB file format has changed to include a prefix containing a "magic"
  identifier and the GTIRB protobuf version number to allow easier id of
  GTIRB files. The change is not backwards compatible. Newer versions of
  GTIRB will not be able to load older GTIRB files.
* Converted the decode mode from an arbitrary integer into a ProtoBuf enum.
  This obviously breaks compatibility with older GTIRB files.

# 1.10.8

* Ubuntu 18 and gcc7 are no longer supported.


# 1.10.7

* Added support for floating-point numbers in AuxData.
* Disabled testing the Python API using `setup.py test`. The tests can still be
  run using ctest or Python's unittest module.
* Improved the performance of the Python API's ByteBlock.references property.
* Fixed a pair of bugs in C++ support for variants in AuxData

# 1.10.6

* Added type annotations to python API and made them available in package.
* Removed install-python target.

# 1.10.5

* Added various symbolic expression attributes.
* Updated Java API

# 1.10.4

* Removed SymStackConst support
* Added variant (union) support for AuxData
* Removed address and size from modules
* Modified Module::findSections(string) to return a range instead of iterator
* Added IR::findSection(string) to C++ API

# 1.10.3

* Added offset helpers to Python API
* Better support for `std::byte` when working with `ByteInterval`
* Fixed a bug which manifested when serializing a big-endian IR

# 1.10.2

* Updated "address" iteration order to compare size if addresses are the same
  and UUIDs if addresses and sizes are the same so that it can be used to
  store objects in ordered containers.
* Fixed some bugs where modifying an object's address or size would cause
  findNodeOn to return incorrect results.
* Improved performance of findNodeOn and findNodeAt queries.

# 1.10.1

* Added module-level endianess flag, for use in archtectures with multiple
  possible endians for its code blocks.
* Introduce iteration helpers cfgPredecessors and cfgSuccessors.

# 1.9.0

* Introduce attributes for symbolic expressions to the core IR.
* Reduce asymptotic complexity of iterating over blocks in a large number of
  byte intervals.
* Fixed bug where searching for blocks by address could return incorrect matches.
* Fixed bug where `ByteInterval::addBlock` would refuse to move an existing
  block to a new offset if it was already present.

# 1.8.5

* Make Python Offset objects immutable and make Offsets equivalent when they
  refer to the same displacement from the same element.

# 1.8.4

* Fix bug where Symbol iteration could get out of order when symbols refer to
  blocks in byte intervals that are relocated.

# 1.8.3

* Fix bug that didn't add CodeBlocks to the CFG if the CodeBlocks were added to
  a ByteInterval before it was added to the IR.

# 1.8.2

* The C++ API build no longer generates a `libgtirb.so.1` symlink. This has the
  effect of requiring clients to link against the full version number (e.g.,
  `libgtirb.so.1.8.2`) to ensure ABI compatibility while we continue to make
  rapid improvements to the library.

# 1.5.0

* In the Python API:
  * Removed `Node.from_uuid` and added `get_by_uuid` to `IR`s.
    This changes UUID lookup from a global cache to a per-IR cache;
    this means you can now have two IRs exist that share UUIDs but have
    different contents, for example.
  * Added convienience properties to all node types to find the parent nodes
    they belong to.

# 1.4.6

* Implement std::hash for Addr objects.

# 1.4.5

* Explicitly disable copy and move constructors for the Node class hierarchy in
  C++. This avoids a class of errors where nodes cannot be found by
  Node::getByUUID.

# 1.4.4

* Build/install libgtirb.so.1 symlink on linux.

# 1.4.3
* Remove the python-egg cmake target, add the python-wheel cmake target

# 1.4.2

* Don't use __declspec(dllimport) on Windows.

# 1.4.1

* Add ISA enums for PPC64, ARM64, MIPS32, and MIPS64.

# 1.3.2

* Access functions for converting to/from protobuf are no longer public in the C++ API.
* The proto library is no longer dllexported.
* GTIRB_EXPORT_API no longer uses dllimport on the client side.

# 1.3.1

* No longer installs Python files by default. Added a new 'install-python'
  target to install Python files.

# 1.3.0

* Added a new field to symbols, `at_end`, which allows symbols to point to the end
  of their referents as well as the beginning.

# 1.2.1

* Moved protobuf definitions into gtirb.proto package (gtirb::proto namespace
  in C++).
* Installing the Python API now respects CMAKE_INSTALL_PREFIX and DESTDIR with
  their usual semantics.

# 1.2.0

* AuxData and AuxDataContainer in the C++ API have been reworked to provide cleaner type safety.
  * AuxData is now retrieved directly from an AuxDataContainer using
	a schema class that specifies both the name of the AuxData object
	as well as its type.
  * Schemata for AuxData types must be registered at process startup
    before GTIRB objects are constructed or unserialized.

# 1.1.1

* Fixed a bug where changing the address of a block caused lookups of symbol
  by address to fail in some cases.

# 1.1.0

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
