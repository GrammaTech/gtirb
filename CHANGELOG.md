# Unreleased

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
