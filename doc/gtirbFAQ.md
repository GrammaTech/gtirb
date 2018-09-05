GTIRB Frequently Asked Questions
=================================

These are questions that have came up during design, development, testing, and
deployment of the GTIRB.

## Design Questions:

**Q: Can an analysis for a binary compiled for _Platform A_ be completed by GT-
IRB tools running on _Platform B_?**

A: Yes.  GTIRB will not depend on any properties of the platform on
which it is compiled/running.

**Q: Will the GTIRB support 3rd Party binary to IR processors?**

A: Yes.  GTIRB will assume that the user has possession of a 3rd party
decoder/encoder.  GTIRB will never peek below the bytes of a basic
block itself.  Examples of using GTIRB with the high quality freely
available
[Keystone](https://www.keystone-engine.org/)/[Capstone](https://www.capstone-engine.org/)
encoder/decoder will be provided in the GTIRB manual.

**Q: What platforms (OS/Compiler Combinations) will GTIRB be tested on?**

A: Ubuntu 16.04 with Clang 6.0.0 will be the initial development
environment for GTIRB.  It will also be tested on Windows 10 with
Visual Studio 2017.  Both compilers will have their full C++17 support
enabled.

## Development Questions:

(TBD)

## Testing Questions:

(TBD)

## User Questions:

(TBD)
