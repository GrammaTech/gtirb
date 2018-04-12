GT-IRB Frequently Asked Questions
=================================

These are questions that have came up during design, development, testing, and
deployment of the GT-IRB.

## Design Questions:

**Q: Can an analysis for a binary compiled for _Platform A_ be completed by GT-
IRB tools running on _Platform B_?**

A:

**Q: Will the GT-IRB support 3rd Party binary to IR processors?**

A:

**Q: What platforms (OS/Compiler Combinations) will GT-IRB be tested on?**

A: OpenSuse 16.04 with Clang 6.0.0 will be the initial development environment for GT-IRB.  It will also be tested on Windows 10 with Visual Studio 2017.  Both compilers will have their full C++17 support enabled.

**Q: What API are external users going to be faced with?**

A: The existing back-end API is not going to be replaced.  There is a large investment in the back-end API from both an internal perspective and from our existing customer base.  That said, the existing documentation of the back-end API is not sufficient to support binary rewriting.  Development of the GT-IRB will be focused on providing an IR which is purpose-built for binary analysis and rewriting.  This would be, in turn, a new API to expose to customers.  Customers would see the GT-IRB as the binary analysis/rewriting IR, while the current back-end IR would not be modified and its API unaffected.

**Q: Will we be exporting a C++ API or a C API?**

A: GT-IRB will use a C++ API conforming to the ISO C++17 Standard.  

## Development Questions:

(TBD)

## Testing Questions:

(TBD)

## User Questions:

(TBD)
