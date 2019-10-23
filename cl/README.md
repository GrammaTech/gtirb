Common Lisp library for GTIRB
=============================

## Installation

Hopefully, eventually, it will be possible to install everything by
(1) installing
[Protobuf](https://developers.google.com/protocol-buffers/), version
3.0.0 or later, and then (2) installing this Common Lisp GTIRB library
with QuickLisp `(ql:quickload :gtirb)`.

We're a ways away from that currently.  So after you've installed
Protobuf, you should clone install the Common Lisp `PROTOBUF` package
according to the instructions at
[https://github.com/brown/protobuf](https://github.com/brown/protobuf).
(*Note*: currently you need my patched version at
[updates-enabling-build-and-install branch of github/eschulte/protobuf](https://github.com/eschulte/protobuf/tree/updates-enabling-build-and-install).)
Once you have installed the `protoc-gen-lisp` binary built from that
repository, you can load up the `GTIRB` package and everything should
work.  Load and test it as shown:

```
CL-USER> (ql:quickload :gtirb/test)
To load "gtirb/test":
  Load 1 ASDF system:
    gtirb/test
; Loading "gtirb/test"
..................................................
..................................................
[package gtirb/test].
(:GTIRB/TEST)
CL-USER> (gtirb/test:test)
...
#<test-run: 4 tests, 3 assertions, 0 failures in 0.101 sec>
CL-USER> 
```
