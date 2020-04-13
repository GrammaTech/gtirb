GTIRB Stack Stamp
=================

TLDR; It is very easy to write binary transforms in GTIRB, see
[gtirb-stack-stamp](#FIXME) for a useful example.

This tutorial demonstrates the development of a binary hardening
transform built on GTIRB.  We will implement *stack stamping* (a
simple <abbr title="Return Oriented Programming">ROP</abbr> defense)
as a GTIRB-to-GTIRB transformation.  We will leverage the
[ddisasm](#FIXME) front-end to disassemble binaries and the
[gtirb-pprinter](#FIXME) back-end to produce a new hardened
executable.  In practice the stack-stamp transform could be chained
with other GTIRB binary analysis or transformation passes.  The
implementation of this transform is presented in all three GTIRB API
languages; [Python](#FIXME), [C++](#FIXME), and [Common Lisp](#FIXME).

- [Install Dependencies](#install-dependencies)
- [Lift a binary to GTIRB](#lift-a-binary-to-gtirb)
- [Implement the transform](#implement-the-transform)
- [Serialize GTIRB to a new executable and test](#serialize-gtirb-to-a-new-executable-and-test)
- [Visualize the difference using gtirb-ghidra-plugin](#visualize-the-difference-using-gtirb-ghidra-plugin)

## Install Dependencies
See [GTIRB#Install](#FIXME) for complete information on installing the
core debloat utilities including `ddisasm` and `gtirb-pprinter`.  In
addition the stack-stamp transform will require two supporting
libraries to add notions of functions ([gtirb-functions](#FIXME)) and
instructions ([gtirb-capstone](#FIXME)) to GTIRB.

The following should be sufficient to install all requirements:

- On Ubuntu,

  ```
  sudo add-apt-repository ppa:mhier/libboost-latest
  echo "deb [trusted=yes] https://grammatech.github.io/gtirb/ppa/bionic ./" | sudo tee -a /etc/apt/sources.list.d/gtirb.list
  sudo apt-get update
  sudo apt-get install gtirb gtirb-pprinter ddisasm gtirb-functions gtirb-capstone
  ```

- On Arch Linux using the popular [aur helper](https://wiki.archlinux.org/index.php/AUR_helpers)
  [yay](https://github.com/Jguer/yay),

  ```
  sudo add-apt-repository ppa:mhier/libboost-latest
  echo "deb [trusted=yes] https://grammatech.github.io/gtirb/ppa/bionic ./" | sudo tee -a /etc/apt/sources.list.d/gtirb.list
  sudo apt-get update
  sudo apt-get install gtirb gtirb-pprinter ddisasm gtirb-functions gtirb-capstone
  ```

- On Windows.

  ```
  FIXME
  ```

## Lift a binary to GTIRB
## Implement the transform
*Stack stamping* is a simple program transformation which encrypts the
return value on the top of the stack on function entry and decrypts
the value before function return.  This encryption is implemented as
an `xor` with a randomly generated key.

Figure

## Serialize GTIRB to a new executable and test

## Visualize the difference using gtirb-ghidra-plugin
