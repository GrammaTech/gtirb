GTIRB Stack Stamp
=================

> **WIP**: this tutorial is a **Work in Progress**

TLDR; It is very easy to write binary transforms in GTIRB, see
[gtirb-stack-stamp](https://github.com/grammatech/gtirb-stack-stamp).

This tutorial demonstrates the development of a binary hardening
transform built on GTIRB.  We will implement *stack stamping* (a
simple <abbr title="Return Oriented Programming">ROP</abbr> defense)
as a GTIRB-to-GTIRB transformation.  We will leverage the
[ddisasm](https://github.com/grammatech/ddisasm) front-end to
disassemble binaries and the
[gtirb-pprinter](https://github.com/grammatech/gtirb-pprinter)
back-end to produce a new hardened executable.  In practice the
stack-stamp transform could be chained with other GTIRB binary
analysis or transformation passes.  The implementation of this
transform is presented in all three GTIRB API languages;
[Python](https://grammatech.github.io/gtirb/python/index.html),
[C++](https://grammatech.github.io/gtirb/cpp/index.html), and
[Common Lisp](https://grammatech.github.io/gtirb/cl/index.html).

- [Install Dependencies](#install-dependencies)
- [Lift a binary to GTIRB](#lift-a-binary-to-gtirb)
- [Implement the transform](#implement-the-transform)
- [Serialize GTIRB to a new executable and test](#serialize-gtirb-to-a-new-executable-and-test)
- [Visualize the difference using gtirb-ghidra-plugin](#visualize-the-difference-using-gtirb-ghidra-plugin)

## Install Dependencies
See [GTIRB#Install](https://github.com/grammatech/gtirb#installing)
for complete information on installing the core debloat utilities
including `ddisasm` and `gtirb-pprinter`.  In addition the stack-stamp
transform will require two supporting libraries to add notions of
functions
([gtirb-functions](https://github.com/grammatech/gtirb-functions)) and
instructions
([gtirb-capstone](https://github.com/grammatech/gtirb-capstone)) to
GTIRB.

The following should be sufficient to install all requirements:

- On Windows.

  ```
  FIXME
  ```

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
  yay -Sy gtirb-git gtirb-pprinter-git ddisasm-git gtirb-functions-git gtirb-capstone-git
  ```

## Lift a binary to GTIRB
Run the datalog disassembler to analyze a binary and produce a GTIRB
representation.

```
ddisasm $(which ls) /tmp/ls.gtirb
```

If a binary on your system doesn't work, please
[let us know](https://github.com/GrammaTech/ddisasm/issues/new).

## Implement the transform
Stack stamping is a technique to help mitigate ROP style attacks.
This is done by 'stamping' the return address on the stack, thus
encrypting it.  Before it is popped off the stack and used, it is
decrypted by 'unstamping' it.  This can be an efficient protection, as
no registers are needed, and while flags are affected, they are only
affected at function entry/exits where they do not need to be
preserved.  By encoding and decoding this return address, an attacker
has a more difficult task, since the replacement data would need to be
properly encoded, such that when it is unstamped, it results in the
desired address.

![Stack Stamp Figure](.stack-stamp.svg)

### Implement this pass
Regardless of the implementation language the mechanics of this
transform will be the same.

1. For those functions which have a single entry and return.
2. Build a random key for that function.
3. Encrypt the return address on entry to the function using this key.
4. Decrypt the return address on exit from the function using this key.

Try to write this yourself using the [GTIRB
manual](https://grammatech.github.io/gtirb/) as a reference.  When
you're done you can compare to the completed transforms implemented in
each language at
[Python](https://github.com/GrammaTech/gtirb-stack-stamp/blob/master/gtirb_stack_stamp/stack_stamp.py#L36),
[C++](#FIXME), and
[Common Lisp](https://github.com/GrammaTech/gtirb-stack-stamp/blob/master/gtirb-stack-stamp.lisp#L24).

## Serialize GTIRB to a new executable and test
Run the GTIRB pretty printer to serialize a GTIRB representation to a
new binary.

```
gtirb-pprinter /tmp/ls.ss.gtirb --skip-section .eh_frame \
               --asm /tmp/ls.ss.s \
               --binary /tmp/ls.ss
```

## Visualize the difference using gtirb-ghidra-plugin

> **TODO**.
