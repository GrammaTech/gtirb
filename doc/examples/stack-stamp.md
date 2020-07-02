GTIRB Stack Stamp
=================

> **WIP**: this tutorial is a **Work in Progress**

TLDR; It is very easy to write binary transforms in GTIRB, see
[gtirb-stack-stamp](https://github.com/grammatech/gtirb-stack-stamp).

This tutorial demonstrates the development of a binary hardening
transform built on GTIRB.  We implement *stack stamping* (a simple
<abbr title="Return Oriented Programming">ROP</abbr> defense) as a
GTIRB-to-GTIRB transformation.  We leverage the
[ddisasm](https://github.com/grammatech/ddisasm) front-end to
disassemble binaries to GTIRB and the
[gtirb-pprinter](https://github.com/grammatech/gtirb-pprinter)
back-end to produce a new hardened executable from the stack stamped
GTIRB.  In practice the stack-stamp transform could be chained with
other GTIRB binary analysis or transformation passes.  Implementations
of the stack stamping transform are given in all three GTIRB API
languages;
Python [API](https://grammatech.github.io/gtirb/python/index.html)/[stack_stamp.py](https://github.com/GrammaTech/gtirb-stack-stamp/blob/master/gtirb_stack_stamp/stack_stamp.py),
C++ [API](https://grammatech.github.io/gtirb/cpp/index.html)/[gtirb_stack_stamp.hpp](https://github.com/GrammaTech/gtirb-stack-stamp/blob/master/include/gtirb_stack_stamp.hpp), and
Common Lisp [API](https://grammatech.github.io/gtirb/cl/index.html)/[gtirb-stack-stamp.lisp](https://github.com/GrammaTech/gtirb-stack-stamp/blob/master/gtirb-stack-stamp.lisp).

This document walks through the whole process of writing and applying
the *stack stamping* binary ROP protection in following steps:

1. [Install Dependencies](#install-dependencies)
2. [Lift a binary to GTIRB](#lift-a-binary-to-gtirb)
3. [Implement the transform](#implement-the-transform)
4. [Serialize GTIRB to a new executable and test](#serialize-gtirb-to-a-new-executable-and-test)
5. [Visualize the difference using the gtirb-ghidra-plugin](#visualize-the-difference-using-gtirb-ghidra-plugin)
6. Let us know what you think.  You can open an issue against
   [github.com/grammatech/gtirb](https://github.com/grammatech/gtirb)
   or email us at `gtirb@grammatech.com`.


## 1. Install Dependencies

The following should be sufficient to install the required GTIRB
libraries and utilities (for complete installation instructions see
[GTIRB#Install](https://github.com/grammatech/gtirb#installing)):

- On Windows grab the binaries from
  [https://grammatech.github.io/gtirb/pkgs/windows-release](https://grammatech.github.io/gtirb/pkgs/windows-release).

- On Ubuntu,

  ```
  sudo add-apt-repository ppa:mhier/libboost-latest
  echo "deb [trusted=yes] https://grammatech.github.io/gtirb/ppa/bionic ./" | sudo tee -a /etc/apt/sources.list.d/gtirb.list
  sudo apt-get update
  sudo apt-get install gtirb gtirb-pprinter ddisasm gtirb-functions gtirb-capstone
  ```

- On Arch Linux install pre-built `pacman` packages from
  [https://grammatech.github.io/gtirb/pkgs/arch](https://grammatech.github.io/gtirb/pkgs/arch)
  or install using the popular [aur
  helper](https://wiki.archlinux.org/index.php/AUR_helpers)
  [yay](https://github.com/Jguer/yay),

  ```
  yay -Sy gtirb-git gtirb-pprinter-git ddisasm-git gtirb-functions-git gtirb-capstone-git
  ```

If you're developing using the Python or Common Lisp APIs then you may
prefer to install the (i.e., 'gtirb', `gtirb-functions` and
`gtirb-capstone`) using your language's package manager.  You'll still
need the `ddisasm` and `gtirb-pprinter` executables installed above.

- Python.

  ```
  pip3 install gtirb gtirb-functions gtirb-capstone
  ```

- Common Lisp.

  ```
  (ql:quickload '(:gtirb :gtirb-functions :gtirb-capstone))
  ```


## 2. Lift a binary to GTIRB

Run the datalog disassembler to analyze a binary and produce a GTIRB
representation.

```
ddisasm $(which ls) --ir /tmp/ls.gtirb
```

If a binary on your system doesn't work, please
[open an issue](https://github.com/GrammaTech/ddisasm/issues/new)
to let us know.

## Implement the transform

Stack stamping is a technique to help mitigate ROP style attacks.
This is done by 'stamping' (`xor`ing with a random number) the return
address on the stack at the beginning of every function, thus
encrypting it.  At the end of the function, before the return address
is popped off the stack and used, it is decrypted by `xor`ing it again
with the same random number.  This can be a very efficient protection.
Because it only requires an `xor` instruction this implementation
requires no registers, and while flags are affected, they are only
affected at function entry/exits where they do not need to be
preserved.  The effect of encrypting and decrypting the return address
on the stack like this is that ROP payloads become much more difficult
to write.  The attacker would have to know the random `xor` number for
every return to encrypt the return addresses in the payload.  These
numbers could easily be regenerated for every instance of a deployed
binary making generic payloads impossible.

![Stack Stamp Figure](.stack-stamp.svg)

Regardless of the implementation language the mechanics of this
transform will be the same--we'll write a GTIRB-to-GTIRB rewriting
pass (the design of GTIRB is similar to LLVM in that it leverages
stand-alone passes for analysis or transformation).

1. For those functions which have a single entry and return.
2. Build a random key for that function.
3. Encrypt the return address on entry to the function using this key.
4. Decrypt the return address on exit from the function using this key.

Try to write this yourself using the [GTIRB
manual](https://grammatech.github.io/gtirb/) as a reference.  When
you're done you can compare to the completed transforms implemented in
each language at
[Python](https://github.com/GrammaTech/gtirb-stack-stamp/blob/master/gtirb_stack_stamp/stack_stamp.py#L36),
[C++](https://github.com/GrammaTech/gtirb-stack-stamp/blob/master/src/gtirb_stack_stamp.cpp), and
[Common Lisp](https://github.com/GrammaTech/gtirb-stack-stamp/blob/master/gtirb-stack-stamp.lisp#L24).

If you're developing in Python or Common Lisp you should first start
up a <abbr title="Read Eval Print Loop">REPL</abbr> and load the your
GTIRB instance. To load an IR from file:

- Python
  ```python
  ir = IR.load_protobuf("/tmp/ls.gtirb")
  ```

- C++
  ```c++
  gtirb::Context Ctx;
  std::ifstream File("/tmp/ls.gtirb");
  gtirb::IR* Ir = *gtirb::IR::load(Ctx, File);
  ```

- Common Lisp
  ```lisp
  (defparameter *ir* (read-gtirb "/tmp/ls.gtirb"))
  ```

You can then develop and apply the transform.  When
you're done, serialize the resulting GTIRB to a new file:

- Python
  ```python
  ir.save_protobuf("/tmp/ls-ss.gtirb")
  ```

- C++
  ```c++
  std::ofstream File("/tmp/ls-ss.gtirb");
  Ir->save(File);
  ```

- Common Lisp
  ```lisp
  (write-gtirb *ir* "/tmp/ls-ss.gtirb")
  ```


## 3. Serialize GTIRB to a new executable and test

Finally, you can run the GTIRB pretty printer to serialize your GTIRB
representation to a new binary.

```
gtirb-pprinter /tmp/ls-ss.gtirb --skip-section .eh_frame \
               --asm /tmp/ls.ss.s \
               --binary /tmp/ls.ss
```

Test out the resulting binary, it should be indistinguishable from the
original.


## 4. Visualize the difference using gtirb-ghidra-plugin

Ghidra is a reverse engineering framework developed by the NSA. With a
GTIRB plug-in, Ghidra offers a useful GUI for examining the
differences between GTIRB files.  To follow the rest of this example
you must first download and install
[Ghidra](https://ghidra-sre.org/) and add the
[GTIRB Ghidra plugin](https://github.com/GrammaTech/gtirb-ghidra-plugin).
Also, Java 11 is a prerequisite for Ghidra (here are installation instructions for
[Ubuntu](https://www.linuxbabe.com/ubuntu/install-oracle-java-8-openjdk-11-ubuntu-18-04-18-10) and
[Windows](https://access.redhat.com/documentation/en-us/openjdk/11/html/openjdk_11_for_windows_getting_started_guide/index))


Procedure:

1. Import and analyze the files
2. Use the Version Tracking tool to match function locations
3. Examine the changes in a side-by-side view


#### 1. Import and analyze the files

Start Ghidra and open a project or create a new one. Import `ls.gtirb`
and double-click it to open a Code Browser.  When prompted to analyze
it, hit "Yes" and select "Disassemble Entry Points" (only).  This will
populate the listing with disassembly for all functions.  When the
analysis is complete, save the file and close the Code Browser. Repeat
for `ls-ss.gtirb`

#### 2. Use the Version Tracking tool to match function locations

Click on the "Footprints" icon to start the Version Tracking tool. The
Version Tracking tool also has a footprints icon, click on this to
start a new session wizard. Enter a session name and select the before
and after files as Source and Destination. Skip the precondition
checks and hit Finish. This will open source and destination tools,
which you can minimize as we don't need them.

#### 3. Examine the changes in a side-by-side view

In the Version Tracking tool, click the green "+" (plus sign) to start
comparing the files.  In the wizard that comes up, select "Exact
Symbol Name Match" (only), this will allow us to do a side-by-side
comparison of functions of the same name. Click Next and Finish, and
the Version Tracking Matches window will be populated with a list of
matches. (If you don't see a Version Tracking Matches window, go to
Window in the top menu and select Version Tracking Matches). Select a
function by clicking on a row with type Function. Then go to the
Version Tracking Markup window to see a comparison of this function
(if you don't see source and destination sections in the Version
Tracking Markup window, click the "Book" icon in the upper right
corner of the Version Tracking Markup window.
