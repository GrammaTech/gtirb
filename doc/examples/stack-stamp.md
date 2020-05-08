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
  yay -Sy gtirb-git gtirb-pprinter-git ddisasm-git TODO:gtirb-functions-git TODO:gtirb-capstone-git
  ```

Note: if you're developing using Python or Common Lisp then you may
prefer to install the `gtirb-functions` and `gtirb-capstone` libraries
using your language's package manager.

- Python.

  ```
  pip install gtirb-functions-git gtirb-capstone-git
  ```

- Common Lisp.

  ```
  (mapc #'ql:quickload '(:gtirb-functions :gtirb-capstone))
  ```

## Lift a binary to GTIRB
Run the datalog disassembler to analyze a binary and produce a GTIRB
representation.

```
ddisasm $(which ls) --ir /tmp/ls.gtirb
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

Ghidra is a reverse engineering framework developed by the NSA. With a 
GTIRB plug-in, Ghidra offers a useful GUI for examining the differences
between GTIRB files.

To follow this procedure you must first download and install
[Ghidra](https://ghidra-sre.org/) and add the 
[GTIRB Ghidra plugin](https://github.com/GrammaTech/gtirb-ghidra-plugin).
Also, Java 11 is a prerequisite for Ghidra (here are installation instructions for
[Ubuntu](https://www.linuxbabe.com/ubuntu/install-oracle-java-8-openjdk-11-ubuntu-18-04-18-10) and
[Windows](https://access.redhat.com/documentation/en-us/openjdk/11/html/openjdk_11_for_windows_getting_started_guide/index))
                                                                                                                        
### Procedure         
                                                                                   
1. Import and analyze the files                                                   
2. Use the Version Tracking tool to match function locations
3. Examine the changes in a side-by-side view                    

                                                                                              
**Import and analyze the files**      

Start Ghidra and open a project or create a new one. Import ls.gtirb and double-click it to open a Code Browser.
When prompted to analyze it, hit "Yes" and select "Disassemble Entry Points" (only).
This will populate the listing with disassembly for all functions.
When the analysis is complete, save the file and close the Code Browser. Repeat for ls.ss.gtirb

**Use the Version Tracking tool to match function locations** 

Click on the "Footprints" icon to start the Version Tracking tool. The Version Tracking
tool also has a footprints icon, click on this to start a new session wizard. Enter a session
name and select the before and after files as Source and Destination. Skip the precondition checks and
hit Finish. This will open source and destination tools, which you can minimize as
we don't need them.

**Examine the changes in a side-by-side view**

In the Version Tracking tool, click the green "+" (plus sign) to start comparing the files.
In the wizard that comes up, select "Exact Symbol Name Match" (only), this will allow us to
do a side-by-side comparison of functions of the same name. Click Next and Finish, and the
Version Tracking Matches window will be populated with a list of matches. (If you don't see
a Version Tracking Matches window, go to Window in the top menu and select Version Tracking
Matches). Select a function by clicking on a row with type Function. Then go to the Version
Tracking Markup window to see a comparison of this function (if you don't see source and
destination sections in the Version Tracking Markup window, click the "Book" icon in the
upper right corner of the Version Tracking Markup window.
