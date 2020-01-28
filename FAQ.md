Frequently Asked Questions             {#FAQ}
--------------------------

- [I get compiler errors when I try to compile programs using GTIRB. How can I make them go away?](#compiler-errors)

- [Linking error with "undefined reference" to gtirb::](#linking-error-with-undefined-reference-to-gtirb)

#### Compiler Errors

__Q: I get compiler errors when I try to compile programs using GTIRB. How can I make them go away?__

A: GTIRB requires C++17, including the C++17 standard library. If your
compiler does not use C++17 by default, you will need to explicitly
specify it when compiling programs that use GTIRB.

For example:
```
g++ --std=c++17 my_gtirb_program.cpp -lgtirb -o my_gtirb_program
```


#### Linking error with "undefined reference" to gtirb

__Q: I get linker errors when I try to compile and link programs using GTIRB. How can I make them go away?__

A: This isn't GTIRB specific, but if you place the `-lgtirb` on your
compilation line *before* the source file the linker will sometimes
throw away the symbols from the GTIRB library which it doesn't think
it needs (and if it hasn't read your source yet it won't think it
needs much).  So if for example,

```
g++ --std=c++17 -lgtirb my_gtirb_program.cpp
```

doesn't work for you, then try this instead.

```
g++ --std=c++17 my_gtirb_program.cpp -lgtirb
```
