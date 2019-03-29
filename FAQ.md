Frequently Asked Questions
--------------------------

- [I get compiler errors when I try to compile programs using GTIRB. How can I make them go away?](#compiler-errors)



#### Compiler Errors

__Q: I get compiler errors when I try to compile programs using GTIRB. How can I make them go away?__

A: GTIRB requires C++17, including the C++17 standard library. If your
compiler does not use C++17 by default, you will need to explicitly
specify it when compiling programs that use GTIRB.

For example:
```
g++ --std=c++17 my_gtirb_program.cpp -lgtirb -o my_gtirb_program
```
