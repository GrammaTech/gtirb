//===- TestInputBinary.cpp --------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2020 GrammaTech, Inc.
//
//  This code is licensed under the MIT license. See the LICENSE file in the
//  project root for license terms.
//
//  This project is sponsored by the Office of Naval Research, One Liberty
//  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
//  N68335-17-C-0700.  The content of the information does not necessarily
//  reflect the position or policy of the Government and no official
//  endorsement should be inferred.
//
//===----------------------------------------------------------------------===//
///
/// \file 		TestInputBinary.cpp
///
/// References:
/// 	http://www.boost.org/doc/libs/1_66_0/libs/preprocessor/doc/index.html
///
/// Define GENERATION_CONSTANT to be between 1 and 65536.
///

#include <array>
#include <cstdint>
#include <cstdlib>
#include <exception>
#include <iostream>
#include <stdio.h>
#include <string>

/// Compute a factorial
int64_t Factorial(int64_t x) {
  if (x <= 0) {
    return 1;
  } else {
    return x * Factorial(x - 1);
  }
}

std::string IndirectFoo() { return "Foo"; }

std::string IndirectBar() { return "Bar"; }

const char* Index8[] = {"one",  "two", "three", "four",
                        "five", "six", "seven", "eight"};

template <int N, int I> class GeneratedClass {
public:
  GeneratedClass() { this->setName("GeneratedClass"); }

  /// Template function to print an integer.
  void printInteger() {
    std::cout << "Class<" << N << ", " << I << "> ";
    std::cout << "Factorial: !" << I << " = " << Factorial(I) << "; ";
    std::cout << "Throws: " << this->randomThrow() << "; ";
    std::cout << "Index: " << this->randomIndex() << "; ";
    std::cout << "Indirect: " << this->randomIndirect() << "; ";
    std::cout << "Member: " << this->member << "; ";
    std::cout << std::endl;
  }

  bool randomThrow() {
    this->setName("VOID");
    this->member += rand();

    try {
      if (rand() % 256 == 0) {
        throw std::logic_error("Ramdom Throw");
      }
    } catch (...) {
      return true;
    }

    return false;
  }

  std::string randomIndex() const { return std::string(Index8[rand() % 8]); }

  std::string randomIndirect() {
    this->member -= rand();

    if (rand() % 2 == 0) {
      return IndirectFoo();
    }

    return IndirectBar();
  }

  void operator()() {
    this->printInteger();

    // Generate small binaries.
    GeneratedClass<N - 1, I>()();

    // Generate much bigger binaries
    // GeneratedClass<N - 1, I + N>()();
    // GeneratedClass<N - 1, I - N>()();
  }

  void setName(const std::string& x) { this->name = x; }

private:
  std::string name;
  int64_t member{0};
};

// The "Exit" case for macro expansion.
template <int I> class GeneratedClass<0, I> {
public:
  void operator()() {
    // Empty
  }
};

int main() { GeneratedClass<GENERATION_CONSTANT, 0>()(); }
