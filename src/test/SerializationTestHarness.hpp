//===- SerializationTestHarness.hpp -----------------------------*- C++ -*-===//
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

#ifndef SERIALIZATION_TEST_HARNESS_HPP
#define SERIALIZATION_TEST_HARNESS_HPP

#include <gtirb/ByteInterval.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <iostream>

namespace gtirb {

// This is a utility class that allows unit tests to access the
// private serialization primitives for individual GTIRB classes. All
// GTIRB classes be-friend this class so they don't have to be-friend
// all the individual unit tests.
//
// Note: this class accesses the save/load member functions in each
// GTIRB class, not the to/fromProtobuf functions, as we do not want
// protobuf Message instances allocated here or in the unit test
// code. If they were allocated here, we would have problems on
// Windows due to protobuf not being dllexported in the gtirb.so
// library.
class SerializationTestHarness {
public:
  template <typename T> static void save(const T& Val, std::ostream& Out) {
    Val.save(Out);
  }

  template <typename T> static auto load(Context& C, std::istream& In) {
    return T::load(C, In);
  }

  template <typename T, typename P>
  static auto load(Context& C, P* Parent, std::istream& In) {
    return T::load(C, Parent, In);
  }

  static void byteIntervalLoadSymbolicExpressions(Context& Ctx,
                                                  ByteInterval& BI,
                                                  std::istream& In) {
    BI.loadSymbolicExpressions(Ctx, In);
  }
};

// Serializaton for CFGs, for which there isn't a class-level protobuf
// concept in the C++ API.
void GTIRB_EXPORT_API cfgSave(const CFG& Cfg, std::ostream& Out);
void GTIRB_EXPORT_API cfgLoad(Context& C, CFG& Result, std::istream& In);

// Serialization for SymbolicExpressions
void GTIRB_EXPORT_API symbolicExpressionSave(const SymbolicExpression& SE,
                                             std::ostream& Out);
void GTIRB_EXPORT_API symbolicExpressionLoad(Context& C,
                                             SymbolicExpression& Result,
                                             std::istream& In);

} // namespace gtirb

#endif // SERIALIZATION_TEST_HARNESS_HPP
