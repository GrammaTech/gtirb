//===- SymbolicExpression.cpp -----------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018 GrammaTech, Inc.
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
#include "SymbolicExpression.hpp"
#include "Serialization.hpp"
#include <gtirb/Context.hpp>
#include <gtirb/Symbol.hpp>
#include <proto/SymbolicExpression.pb.h>
#include <variant>

namespace gtirb {
class SymbolicVisitor {
public:
  proto::SymbolicExpression* Message;

  SymbolicVisitor(proto::SymbolicExpression* M) : Message(M) {}

  void operator()(const SymStackConst& Val) const {
    auto M = Message->mutable_stack_const();
    M->set_negate(Val.Negate);
    M->set_offset(Val.Offset);
    M->set_displacement(Val.Displacement);
    if (Val.Sym) {
      uuidToBytes(Val.Sym->getUUID(), *M->mutable_symbol_uuid());
    }
  }

  void operator()(const SymAddrConst& Val) const {
    auto M = Message->mutable_addr_const();
    M->set_displacement(Val.Displacement);
    if (Val.Sym) {
      uuidToBytes(Val.Sym->getUUID(), *M->mutable_symbol_uuid());
    }
  }

  void operator()(const SymAddrAddr& Val) const {
    auto M = Message->mutable_addr_addr();
    M->set_scale(Val.Scale);
    M->set_offset(Val.Offset);
    if (Val.Sym1) {
      uuidToBytes(Val.Sym1->getUUID(), *M->mutable_symbol1_uuid());
    }
    if (Val.Sym2) {
      uuidToBytes(Val.Sym2->getUUID(), *M->mutable_symbol2_uuid());
    }
  }
};

proto::SymbolicExpression toProtobuf(const SymbolicExpression& Value) {
  proto::SymbolicExpression Message;
  std::visit(SymbolicVisitor(&Message), Value);
  return Message;
}

namespace {
Symbol* symbolFromProto(Context& C, std::string Bytes) {
  if (Bytes.empty()) {
    return nullptr;
  } else {
    return dyn_cast_or_null<Symbol>(Node::getByUUID(C, uuidFromBytes(Bytes)));
  }
}
} // namespace

void fromProtobuf(Context& C, SymbolicExpression& Result,
                  const proto::SymbolicExpression& Message) {
  switch (Message.value_case()) {
  case proto::SymbolicExpression::kStackConst: {
    auto Val = Message.stack_const();
    Result = SymStackConst{Val.negate(), Val.offset(), Val.displacement(),
                           symbolFromProto(C, Val.symbol_uuid())};
    break;
  }
  case proto::SymbolicExpression::kAddrConst: {
    auto Val = Message.addr_const();
    Result =
        SymAddrConst{Val.displacement(), symbolFromProto(C, Val.symbol_uuid())};
    break;
  }
  case proto::SymbolicExpression::kAddrAddr: {
    auto Val = Message.addr_addr();
    Result = SymAddrAddr{Val.scale(), Val.offset(),
                         symbolFromProto(C, Val.symbol1_uuid()),
                         symbolFromProto(C, Val.symbol2_uuid())};
    break;
  }
  case proto::SymbolicExpression::VALUE_NOT_SET:
    assert(false);
  }
}
} // namespace gtirb
