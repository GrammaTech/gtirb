//===- SymbolicExpression.cpp -----------------------------------*- C++ -*-===//
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
#include "SymbolicExpression.hpp"
#include "Serialization.hpp"
#include <gtirb/Context.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/proto/SymbolicExpression.pb.h>
#include <variant>

namespace gtirb {

static void symAttributeSetToProtobuf(const SymAttributeSet& SASet,
                                      proto::SymbolicExpression* Message) {
  for (auto Attr : SASet) {
    Message->add_attribute_flags(static_cast<proto::SymAttribute>(Attr));
  }
}

static SymAttributeSet
symAttributeSetFromProtobuf(const proto::SymbolicExpression& Message) {
  SymAttributeSet SASet;
  for (int I = 0, E = Message.attribute_flags_size(); I != E; ++I) {
    SASet.insert(static_cast<SymAttribute>(Message.attribute_flags(I)));
  }
  return SASet;
}

class SymbolicVisitor {
public:
  proto::SymbolicExpression* Message;

  SymbolicVisitor(proto::SymbolicExpression* M) : Message(M) {}

  void operator()(const SymAddrConst& Val) const {
    auto M = Message->mutable_addr_const();
    M->set_offset(Val.Offset);
    if (Val.Sym) {
      uuidToBytes(Val.Sym->getUUID(), *M->mutable_symbol_uuid());
    }
    symAttributeSetToProtobuf(Val.Attributes, Message);
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
    symAttributeSetToProtobuf(Val.Attributes, Message);
  }
};

proto::SymbolicExpression toProtobuf(const SymbolicExpression& Value) {
  proto::SymbolicExpression Message;
  std::visit(SymbolicVisitor(&Message), Value);
  return Message;
}

namespace {
Symbol* symbolFromProto(Context& C, const std::string& Bytes) {
  if (UUID Id; !Bytes.empty() && uuidFromBytes(Bytes, Id))
    return dyn_cast_or_null<Symbol>(Node::getByUUID(C, Id));
  return nullptr;
}
} // namespace

bool fromProtobuf(Context& C, SymbolicExpression& Result,
                  const proto::SymbolicExpression& Message) {
  switch (Message.value_case()) {
  case proto::SymbolicExpression::kAddrConst: {
    const auto& Val = Message.addr_const();
    Result = SymAddrConst{Val.offset(), symbolFromProto(C, Val.symbol_uuid()),
                          symAttributeSetFromProtobuf(Message)};
    return std::get<SymAddrConst>(Result).Sym != nullptr;
  }
  case proto::SymbolicExpression::kAddrAddr: {
    const auto& Val = Message.addr_addr();
    Result = SymAddrAddr{Val.scale(), Val.offset(),
                         symbolFromProto(C, Val.symbol1_uuid()),
                         symbolFromProto(C, Val.symbol2_uuid()),
                         symAttributeSetFromProtobuf(Message)};
    return std::get<SymAddrAddr>(Result).Sym1 != nullptr &&
           std::get<SymAddrAddr>(Result).Sym2 != nullptr;
  }
  case proto::SymbolicExpression::VALUE_NOT_SET:
    assert(false && "unknown symbolic expression kind");
  }
  return false;
}

// This function is defined here w/ GTIRB_EXPORT_API to provide a
// means for test code to directly invoke serialization routines on a
// CFG. This is a capability not supported for GTIRB clients, but must
// be made available to the testing system.
void GTIRB_EXPORT_API symbolicExpressionSave(const SymbolicExpression& SE,
                                             std::ostream& Out) {
  proto::SymbolicExpression Message = toProtobuf(SE);
  Message.SerializeToOstream(&Out);
}

// This function is defined here w/ GTIRB_EXPORT_API to provide a
// means for test code to directly invoke serialization routines on a
// CFG. This is a capability not supported for GTIRB clients, but must
// be made available to the testing system.
void GTIRB_EXPORT_API symbolicExpressionLoad(Context& C,
                                             SymbolicExpression& Result,
                                             std::istream& In) {
  proto::SymbolicExpression Message;
  Message.ParseFromIstream(&In);
  fromProtobuf(C, Result, Message);
}

} // namespace gtirb
