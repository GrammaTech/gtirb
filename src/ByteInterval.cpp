//===- ByteInterval.cpp -----------------------------------------*- C++ -*-===//
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
#include <gtirb/ByteInterval.hpp>
#include <proto/ByteInterval.pb.h>

using namespace gtirb;

struct GetSymExprOffsetVisitor {
  template <class T> uint64_t operator()(const T& symExpr) {
    return symExpr.Offset;
  }
};

struct GetBlockOffsetVisitor {
  uint64_t operator()(const CodeBlock* block) { return block->getOffset(); }
  uint64_t operator()(const DataBlock* block) { return block->getOffset(); }
  uint64_t operator()(const SymbolicExpression* block) {
    return std::visit(GetSymExprOffsetVisitor{}, *block);
  }
};

uint64_t gtirb::getBlockOffset(const Block& B) {
  return std::visit(GetBlockOffsetVisitor{}, B);
}

void ByteInterval::toProtobuf(MessageType* Message) const {
  if (Address.has_value()) {
    Message->set_has_address(true);
    Message->set_address((uint64_t)*Address);
  } else {
    Message->set_has_address(false);
  }

  Message->set_size(Size);
}

ByteInterval* ByteInterval::fromProtobuf(Context& C,
                                         const MessageType& Message) {
  auto addr = Message.has_address() ? std::optional<Addr>{Message.address()}
                                    : std::optional<Addr>{};
  ByteInterval* result = ByteInterval::Create(C, addr, Message.size());
  return result;
}
