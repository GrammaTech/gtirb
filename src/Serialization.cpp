//===- Serialization.cpp ----------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018-2019 GrammaTech, Inc.
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
#include "Serialization.hpp"
#include <gtirb/Node.hpp>

namespace gtirb {
UUID uuidFromBytes(const std::string& Bytes) {
  UUID Id;
  assert(Bytes.size() == sizeof(Id.data));
  std::copy(Bytes.begin(), Bytes.end(), std::begin(Id.data));
  return Id;
}

void uuidToBytes(UUID Uuid, std::string& Bytes) {
  Bytes.clear();
  Bytes.reserve(sizeof(Uuid.data));
  std::copy(std::begin(Uuid.data), std::end(Uuid.data),
            std::back_inserter(Bytes));
}

void nodeUUIDToBytes(const Node* Node, std::string& Bytes) {
  uuidToBytes(Node->getUUID(), Bytes);
}

void setNodeUUIDFromBytes(Node* Node, const std::string& Bytes) {
  Node->setUUID(uuidFromBytes(Bytes));
}

uint64_t toProtobuf(const Addr Val) { return static_cast<uint64_t>(Val); }

std::string toProtobuf(const std::string& Val) { return Val; }

int64_t toProtobuf(const int64_t& Val) { return Val; }

uint64_t toProtobuf(const uint64_t& Val) { return Val; }

std::string toProtobuf(const UUID& Val) {
  std::string Result;
  uuidToBytes(Val, Result);
  return Result;
}

void fromProtobuf(Context&, Addr& Result, const uint64_t& Message) {
  Result = Addr(Message);
}

void fromProtobuf(Context&, UUID& Result, const std::string& Message) {
  Result = uuidFromBytes(Message);
}

} // namespace gtirb
