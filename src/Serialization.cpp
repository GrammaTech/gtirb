//===- Serialization.cpp ----------------------------------------*- C++ -*-===//
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
#include "Serialization.hpp"
#include <gtirb/Node.hpp>

namespace gtirb {
bool uuidFromBytes(const std::string& Bytes, UUID& Uuid) {
  if (Bytes.size() == sizeof(UUID::data)) {
    std::copy(Bytes.begin(), Bytes.end(), std::begin(Uuid.data));
    return true;
  }
  assert(false && "Incorrect number of UUID bytes passed in");
  return false;
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

bool setNodeUUIDFromBytes(Node* Node, const std::string& Bytes) {
  if (UUID Id; uuidFromBytes(Bytes, Id)) {
    Node->setUUID(Id);
    return true;
  }
  return false;
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

bool fromProtobuf(Context&, Addr& Result, const uint64_t& Message) {
  Result = Addr(Message);
  return true;
}

bool fromProtobuf(Context&, UUID& Result, const std::string& Message) {
  return uuidFromBytes(Message, Result);
}

} // namespace gtirb
