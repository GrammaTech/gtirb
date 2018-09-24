//===- Context.cpp ----------------------------------------------*- C++ -*-===//
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
#include "Context.hpp"
#include <gtirb/Node.hpp>

using namespace gtirb;

void Context::unregisterNode(const Node* N) { UuidMap.erase(N->getUUID()); }

const Node* Context::findNode(const UUID& ID) const {
  auto Iter = UuidMap.find(ID);
  return Iter != UuidMap.end() ? Iter->second : nullptr;
}

Node* Context::findNode(const UUID& ID) {
  auto Iter = UuidMap.find(ID);
  return Iter != UuidMap.end() ? Iter->second : nullptr;
}
