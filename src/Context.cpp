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
#include <gtirb/CodeBlock.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/ProxyBlock.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>

using namespace gtirb;

// By moving these declarations here, we avoid instantiating the default
// ctor/dtor in other compilation units which include Context.hpp, where some
// of the Node types may be incomplete.
Context::Context() = default;
Context::~Context() = default;

void Context::unregisterNode(const Node* N) { UuidMap.erase(N->getUUID()); }

const Node* Context::findNode(const UUID& ID) const {
  auto Iter = UuidMap.find(ID);
  return Iter != UuidMap.end() ? Iter->second : nullptr;
}

Node* Context::findNode(const UUID& ID) {
  auto Iter = UuidMap.find(ID);
  return Iter != UuidMap.end() ? Iter->second : nullptr;
}

template <> void* Context::Allocate<Node>() const {
  return NodeAllocator.Allocate();
}
template <> void* Context::Allocate<CodeBlock>() const {
  return BlockAllocator.Allocate();
}
template <> void* Context::Allocate<DataBlock>() const {
  return DataBlockAllocator.Allocate();
}
template <> void* Context::Allocate<IR>() const {
  return IrAllocator.Allocate();
}
template <> void* Context::Allocate<Module>() const {
  return ModuleAllocator.Allocate();
}
template <> void* Context::Allocate<ProxyBlock>() const {
  return ProxyBlockAllocator.Allocate();
}
template <> void* Context::Allocate<Section>() const {
  return SectionAllocator.Allocate();
}
template <> void* Context::Allocate<Symbol>() const {
  return SymbolAllocator.Allocate();
}
