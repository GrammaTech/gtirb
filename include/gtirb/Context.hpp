//===- Context.hpp ----------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_CONTEXT_H
#define GTIRB_CONTEXT_H

#include <gtirb/Allocator.hpp>
#include <gtirb/Export.hpp>
#include <boost/uuid/uuid.hpp>
#include <cstdlib>
#include <map>

/// \file Context.hpp
/// \brief Class \ref gtirb::Context and related operators.

namespace gtirb {

/// \brief Represents a universally unique identifier used to identify Node
/// objects across serialization boundaries.
///
/// \see Node
/// \see Context
using UUID = boost::uuids::uuid;

class Node;
class CfgNode;
class ByteInterval;
class CodeBlock;
class DataBlock;
class IR;
class Module;
class ProxyBlock;
class Section;
class Symbol;

/// \class Context
///
/// \brief The context under which GTIRB operations occur.
///
/// This object is responsible for holding serialization and
/// serialization state, allowing for control over when Node memory
/// can be released and providing a mechanism for ensuring thread
/// safety.
///
/// Any API that requires a \ref Context object may potentially
/// allocate memory within that context. Destroying the Context object
/// will release that memory.  In a multithreaded environment, sharing
/// a Context object across multiple threads can introduce data races,
/// so protecting the object with a locking primitive is recommended.
class GTIRB_EXPORT_API Context {
  // Note: this must be declared first so it outlives the allocators. They
  // will access the UuidMap during their destructors to unregister nodes.
  std::map<UUID, Node*> UuidMap;

  // Allocate each node type in a separate arena.
  mutable SpecificBumpPtrAllocator<Node> NodeAllocator;
  mutable SpecificBumpPtrAllocator<ByteInterval> ByteIntervalAllocator;
  mutable SpecificBumpPtrAllocator<CodeBlock> BlockAllocator;
  mutable SpecificBumpPtrAllocator<DataBlock> DataBlockAllocator;
  mutable SpecificBumpPtrAllocator<IR> IrAllocator;
  mutable SpecificBumpPtrAllocator<Module> ModuleAllocator;
  mutable SpecificBumpPtrAllocator<ProxyBlock> ProxyBlockAllocator;
  mutable SpecificBumpPtrAllocator<Section> SectionAllocator;
  mutable SpecificBumpPtrAllocator<Symbol> SymbolAllocator;

  /// \copybrief gtirb::Node
  friend class Node;

  void registerNode(const UUID& ID, Node* N) { UuidMap[ID] = N; }

  void unregisterNode(const Node* N);
  const Node* findNode(const UUID& ID) const;
  Node* findNode(const UUID& ID);

  /// \brief Allocates a chunk of memory for an object of type \ref T.
  ///
  /// \tparam T   The type of object for which to allocate memory.
  ///
  /// \return The newly allocated memory, suitably sized for the given
  /// type. Will return nullptr if the allocation cannot be honored.
  template <class T> void* Allocate() const;

  /// \brief Deallocates memory allocated through a call to Allocate().
  ///
  /// \return void
  ///
  /// Deallocation of individual pointers leads to memory fragmentation, which
  /// is why this function is currently a placeholder that does not actually
  /// perform the deallocation. Instead, memory is freed as a whole when the
  /// \ref Context object is destroyed.
  void Deallocate(void*, size_t) const {
    // Noop -- we don't want callers to deallocate individual allocations, but
    // should instead deallocate the entire Context object to free memory.
  }

public:
  Context();
  ~Context();

  /// \brief Create an object of type \ref T.
  ///
  /// \tparam NodeTy   The type of object for which to allocate memory.
  /// \tparam Args     The types of the constructor arguments.
  /// \param TheArgs   The constructor arguments.
  ///
  /// \return A newly created object, allocated within the Context.
  template <typename NodeTy, typename... Args>
  NodeTy* Create(Args&&... TheArgs) {
    return new (Allocate<NodeTy>()) NodeTy(std::forward<Args>(TheArgs)...);
  }
};

template <> GTIRB_EXPORT_API void* Context::Allocate<Node>() const;
template <> GTIRB_EXPORT_API void* Context::Allocate<ByteInterval>() const;
template <> GTIRB_EXPORT_API void* Context::Allocate<CodeBlock>() const;
template <> GTIRB_EXPORT_API void* Context::Allocate<DataBlock>() const;
template <> GTIRB_EXPORT_API void* Context::Allocate<IR>() const;
template <> GTIRB_EXPORT_API void* Context::Allocate<Module>() const;
template <> GTIRB_EXPORT_API void* Context::Allocate<ProxyBlock>() const;
template <> GTIRB_EXPORT_API void* Context::Allocate<Section>() const;
template <> GTIRB_EXPORT_API void* Context::Allocate<Symbol>() const;

} // namespace gtirb

#endif // GTIRB_CONTEXT_H
