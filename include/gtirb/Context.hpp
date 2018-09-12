#ifndef GTIRB_CONTEXT_H
#define GTIRB_CONTEXT_H

#include <gtirb/Allocator.hpp>
#include <gtirb/Export.hpp>
#include <boost/uuid/uuid.hpp>
#include <cstdlib>
#include <map>

namespace gtirb {
using UUID = boost::uuids::uuid;

class Node;

/// \class Context
///
/// \brief The context under which GT-IRB operations occur. This object is
/// responsible for holding serialization and serialization state, allowing for
/// control over when Node memory can be released and providing a mechanism for
/// ensuring thread safety.
///
/// Any API that requires a \ref Context object may potentially allocate memory
/// within that context. Destroying the Context object will release that memory.
/// In a multithreaded environment, sharing a Context object across multiple
/// threads can introduce data races, so protecting the object with a locking
/// primitive is recommended.
class GTIRB_EXPORT_API Context {
  mutable BumpPtrAllocator Allocator;

  std::map<UUID, Node*> UuidMap;
  friend class Node;

  void registerNode(const UUID& ID, Node* N) { UuidMap[ID] = N; }

  void unregisterNode(const Node* N);
  const Node* findNode(const UUID& ID) const;
  Node* findNode(const UUID& ID);

public:
  /// \brief Allocates a chunk of memory of the given size with the given
  /// alignment requirements.
  ///
  /// \param Size   The amount of memory to allocate, in bytes.
  /// \param Align  The alignment required for the return pointer, in bytes.
  ///
  /// \return The newly allocated memory, suitably aligned for the given
  /// alignment. Will return nullptr if the allocation cannot be honored.
  void* Allocate(size_t Size, size_t Align) const {
    return Allocator.Allocate(Size, Align);
  }

  /// \brief Deallocates memory allocated through a call to Allocate().
  /// Deallocation of individual pointers leads to memory fragmentation, which
  /// is why this function is currently a placeholder that does not actually
  /// perform the deallocation. Instead, memory is freed as a whole when the
  /// \ref Context object is destroyed.
  ///
  /// \param <unnamed>
  /// \param <unnamed>
  ///
  /// \return void
  void Deallocate(void*, size_t) const {
    // Noop -- we don't want callers to deallocate individual allocations, but
    // should instead deallocate the entire Context object to free memory.
  }
};
} // namespace gtirb

/// \brief Custom placement new operator for performing allocations on a \ref
/// Context object.
///
/// \param Size   The amount of memory to allocate, in bytes.
/// \param C      The \ref Context object on which to perform the allocation.
/// \param Align  The alignment required for the returned pointer.
///
/// \return The newly allocated memory, or nullptr on error.
inline void* operator new(size_t Size, const gtirb::Context& C,
                          size_t Align = 8) {
  return C.Allocate(Size, Align);
}

/// \brief Custom placement delete operator that pairs with the custom new
/// operator. This operator cannot be called manually and is only called in the
/// event a constructor called via placement new terminates with an exception.
/// Context-allocated objects are expected to have non-throwing constructors and
/// so this function should never be called implicitly. However, some compilers
/// will diagnose definitions of placement new without a matching definition of
/// placement delete.
///
/// \param <unnamed>
/// \param <unnamed>
/// \param <unnamed>
///
/// \return void
inline void operator delete(void*, const gtirb::Context&, size_t) {
  // Noop -- this is only called if the placement new using our Context object
  // throws; there is no way to call this directly.
}

#endif // GTIRB_CONTEXT_H
