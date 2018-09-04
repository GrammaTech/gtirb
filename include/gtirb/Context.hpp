#ifndef GTIRB_CONTEXT_H
#define GTIRB_CONTEXT_H

#include <gtirb/Allocator.hpp>
#include <cstdlib>
#include <memory>

namespace gtirb {
class Context {
  mutable BumpPtrAllocator Allocator;

public:
  void* Allocate(size_t Size, size_t Align) const {
    return Allocator.Allocate(Size, Align);
  }

  void Deallocate(void* Ptr, size_t Size) const {
    // Noop -- we don't want callers to deallocate individual allocations, but
    // should instead deallocate the entire Context object to free memory.
  }
};
} // namespace gtirb

inline void* operator new(size_t Size, const gtirb::Context& C,
                          size_t Align = 8) {
  return C.Allocate(Size, Align);
}

inline void operator delete(void*, const gtirb::Context&, size_t) {
  // Noop -- this is only called if the placement new using our Context object
  // throws; there is no way to call this directly.
}

#endif // GTIRB_CONTEXT_H
