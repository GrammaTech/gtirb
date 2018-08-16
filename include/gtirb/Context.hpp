#ifndef GTIRB_CONTEXT_H
#define GTIRB_CONTEXT_H

#include <cstdlib>
#include <memory>

namespace gtirb {
class Context {
public:
  void *Allocate(size_t Size) const {
    return std::malloc(Size);
  }
};
} // namespace gtirb

inline void *operator new(size_t Size, const gtirb::Context &C) {
  return C.Allocate(Size);
}

inline void operator delete(void *, const gtirb::Context &C) {
  // Noop -- this is only called if the placement new using our Context object
  // throws; there is no way to call this directly.
}

#endif // GTIRB_CONTEXT_H

