//===- Allocator.hpp --------------------------------------------*- C++ -*-===//
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
//===-Addition License Information-----------------------------------------===//
//
// This file was initially written for the LLVM Compiler Infrastructure
// project where it is distributed under the University of Illinois Open Source
// License. See the LICENSE file in the project root for license terms.
//
//===----------------------------------------------------------------------===//
#ifndef GTIRB_ALLOCATOR_H
#define GTIRB_ALLOCATOR_H

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <iterator>
#include <utility>
#include <vector>

namespace gtirb {
/// Returns the next power of two (in 64-bits) that is strictly greater than A.
/// Returns zero on overflow.
inline uint64_t NextPowerOf2(uint64_t A) {
  A |= (A >> 1);
  A |= (A >> 2);
  A |= (A >> 4);
  A |= (A >> 8);
  A |= (A >> 16);
  A |= (A >> 32);
  return A + 1;
}

/// Return true if the argument is a power of two > 0 (64 bit edition.)
constexpr inline bool isPowerOf2_64(uint64_t Value) {
  return Value && !(Value & (Value - 1));
}

/// Aligns \c Addr to \c Alignment bytes, rounding up.
///
/// Alignment should be a power of two.  This method rounds up, so
/// alignAddr(7, 4) == 8 and alignAddr(8, 4) == 8.
inline uintptr_t alignAddr(const void* Addr, size_t Alignment) {
  assert(Alignment && isPowerOf2_64((uint64_t)Alignment) &&
         "Alignment is not a power of two!");

  assert((uintptr_t)Addr + Alignment - 1 >= (uintptr_t)Addr);

  return (((uintptr_t)Addr + Alignment - 1) & ~(uintptr_t)(Alignment - 1));
}

/// Returns the necessary adjustment for aligning \c Ptr to \c Alignment
/// bytes, rounding up.
inline size_t alignmentAdjustment(const void* Ptr, size_t Alignment) {
  return alignAddr(Ptr, Alignment) - (uintptr_t)Ptr;
}

/// Allocate memory in an ever growing pool, as if by bump-pointer.
///
/// This isn't strictly a bump-pointer allocator as it uses backing slabs of
/// memory rather than relying on a boundless contiguous heap. However, it has
/// bump-pointer semantics in that it is a monotonically growing pool of memory
/// where every allocation is found by merely allocating the next N bytes in
/// the slab, or the next N bytes in the next slab.
///
/// Note that this also has a threshold for forcing allocations above a certain
/// size into their own slab.
template <size_t SlabSize = 4096, size_t SizeThreshold = SlabSize>
class BumpPtrAllocatorImpl {
public:
  static_assert(SizeThreshold <= SlabSize,
                "The SizeThreshold must be at most the SlabSize to ensure "
                "that objects larger than a slab go into their own memory "
                "allocation.");

  BumpPtrAllocatorImpl() = default;

  // Manually implement a move constructor as we must clear the old allocator's
  // slabs as a matter of correctness.
  BumpPtrAllocatorImpl(BumpPtrAllocatorImpl&& Old)
      : CurPtr(Old.CurPtr), End(Old.End), Slabs(std::move(Old.Slabs)),
        CustomSizedSlabs(std::move(Old.CustomSizedSlabs)),
        BytesAllocated(Old.BytesAllocated), RedZoneSize(Old.RedZoneSize) {
    Old.CurPtr = Old.End = nullptr;
    Old.BytesAllocated = 0;
    Old.Slabs.clear();
    Old.CustomSizedSlabs.clear();
  }

  ~BumpPtrAllocatorImpl() {
    DeallocateSlabs(Slabs.begin(), Slabs.end());
    DeallocateCustomSizedSlabs();
  }

  BumpPtrAllocatorImpl& operator=(BumpPtrAllocatorImpl&& RHS) {
    DeallocateSlabs(Slabs.begin(), Slabs.end());
    DeallocateCustomSizedSlabs();

    CurPtr = RHS.CurPtr;
    End = RHS.End;
    BytesAllocated = RHS.BytesAllocated;
    RedZoneSize = RHS.RedZoneSize;
    Slabs = std::move(RHS.Slabs);
    CustomSizedSlabs = std::move(RHS.CustomSizedSlabs);

    RHS.CurPtr = RHS.End = nullptr;
    RHS.BytesAllocated = 0;
    RHS.Slabs.clear();
    RHS.CustomSizedSlabs.clear();
    return *this;
  }

  /// Allocate space at the specified alignment.
  void* Allocate(size_t Size, size_t Alignment) {
    assert(Alignment > 0 && "0-byte alignnment is not allowed. Use 1 instead.");

    // Keep track of how many bytes we've allocated.
    BytesAllocated += Size;

    size_t Adjustment = alignmentAdjustment(CurPtr, Alignment);
    assert(Adjustment + Size >= Size && "Adjustment + Size must not overflow");

    size_t SizeToAllocate = Size;
    // #if LLVM_ADDRESS_SANITIZER_BUILD
    //     // Add trailing bytes as a "red zone" under ASan.
    //     SizeToAllocate += RedZoneSize;
    // #endif

    // Check if we have enough space.
    if (Adjustment + SizeToAllocate <= size_t(End - CurPtr)) {
      char* AlignedPtr = CurPtr + Adjustment;
      CurPtr = AlignedPtr + SizeToAllocate;
      // Update the allocation point of this memory block in MemorySanitizer.
      // Without this, MemorySanitizer messages for values originated from here
      // will point to the allocation of the entire slab.
      //      __msan_allocated_memory(AlignedPtr, Size);
      // Similarly, tell ASan about this space.
      //      __asan_unpoison_memory_region(AlignedPtr, Size);
      return AlignedPtr;
    }

    // If Size is really big, allocate a separate slab for it.
    size_t PaddedSize = SizeToAllocate + Alignment - 1;
    if (PaddedSize > SizeThreshold) {
      void* NewSlab = std::malloc(PaddedSize);
      // We own the new slab and don't want anyone reading anyting other than
      // pieces returned from this method.  So poison the whole slab.
      //      __asan_poison_memory_region(NewSlab, PaddedSize);
      CustomSizedSlabs.push_back(std::make_pair(NewSlab, PaddedSize));

      uintptr_t AlignedAddr = alignAddr(NewSlab, Alignment);
      assert(AlignedAddr + Size <= (uintptr_t)NewSlab + PaddedSize);
      char* AlignedPtr = (char*)AlignedAddr;
      //      __msan_allocated_memory(AlignedPtr, Size);
      //      __asan_unpoison_memory_region(AlignedPtr, Size);
      return AlignedPtr;
    }

    // Otherwise, start a new slab and try again.
    StartNewSlab();
    uintptr_t AlignedAddr = alignAddr(CurPtr, Alignment);
    assert(AlignedAddr + SizeToAllocate <= (uintptr_t)End &&
           "Unable to allocate memory!");
    char* AlignedPtr = (char*)AlignedAddr;
    CurPtr = AlignedPtr + SizeToAllocate;
    //    __msan_allocated_memory(AlignedPtr, Size);
    //    __asan_unpoison_memory_region(AlignedPtr, Size);
    return AlignedPtr;
  }

  /// Allocate space for a sequence of objects without constructing them.
  template <typename T> T* Allocate(size_t Num = 1) {
    return static_cast<T*>(Allocate(Num * sizeof(T), alignof(T)));
  }

  // Bump pointer allocators are expected to never free their storage; and
  // clients expect pointers to remain valid for non-dereferencing uses even
  // after deallocation.
  void Deallocate(const void*, size_t) {
    //    __asan_poison_memory_region(Ptr, Size);
  }

  size_t GetNumSlabs() const { return Slabs.size() + CustomSizedSlabs.size(); }

  size_t getTotalMemory() const {
    size_t TotalMemory = 0;
    for (auto I = Slabs.begin(), E = Slabs.end(); I != E; ++I)
      TotalMemory += computeSlabSize(std::distance(Slabs.begin(), I));
    for (auto& PtrAndSize : CustomSizedSlabs)
      TotalMemory += PtrAndSize.second;
    return TotalMemory;
  }

  size_t getBytesAllocated() const { return BytesAllocated; }

  void setRedZoneSize(size_t NewSize) { RedZoneSize = NewSize; }

private:
  /// The current pointer into the current slab.
  ///
  /// This points to the next free byte in the slab.
  char* CurPtr = nullptr;

  /// The end of the current slab.
  char* End = nullptr;

  /// The slabs allocated so far.
  std::vector<void*> Slabs;

  /// Custom-sized slabs allocated for too-large allocation requests.
  std::vector<std::pair<void*, size_t>> CustomSizedSlabs;

  /// How many bytes we've allocated.
  ///
  /// Used so that we can compute how much space was wasted.
  size_t BytesAllocated = 0;

  /// The number of bytes to put between allocations when running under
  /// a sanitizer.
  size_t RedZoneSize = 1;

  static size_t computeSlabSize(size_t SlabIdx) {
    // Scale the actual allocated slab size based on the number of slabs
    // allocated. Every 128 slabs allocated, we double the allocated size to
    // reduce allocation frequency, but saturate at multiplying the slab size by
    // 2^30.
    return SlabSize * ((size_t)1 << std::min<size_t>(30, SlabIdx / 128));
  }

  /// Allocate a new slab and move the bump pointers over into the new
  /// slab, modifying CurPtr and End.
  void StartNewSlab() {
    size_t AllocatedSlabSize = computeSlabSize(Slabs.size());

    void* NewSlab = std::malloc(AllocatedSlabSize);
    // We own the new slab and don't want anyone reading anything other than
    // pieces returned from this method.  So poison the whole slab.
    //    __asan_poison_memory_region(NewSlab, AllocatedSlabSize);

    Slabs.push_back(NewSlab);
    CurPtr = (char*)(NewSlab);
    End = ((char*)NewSlab) + AllocatedSlabSize;
  }

  /// Deallocate a sequence of slabs.
  void DeallocateSlabs(std::vector<void*>::iterator I,
                       std::vector<void*>::iterator E) {
    for (; I != E; ++I) {
      std::free(*I);
    }
  }

  /// Deallocate all memory for custom sized slabs.
  void DeallocateCustomSizedSlabs() {
    for (auto& PtrAndSize : CustomSizedSlabs) {
      std::free(PtrAndSize.first);
    }
  }

  template <typename T> friend class SpecificBumpPtrAllocator;
};

/// The standard BumpPtrAllocator which just uses the default template
/// parameters.
typedef BumpPtrAllocatorImpl<> BumpPtrAllocator;

/// A BumpPtrAllocator that allows only elements of a specific type to be
/// allocated.
///
/// This allows calling the destructor in DestroyAll() and when the allocator is
/// destroyed.
template <typename T> class SpecificBumpPtrAllocator {
  BumpPtrAllocator Allocator;

public:
  SpecificBumpPtrAllocator() {
    // Because SpecificBumpPtrAllocator walks the memory to call destructors,
    // it can't have red zones between allocations.
    Allocator.setRedZoneSize(0);
  }
  SpecificBumpPtrAllocator(SpecificBumpPtrAllocator&& Old)
      : Allocator(std::move(Old.Allocator)) {}
  ~SpecificBumpPtrAllocator() { DestroyAll(); }

  SpecificBumpPtrAllocator& operator=(SpecificBumpPtrAllocator&& RHS) {
    Allocator = std::move(RHS.Allocator);
    return *this;
  }

  /// Allocate space for an array of objects without constructing them.
  T* Allocate(size_t num = 1) { return Allocator.Allocate<T>(num); }

  /// Forgets all allocations from the underlying allocator, effectively
  /// leaking the memory. This is useful when the allocator is no longer needed
  /// and the operating system will be reclaiming the memory (such as at
  // program shutdown time).
  void ForgetAllocations() {
    Allocator.Slabs.clear();
    Allocator.CustomSizedSlabs.clear();
  }

private:
  /// Call the destructor of each allocated object and deallocate all but the
  /// current slab and reset the current pointer to the beginning of it, freeing
  /// all memory allocated so far.
  void DestroyAll() {
    auto DestroyElements = [](char* Begin, char* End) {
      assert(Begin == (char*)alignAddr(Begin, alignof(T)));
      for (char* Ptr = Begin; Ptr + sizeof(T) <= End; Ptr += sizeof(T))
        reinterpret_cast<T*>(Ptr)->~T();
    };

    for (auto I = Allocator.Slabs.begin(), E = Allocator.Slabs.end(); I != E;
         ++I) {
      size_t AllocatedSlabSize = BumpPtrAllocator::computeSlabSize(
          std::distance(Allocator.Slabs.begin(), I));
      char* Begin = (char*)alignAddr(*I, alignof(T));
      char* End = *I == Allocator.Slabs.back() ? Allocator.CurPtr
                                               : (char*)*I + AllocatedSlabSize;

      DestroyElements(Begin, End);
    }

    for (auto& PtrAndSize : Allocator.CustomSizedSlabs) {
      void* Ptr = PtrAndSize.first;
      size_t Size = PtrAndSize.second;
      DestroyElements((char*)alignAddr(Ptr, alignof(T)), (char*)Ptr + Size);
    }
  }
};
} // namespace gtirb

template <size_t SlabSize, size_t SizeThreshold>
void* operator new(
    size_t Size,
    gtirb::BumpPtrAllocatorImpl<SlabSize, SizeThreshold>& Allocator) {
  struct S {
    char c;
    union {
      double D;
      long double LD;
      long long L;
      void* P;
    } x;
  };
  return Allocator.Allocate(
      Size, std::min((size_t)gtirb::NextPowerOf2(Size), offsetof(S, x)));
}

template <size_t SlabSize, size_t SizeThreshold>
void operator delete(void*,
                     gtirb::BumpPtrAllocatorImpl<SlabSize, SizeThreshold>&) {}

#endif // GTIRB_ALLOCATOR_H
