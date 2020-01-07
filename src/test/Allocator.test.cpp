//===- Allocator.test.cpp ---------------------------------------*- C++ -*-===//
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

#include <gtirb/Allocator.hpp>
#include <array>
#include <gtest/gtest.h>

class AllocTest {
public:
  static size_t CtorCount;
  static size_t DtorCount;

  AllocTest() { CtorCount++; }
  ~AllocTest() { DtorCount++; }

  std::array<char, 7> Data; // Take up some space
};
size_t AllocTest::CtorCount = 0;
size_t AllocTest::DtorCount = 0;

using Allocator = SpecificBumpPtrAllocator<AllocTest>;

inline void* operator new(size_t, Allocator& A) { return A.Allocate(); }
inline void operator delete(void*, Allocator&) {}

TEST(Unit_Allocator, allocate) {
  AllocTest::CtorCount = AllocTest::DtorCount = 0;
  Allocator A;
  EXPECT_NE(new (A) AllocTest, nullptr);
  EXPECT_NE(new (A) AllocTest, nullptr);
  EXPECT_NE(new (A) AllocTest, nullptr);
  EXPECT_EQ(AllocTest::CtorCount, 3);
}

TEST(Unit_Allocator, deallocate) {
  // For varying numbers of allocations, ensure that destructors are called.
  for (int AllocCount = 0; AllocCount < 100; AllocCount++) {
    AllocTest::CtorCount = AllocTest::DtorCount = 0;
    {
      Allocator A;
      EXPECT_NE(new (A) AllocTest, nullptr);
      EXPECT_NE(new (A) AllocTest, nullptr);
      EXPECT_NE(new (A) AllocTest, nullptr);
      EXPECT_EQ(AllocTest::DtorCount, 0);
    }
    // All objects destroyed when allocator goes out of scope
    EXPECT_EQ(AllocTest::DtorCount, AllocTest::CtorCount);
  }
}
