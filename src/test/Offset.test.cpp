//===- Offset.test.cpp ------------------------------------------*- C++ -*-===//
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
#include <gtirb/Offset.hpp>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_Offset, ordering) {
  UUID uuid1;
  UUID uuid2;
  uint64_t disp1(5);
  uint64_t disp2(10);
  Offset offset1(uuid1, disp1);
  Offset offset2(uuid1, disp2);
  Offset offset3(uuid2, disp1);
  Offset offset4(uuid2, disp2);
  EXPECT_TRUE(offset1 < offset2);
  EXPECT_TRUE(offset3 < offset4);
  if (uuid1 < uuid2) {
    EXPECT_TRUE(offset1 < offset3);
    EXPECT_TRUE(offset2 < offset3);
    EXPECT_TRUE(offset1 < offset4);
    EXPECT_TRUE(offset2 < offset4);
  }
  if (uuid2 < uuid1) {
    EXPECT_TRUE(offset1 > offset3);
    EXPECT_TRUE(offset2 > offset3);
    EXPECT_TRUE(offset1 > offset4);
    EXPECT_TRUE(offset2 > offset4);
  }
}

TEST(Unit_Offset, hash) {
  UUID uuid1;
  uint64_t disp1(5);
  uint64_t disp2(10);
  Offset offset1(uuid1, disp1);
  Offset offset2(uuid1, disp2);
  EXPECT_EQ(std::hash<Offset>()(offset1), std::hash<Offset>()(offset1));
  EXPECT_NE(std::hash<Offset>()(offset1), std::hash<Offset>()(offset2));
}
