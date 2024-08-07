//===- UtilsDeprecatedGlobals.test.cpp --------------------------*- C++ -*-===//
//
//  Copyright (C) 2024 GrammaTech, Inc.
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
#include <gtest/gtest.h>

/*
Ensure it is possible to use utility functions with no namespace if
GTIRB_WRAP_UTILS_IN_NAMESPACE is not defined.
*/
TEST(Unit_IR, globalNextPowerOfTwo) { EXPECT_EQ(NextPowerOf2(5), 8U); }
