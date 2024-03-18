//===- TestHelpers.hpp ------------------------------------------*- C++ -*-===//
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

#ifndef GTIRB_TEST_HELPERS_H
#define GTIRB_TEST_HELPERS_H

#include <boost/iterator/transform_iterator.hpp>
#include <boost/range/iterator_range.hpp>

// Transforms a range of references into a range of pointers.
template <typename RangeTy> auto pointers(const RangeTy& Range) {
  auto GetPointer = [](auto& arg) { return &arg; };
  return boost::make_iterator_range(
      boost::make_transform_iterator(Range.begin(), GetPointer),
      boost::make_transform_iterator(Range.end(), GetPointer));
}

#endif // GTIRB_TEST_HELPERS_H
