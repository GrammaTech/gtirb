//===- MergeSortedIterator.hpp ----------------------------------*- C++ -*-===//
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
#ifndef GTIRB_MERGE_SORTED_ITERATOR_H
#define GTIRB_MERGE_SORTED_ITERATOR_H

#include <boost/iterator/iterator_categories.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/iterator/iterator_traits.hpp>
#include <boost/range/iterator_range.hpp>
#include <functional>
#include <iterator>
#include <vector>

namespace gtirb {

/// \class MergeSortedIterator
///
/// \brief This iterator merges a set of sorted iterators together, producing
/// a single sorted output iterator.
///
/// This iterator is a forward iterator, irrespective of the class of the base
/// iterator.
///
/// \tparam ForwardIterator The type of forward iterator to be merged. Results
///                         from these iterators must be in sorted order.
/// \tparam Compare         A comparison function object to use to sort results.
///                         Defaults to std::less.
template <typename ForwardIterator,
          typename Compare = std::less<
              typename std::iterator_traits<ForwardIterator>::value_type>>
class MergeSortedIterator
    : public boost::iterator_facade<
          MergeSortedIterator<ForwardIterator, Compare>,
          typename std::iterator_traits<ForwardIterator>::value_type,
          boost::forward_traversal_tag,
          typename std::iterator_traits<ForwardIterator>::reference,
          typename std::iterator_traits<ForwardIterator>::difference_type> {
public:
  /// \brief Create a MergeSortedIterator representing the end of iteration.
  ///
  /// Dereferencing or incrementing this iterator results in undefined behavior.
  MergeSortedIterator() {}

  /// \brief Create a MergeSortedIterator from a range of ranges.
  ///
  /// \tparam RangeIteratorRange Any class fulfilling the Boost concept
  /// SinglePassRange<SinglePassIterator<SinglePassRange<ForwardIterator>>>.
  ///
  /// \param RangeRange A \p RangeIteratorRange to build this iterator from.
  template <typename RangeIteratorRange>
  explicit MergeSortedIterator(RangeIteratorRange RangeRange) {
    for (const auto& Range : RangeRange) {
      Ranges.emplace_back(Range.begin(), Range.end());
    }
  }

  /// \brief Create a MergeSortedIterator from an iterator of ranges.
  ///
  /// \tparam RangeIterator Any class fulfilling the Boost concept
  /// SinglePassIterator<SinglePassRange<ForwardIterator>>.
  ///
  /// \param Begin The beginning of the ranges to build this iterator from.
  /// \param End   The end of the ranges to build this iterator from.
  template <typename RangeIterator>
  MergeSortedIterator(RangeIterator Begin, RangeIterator End) {
    for (const auto& Range : boost::make_iterator_range(Begin, End)) {
      Ranges.emplace_back(Range.begin(), Range.end());
    }
  }

  // Beginning of functions for iterator facade compatibility.
  typename std::iterator_traits<ForwardIterator>::reference
  dereference() const {
    return *Ranges[minIndex()].first;
  }

  bool equal(const MergeSortedIterator<ForwardIterator, Compare>& Other) const {
    return Ranges == Other.Ranges;
  }

  void increment() {
    auto MinIndex = minIndex();
    auto& MinRange = Ranges[MinIndex];
    MinRange.first++;
    if (MinRange.first == MinRange.second) {
      Ranges.erase(Ranges.begin() + MinIndex);
    }
  }
  // End of functions for iterator facade compatibility.
private:
  std::vector<std::pair<ForwardIterator, ForwardIterator>> Ranges;

  /// \brief Get the index in \p Ranges containing the iterator with the
  /// smallest item, as determined via /p Compare.
  size_t minIndex() const {
    size_t Result = 0;
    for (size_t I = 0; I < Ranges.size(); I++) {
      if (Compare()(*Ranges[I].first, *Ranges[Result].first)) {
        Result = I;
      }
    }
    return Result;
  }
};

} // namespace gtirb

#endif // GTIRB_MERGE_SORTED_ITERATOR_H
