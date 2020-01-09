//===- MergeSortedIterator.test.cpp -----------------------------*- C++ -*-===//
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
#include <gtirb/MergeSortedIterator.hpp>
#include <boost/range/iterator_range.hpp>
#include <gtest/gtest.h>
#include <vector>

using namespace gtirb;

TEST(Unit_MergeSortedIterator, testCtor0) {
  MergeSortedIterator<std::vector<int>::iterator> MSI;
}

TEST(Unit_MergeSortedIterator, testCtor1) {
  using Vector = std::vector<int>;
  using VectorIt = Vector::iterator;

  Vector Evens = {2, 4, 6, 8};
  Vector Odds = {1, 3, 5, 7};
  std::vector<boost::iterator_range<VectorIt>> Its = {
      boost::make_iterator_range(Evens), boost::make_iterator_range(Odds)};

  MergeSortedIterator<std::vector<int>::iterator> Begin{Its};
  MergeSortedIterator<std::vector<int>::iterator> End;

  Vector Combined = {1, 2, 3, 4, 5, 6, 7, 8};
  auto ExpectedIt = Combined.begin();
  auto GotIt = Begin;
  while (ExpectedIt != Combined.end() && GotIt != End) {
    ASSERT_EQ(*ExpectedIt, *GotIt);
    ++ExpectedIt;
    ++GotIt;
  }
  ASSERT_EQ(ExpectedIt, Combined.end());
  ASSERT_EQ(GotIt, End);
}

TEST(Unit_MergeSortedIterator, testCtor2) {
  using Vector = std::vector<int>;
  using VectorIt = Vector::iterator;

  Vector Evens = {2, 4, 6, 8};
  Vector Odds = {1, 3, 5, 7};
  std::vector<boost::iterator_range<VectorIt>> Its = {
      boost::make_iterator_range(Evens), boost::make_iterator_range(Odds)};

  MergeSortedIterator<std::vector<int>::iterator> Begin{Its.begin(), Its.end()};
  MergeSortedIterator<std::vector<int>::iterator> End;

  Vector Combined = {1, 2, 3, 4, 5, 6, 7, 8};
  auto ExpectedIt = Combined.begin();
  auto GotIt = Begin;
  while (ExpectedIt != Combined.end() && GotIt != End) {
    ASSERT_EQ(*ExpectedIt, *GotIt);
    ++ExpectedIt;
    ++GotIt;
  }
  ASSERT_EQ(ExpectedIt, Combined.end());
  ASSERT_EQ(GotIt, End);
}

struct CustomCompare {
  bool operator()(int x, int y) const { return x >= y; }
};

TEST(Unit_MergeSortedIterator, testCustomCompare) {
  using Vector = std::vector<int>;
  using VectorIt = Vector::iterator;

  Vector Evens = {8, 6, 4, 2};
  Vector Odds = {7, 5, 3, 1};
  std::vector<boost::iterator_range<VectorIt>> Its = {
      boost::make_iterator_range(Evens), boost::make_iterator_range(Odds)};

  MergeSortedIterator<std::vector<int>::iterator, CustomCompare> Begin{Its};
  MergeSortedIterator<std::vector<int>::iterator, CustomCompare> End;

  Vector Combined = {8, 7, 6, 5, 4, 3, 2, 1};
  auto ExpectedIt = Combined.begin();
  auto GotIt = Begin;
  while (ExpectedIt != Combined.end() && GotIt != End) {
    ASSERT_EQ(*ExpectedIt, *GotIt);
    ++ExpectedIt;
    ++GotIt;
  }
  ASSERT_EQ(ExpectedIt, Combined.end());
  ASSERT_EQ(GotIt, End);
}
