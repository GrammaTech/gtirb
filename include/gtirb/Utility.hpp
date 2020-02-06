//===- Utility.hpp ----------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_UTILITY_H
#define GTIRB_UTILITY_H

#include <gtirb/Addr.hpp>
#include <gtirb/Export.hpp>
#include <gtirb/Node.hpp>
#include <boost/iterator/iterator_categories.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/iterator/iterator_traits.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/range/iterator_range.hpp>
#include <functional>
#include <iterator>
#include <optional>
#include <type_traits>
#include <vector>

namespace gtirb {

/// @cond INTERNAL

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
  MergeSortedIterator() = default;

  /// \brief Create a MergeSortedIterator from a range of ranges.
  ///
  /// \tparam RangeIteratorRange Any class fulfilling the Boost concept
  /// SinglePassRange<SinglePassIterator<SinglePassRange<ForwardIterator>>>.
  ///
  /// \param RangeRange A \p RangeIteratorRange to build this iterator from.
  template <typename RangeIteratorRange>
  explicit MergeSortedIterator(RangeIteratorRange RangeRange) {
    for (const auto& Range : RangeRange) {
      if (auto RBegin = Range.begin(), REnd = Range.end(); RBegin != REnd) {
        Ranges.emplace_back(RBegin, REnd);
      }
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
  MergeSortedIterator(RangeIterator Begin, RangeIterator End)
      : MergeSortedIterator(boost::make_iterator_range(Begin, End)) {}

  // Beginning of functions for iterator facade compatibility.
  typename std::iterator_traits<ForwardIterator>::reference
  dereference() const {
    assert(!Ranges.empty() && "Attempt to dereference end of iterator!");
    return *Ranges[minIndex()].first;
  }

  bool equal(const MergeSortedIterator<ForwardIterator, Compare>& Other) const {
    return Ranges == Other.Ranges;
  }

  void increment() {
    assert(!Ranges.empty() && "Attempt to increment end of iterator!");
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

/// \class AddressLess
///
/// \brief A comparison function object for comparing nodes in address order.
///
/// \tparam T The type to compare addresses with. Must have a method with the
///           signature "std::optional<Addr> getAddress() const".
template <typename T> struct AddressLess {
  using key_type = std::optional<Addr>;

  static key_type key(const T& N) { return N.getAddress(); }
  static key_type key(const T* N) { return N->getAddress(); }

  bool operator()(const T& N1, const T& N2) const { return key(N1) < key(N2); }
  bool operator()(const T* N1, const T* N2) const {
    return key(*N1) < key(*N2);
  }
};

/// \class BlockAddressLess
///
/// \brief A comparison function object for comparing blocks (that is, \ref Node
/// objects that are either \ref CodeBlock or \ref DastaBlock objects) in
/// address order.
struct GTIRB_EXPORT_API BlockAddressLess {
  using key_type = std::optional<Addr>;

  static key_type key(const Node& N);
  static key_type key(const Node* N) { return key(*N); }

  bool operator()(const Node* N1, const Node* N2) const {
    return key(*N1) < key(*N2);
  }
  bool operator()(const Node& N1, const Node& N2) const {
    return key(N1) < key(N2);
  }
};

/// \class ArbitraryLess
///
/// \brief A comparison function object for comparing objects in a manner with
/// no ordering guarantees.
///
/// \tparam T Any type.
template <typename T> struct ArbitraryLess {
  bool operator()(const T& N1, const T& N2) const { return &N1 < &N2; }
};

/// \class NodeToChildRange
///
/// \brief A function object for constructing \ref MergeSortedIterator objects
/// via Boost transform iterators.
///
/// \tparam T The type to retrieve a range of children nodes from.
/// \tparam Method A pointer-to-method type, taking no arguments and retuning
///                a child node iterator.
/// \tparam Begin  A pointer-to-method of the beginning of the child node range.
/// \tparam End    A pointer-to-method of the end of the child node range.
template <typename T, typename Method, Method Begin, Method End>
struct NodeToChildRange {
  boost::iterator_range<decltype((std::declval<T>().*Begin)())>
  operator()(T& N) const {
    return boost::make_iterator_range((N.*Begin)(), (N.*End)());
  }
};

/// \class NodeToBlockRange
///
/// \brief A function object for constructing \ref MergeSortedIterator objects
/// via Boost transform iterators. Returns ranges of \ref Node objects, which
/// are either \ref CodeBlock objects or \ref DataBlock objects.
///
/// \tparam T The type to retrieve ranges of blocks from. If const-qualified,
///           the const iterators on this type are used; else the non-const
///           iterators are used.
template <typename T>
using NodeToBlockRange = NodeToChildRange<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_block_iterator (T::*)() const,
                       typename T::block_iterator (T::*)()>,
    &T::blocks_begin, &T::blocks_end>;

/// \class NodeToCodeBlockRange
///
/// \brief A function object for constructing \ref MergeSortedIterator objects
/// via Boost transform iterators. Returns ranges of \ref CodeBlock objects.
///
/// \tparam T The type to retrieve ranges of blocks from. If const-qualified,
///           the const iterators on this type are used; else the non-const
///           iterators are used.
template <typename T>
using NodeToCodeBlockRange = NodeToChildRange<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_code_block_iterator (T::*)() const,
                       typename T::code_block_iterator (T::*)()>,
    &T::code_blocks_begin, &T::code_blocks_end>;

/// \class NodeToDataBlockRange
///
/// \brief A function object for constructing \ref MergeSortedIterator objects
/// via Boost transform iterators. Returns ranges of \ref DataBlock objects.
///
/// \tparam T The type to retrieve ranges of blocks from. If const-qualified,
///           the const iterators on this type are used; else the non-const
///           iterators are used.
template <typename T>
using NodeToDataBlockRange = NodeToChildRange<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_data_block_iterator (T::*)() const,
                       typename T::data_block_iterator (T::*)()>,
    &T::data_blocks_begin, &T::data_blocks_end>;

/// \class NodeToSymbolicExpressionRange
///
/// \brief A function object for constructing \ref MergeSortedIterator objects
/// via Boost transform iterators. Returns ranges of \ref
/// ByteInterval::SymbolicExpressionElement or \ref
/// ByteInterval::ConstSymbolicExpressionElement objects.
///
/// \tparam T The type to retrieve ranges of symbolic expressions from. If
///           const-qualified, the const iterators on this type are used; else
///           the non-const iterators are used.
template <typename T>
using NodeToSymbolicExpressionRange = NodeToChildRange<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_symbolic_expression_iterator (T::*)()
                           const,
                       typename T::symbolic_expression_iterator (T::*)()>,
    &T::symbolic_expressions_begin, &T::symbolic_expressions_end>;

/// \class NodeToByteIntervalRange
///
/// \brief A function object for constructing \ref MergeSortedIterator objects
/// via Boost transform iterators. Returns ranges of \ref ByteInterval objects.
///
/// \tparam T The type to retrieve ranges of intervals from. If const-qualified,
///           the const iterators on this type are used; else the non-const
///           iterators are used.
template <typename T>
using NodeToByteIntervalRange = NodeToChildRange<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_byte_interval_iterator (T::*)() const,
                       typename T::byte_interval_iterator (T::*)()>,
    &T::byte_intervals_begin, &T::byte_intervals_end>;

/// \class NodeToSymbolRange
///
/// \brief A function object for constructing \ref MergeSortedIterator objects
/// via Boost transform iterators. Returns ranges of \ref Symbol objects.
///
/// \tparam T The type to retrieve ranges of symbols from. If const-qualified,
///           the const iterators on this type are used; else the non-const
///           iterators are used.
template <typename T>
using NodeToSymbolRange = NodeToChildRange<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_symbol_iterator (T::*)() const,
                       typename T::symbol_iterator (T::*)()>,
    &T::symbols_begin, &T::symbols_end>;

/// \class NodeToSectionRange
///
/// \brief A function object for constructing \ref MergeSortedIterator objects
/// via Boost transform iterators. Returns ranges of \ref Section objects.
///
/// \tparam T The type to retrieve ranges of sections from. If const-qualified,
///           the const iterators on this type are used; else the non-const
///           iterators are used.
template <typename T>
using NodeToSectionRange = NodeToChildRange<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_section_iterator (T::*)() const,
                       typename T::section_iterator (T::*)()>,
    &T::sections_begin, &T::sections_end>;

/// \class NodeToProxyBlockRange
///
/// \brief A function object for constructing \ref MergeSortedIterator objects
/// via Boost transform iterators. Returns ranges of \ref ProxyBlock objects.
///
/// \tparam T The type to retrieve ranges of blocks from. If const-qualified,
///           the const iterators on this type are used; else the non-const
///           iterators are used.
template <typename T>
using NodeToProxyBlockRange = NodeToChildRange<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_proxy_block_iterator (T::*)() const,
                       typename T::proxy_block_iterator (T::*)()>,
    &T::proxy_blocks_begin, &T::proxy_blocks_end>;

/// \class FindNodesAt
///
/// \brief A function object for merging together calls to "findNodeAt" style
/// methods, taking one address parameter.
///
/// \tparam T The type of node to call \p FindAtMethod on.
/// \tparam MethodType The type of \p Method.
/// \tparam Method A pointer to a method on \p T that takes an
/// address and returns a range.
template <typename T, typename MethodType, MethodType Method>
struct FindNodesAt {
  Addr A;

  FindNodesAt(Addr A_) : A{A_} {}

  decltype((std::declval<T>().*Method)(Addr())) operator()(T& N) const {
    return (N.*Method)(A);
  }
};

/// \class FindNodesBetween
///
/// \brief A function object for merging together calls to "findNodeAt" style
/// methods, taking two address parameters.
///
/// \tparam T The type of node to call \p FindAtMethod on.
/// \tparam MethodType The type of \p Method.
/// \tparam Method A pointer to a method on \p T that takes two
/// addresses and returns a range.
template <typename T, typename MethodType, MethodType Method>
struct FindNodesBetween {
  Addr Low, High;

  FindNodesBetween(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}

  decltype((std::declval<T>().*Method)(Addr(), Addr())) operator()(T& N) const {
    return (N.*Method)(Low, High);
  }
};

/// \class FindBlocksIn
///
/// \brief A function object for merging together calls to findBlocksIn.
///
/// \tparam T The node to call findBlocksIn from. If const-qualified,
///           the const functions on this type are used; else the non-const
///           functions are used.
template <typename T>
using FindBlocksIn = FindNodesAt<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_block_subrange (T::*)(Addr) const,
                       typename T::block_subrange (T::*)(Addr)>,
    &T::findBlocksIn>;

/// \class FindBlocksAt
///
/// \brief A function object for merging together calls to findBlocksAt, taking
/// one address parameter.
///
/// \tparam T The node to call findBlocksAt from. If const-qualified,
///           the const functions on this type are used; else the non-const
///           functions are used.
template <typename T>
using FindBlocksAt = FindNodesAt<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_block_range (T::*)(Addr) const,
                       typename T::block_range (T::*)(Addr)>,
    &T::findBlocksAt>;

/// \class FindBlocksBetween
///
/// \brief A function object for merging together calls to findBlocksAt, taking
/// two address parameters.
///
/// \tparam T The node to call findBlocksAt from. If const-qualified,
///           the const functions on this type are used; else the non-const
///           functions are used.
template <typename T>
using FindBlocksBetween = FindNodesBetween<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_block_range (T::*)(Addr, Addr) const,
                       typename T::block_range (T::*)(Addr, Addr)>,
    &T::findBlocksAt>;

/// \class FindCodeBlocksIn
///
/// \brief A function object for merging together calls to findCodeBlocksIn.
///
/// \tparam T The node to call findCodeBlocksIn from. If const-qualified,
///           the const functions on this type are used; else the non-const
///           functions are used.
template <typename T>
using FindCodeBlocksIn = FindNodesAt<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_code_block_subrange (T::*)(Addr) const,
                       typename T::code_block_subrange (T::*)(Addr)>,
    &T::findCodeBlocksIn>;

/// \class FindCodeBlocksAt
///
/// \brief A function object for merging together calls to findCodeBlocksAt,
/// taking one address parameter.
///
/// \tparam T The node to call findCodeBlocksAt from. If const-qualified,
///           the const functions on this type are used; else the non-const
///           functions are used.
template <typename T>
using FindCodeBlocksAt = FindNodesAt<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_code_block_range (T::*)(Addr) const,
                       typename T::code_block_range (T::*)(Addr)>,
    &T::findCodeBlocksAt>;

/// \class FindCodeBlocksBetween
///
/// \brief A function object for merging together calls to findCodeBlocksAt,
/// taking two address parameters.
///
/// \tparam T The node to call findCodeBlocksAt from. If const-qualified,
///           the const functions on this type are used; else the non-const
///           functions are used.
template <typename T>
using FindCodeBlocksBetween = FindNodesBetween<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_code_block_range (T::*)(Addr, Addr)
                           const,
                       typename T::code_block_range (T::*)(Addr, Addr)>,
    &T::findCodeBlocksAt>;

/// \class FindDataBlocksIn
///
/// \brief A function object for merging together calls to findDataBlocksIn.
///
/// \tparam T The node to call findDataBlocksIn from. If const-qualified,
///           the const functions on this type are used; else the non-const
///           functions are used.
template <typename T>
using FindDataBlocksIn = FindNodesAt<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_data_block_subrange (T::*)(Addr) const,
                       typename T::data_block_subrange (T::*)(Addr)>,
    &T::findDataBlocksIn>;

/// \class FindDataBlocksAt
///
/// \brief A function object for merging together calls to findDataBlocksAt,
/// taking one address parameter.
///
/// \tparam T The node to call findDataBlocksAt from. If const-qualified,
///           the const functions on this type are used; else the non-const
///           functions are used.
template <typename T>
using FindDataBlocksAt = FindNodesAt<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_data_block_range (T::*)(Addr) const,
                       typename T::data_block_range (T::*)(Addr)>,
    &T::findDataBlocksAt>;

/// \class FindDataBlocksBetween
///
/// \brief A function object for merging together calls to findDataBlocksAt,
/// taking two address parameters.
///
/// \tparam T The node to call findDataBlocksAt from. If const-qualified,
///           the const functions on this type are used; else the non-const
///           functions are used.
template <typename T>
using FindDataBlocksBetween = FindNodesBetween<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_data_block_range (T::*)(Addr, Addr)
                           const,
                       typename T::data_block_range (T::*)(Addr, Addr)>,
    &T::findDataBlocksAt>;

/// \class FindSymExprsAt
///
/// \brief A function object for merging together calls to
/// findSymbolicExpressionsAt, taking one address parameter.
///
/// \tparam T The node to call findSymbolicExpressionsAt from. If
/// const-qualified, the const functions on this type are used; else the
/// non-const functions are used.
template <typename T>
using FindSymExprsAt = FindNodesAt<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_symbolic_expression_range (T::*)(Addr)
                           const,
                       typename T::symbolic_expression_range (T::*)(Addr)>,
    &T::findSymbolicExpressionsAt>;

/// \class FindSymExprsBetween
///
/// \brief A function object for merging together calls to
/// findSymbolicExpressionsAt, taking two address parameters.
///
/// \tparam T The node to call findSymbolicExpressionsAt from. If
/// const-qualified, the const functions on this type are used; else the
/// non-const functions are used.
template <typename T>
using FindSymExprsBetween = FindNodesBetween<
    T,
    std::conditional_t<
        std::is_const_v<T>,
        typename T::const_symbolic_expression_range (T::*)(Addr, Addr) const,
        typename T::symbolic_expression_range (T::*)(Addr, Addr)>,
    &T::findSymbolicExpressionsAt>;

/// \class FindByteIntervalsIn
///
/// \brief A function object for merging together calls to findByteIntervalsIn.
///
/// \tparam T The node to call findByteIntervalsIn from. If const-qualified,
///           the const functions on this type are used; else the non-const
///           functions are used.
template <typename T>
using FindByteIntervalsIn =
    FindNodesAt<T,
                std::conditional_t<
                    std::is_const_v<T>,
                    typename T::const_byte_interval_subrange (T::*)(Addr) const,
                    typename T::byte_interval_subrange (T::*)(Addr)>,
                &T::findByteIntervalsIn>;

/// \class FindByteIntervalsAt
///
/// \brief A function object for merging together calls to
/// findByteIntervalsAt, taking one address parameter.
///
/// \tparam T The node to call findByteIntervalsAt from. If
/// const-qualified, the const functions on this type are used; else the
/// non-const functions are used.
template <typename T>
using FindByteIntervalsAt = FindNodesAt<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_byte_interval_range (T::*)(Addr) const,
                       typename T::byte_interval_range (T::*)(Addr)>,
    &T::findByteIntervalsAt>;

/// \class FindByteIntervalsBetween
///
/// \brief A function object for merging together calls to
/// findByteIntervalsAt, taking two address parameters.
///
/// \tparam T The node to call findByteIntervalsAt from. If
/// const-qualified, the const functions on this type are used; else the
/// non-const functions are used.
template <typename T>
using FindByteIntervalsBetween = FindNodesBetween<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_byte_interval_range (T::*)(Addr, Addr)
                           const,
                       typename T::byte_interval_range (T::*)(Addr, Addr)>,
    &T::findByteIntervalsAt>;

/// \class FindSectionsIn
///
/// \brief A function object for merging together calls to findSectionsIn.
///
/// \tparam T The node to call findSectionsIn from. If const-qualified,
///           the const functions on this type are used; else the non-const
///           functions are used.
template <typename T>
using FindSectionsIn = FindNodesAt<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_section_subrange (T::*)(Addr) const,
                       typename T::section_subrange (T::*)(Addr)>,
    &T::findSectionsIn>;

/// \class FindSectionsAt
///
/// \brief A function object for merging together calls to
/// findSectionsAt, taking one address parameter.
///
/// \tparam T The node to call findSectionsAt from. If
/// const-qualified, the const functions on this type are used; else the
/// non-const functions are used.
template <typename T>
using FindSectionsAt = FindNodesAt<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_section_range (T::*)(Addr) const,
                       typename T::section_range (T::*)(Addr)>,
    &T::findSectionsAt>;

/// \class FindSectionsBetween
///
/// \brief A function object for merging together calls to
/// findSectionsAt, taking two address parameters.
///
/// \tparam T The node to call findSectionsAt from. If
/// const-qualified, the const functions on this type are used; else the
/// non-const functions are used.
template <typename T>
using FindSectionsBetween = FindNodesBetween<
    T,
    std::conditional_t<std::is_const_v<T>,
                       typename T::const_section_range (T::*)(Addr, Addr) const,
                       typename T::section_range (T::*)(Addr, Addr)>,
    &T::findSectionsAt>;

template <typename T> std::optional<uint64_t> asOptionalSize(T X);

/// @endcond

} // namespace gtirb

#endif // GTIRB_UTILITY_H
