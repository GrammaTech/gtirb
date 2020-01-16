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
#include <gtirb/MergeSortedIterator.hpp>
#include <gtirb/Node.hpp>
#include <boost/range/iterator_range.hpp>
#include <optional>
#include <type_traits>

namespace gtirb {

/// @cond INTERNAL

/// \class AddressOrder
///
/// \brief A comparison function object for comparing nodes in address order.
///
/// \tparam T The type to compare addresses with. Must have a method with the
///           signature "std::optional<Addr> getAddress() const".
template <typename T> struct AddressOrder {
  using key_type = std::optional<Addr>;
  static key_type key(const T& N) { return N.getAddress(); }
  bool operator()(const T* N1, const T* N2) const {
    return key(*N1) < key(*N2);
  }
};

/// \class BlockAddressOrder
///
/// \brief A comparison function object for comparing blocks (that is, \ref Node
/// objects that are either \ref CodeBlock or \ref DastaBlock objects) in
/// address order.
struct GTIRB_EXPORT_API BlockAddressOrder {
  using key_type = std::optional<Addr>;
  static key_type getAddress(const Node* N);
  bool operator()(const Node& N1, const Node& N2) const {
    return getAddress(&N1) < getAddress(&N2);
  }
};

/// \class ArbitraryOrder
///
/// \brief A comparison function object for comparing types in an arbitrary yet
/// stable manner.
///
/// \tparam T Any type.
template <typename T> struct ArbitraryOrder {
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
/// @endcond

} // namespace gtirb

#endif // GTIRB_UTILITY_H
