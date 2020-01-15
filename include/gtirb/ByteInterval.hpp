//===- ByteInterval.hpp -----------------------------------------*- C++ -*-===//
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

#ifndef GTIRB_BYTE_INTERVAL_H
#define GTIRB_BYTE_INTERVAL_H

#include <gtirb/Export.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <array>
#include <boost/endian/conversion.hpp>
#include <boost/icl/interval_map.hpp>
#include <boost/iterator/filter_iterator.hpp>
#include <boost/iterator/iterator_categories.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/iterator/iterator_traits.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/key_extractors.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/range/iterator_range.hpp>
#include <cstdint>
#include <map>
#include <optional>
#include <type_traits>
#include <variant>
#include <vector>

/// \file ByteInterval.hpp
/// \brief Class gtirb::ByteInterval.

namespace proto {
class ByteInterval;
} // namespace proto

namespace gtirb {
class Section;   // Forward declared for the backpointer.
class CodeBlock; // Forward declared so Blocks can store CodeBlocks.
class DataBlock; // Forward declared so Blocks can store DataBlocks.

/// \class ByteInterval
///
/// \brief A contiguous region of bytes in a binary.
///
/// A ByteInterval defines a relative ordering for a group of \ref Block
/// objects, optionally at a fixed address in memory. It also stores the bytes
/// associated with these blocks.
///
/// If two blocks are in two different ByteIntervals, then it should be
/// considered safe (that is, preserving of program semantics) to move one block
/// relative to the other in memory. If two blocks are in the same ByteInterval,
/// then it should be considered unknown if moving the two blocks relative to
/// one another in memory is a safe operation.
class GTIRB_EXPORT_API ByteInterval : public Node {
  /// \class Block
  ///
  /// \brief A node (either a \ref CodeBlock or \ref DataBlock), alongside an
  /// offset, held within this interval.
  struct Block {
    uint64_t Offset;
    gtirb::Node* Node;

    Block(uint64_t Off, gtirb::Node* N) : Offset(Off), Node(N) {}

    /// \brief Get the offset from the beginning of this block's \ref
    /// ByteInterval.
    uint64_t getOffset() const { return Offset; }

    /// \brief Get the \ref Node, either a \ref CodeBlock or \ref DataBlock, in
    /// the \ref ByteInterval.
    ///
    /// This function returns a nonconst pointer despite being const
    /// because it's valid to mutate the nodes, but not valid to mutate the
    /// block-offset pairs in the BlockSet.
    gtirb::Node* getNode() const { return Node; }
  };

  /// \class BlockKindEquals
  ///
  /// \brief A predicate object to discern one type of block.
  ///
  /// This predicate does not interact with the subclassing system;
  /// BlockKindEquals<Node::Kind::CfgNode> will not identify any blocks as CFG
  /// nodes, for example.
  template <Node::Kind K> struct BlockKindEquals {
    bool operator()(const Block& B) const {
      return B.getNode()->getKind() == K;
    }
  };

  /// \class BlockToNode
  ///
  /// \brief A function for a transform iterator to turn blocks into nodes.
  template <typename NodeType> struct BlockToNode {
    NodeType& operator()(const Block& B) const {
      // We avoid the call to cast() here because we use this function after
      // BlockKindEquals, which confirms the type of the Node for us
      // (and more importantly, we avoid having to include Code/DataBlock).
      return *reinterpret_cast<NodeType*>(B.Node);
    }
  };

  /// \class BlockPointerToNode
  ///
  /// \brief A function for a transform iterator to turn blocks into nodes.
  template <typename NodeType> struct BlockPointerToNode {
    NodeType& operator()(const Block* B) const {
      // We avoid the call to cast() here because we use this function after
      // BlockKindEquals, which confirms the type of the Node for us
      // (and more importantly, we avoid having to include Code/DataBlock).
      return *reinterpret_cast<NodeType*>(B->Node);
    }
  };

  struct OffsetOrder {
    bool operator()(const Block* b1, const Block* b2) const {
      return b1->Offset < b2->Offset;
    }
  };

  struct by_offset {};
  struct by_pointer {};
  using BlockSet = boost::multi_index::multi_index_container<
      Block, boost::multi_index::indexed_by<
                 boost::multi_index::ordered_non_unique<
                     boost::multi_index::tag<by_offset>,
                     boost::multi_index::const_mem_fun<Block, uint64_t,
                                                       &Block::getOffset>>,
                 boost::multi_index::hashed_unique<
                     boost::multi_index::tag<by_pointer>,
                     boost::multi_index::const_mem_fun<Block, Node*,
                                                       &Block::getNode>>>>;
  using BlockIntMap =
      boost::icl::interval_map<Addr, std::multiset<const Block*, OffsetOrder>>;
  using SymbolicExpressionMap = std::map<uint64_t, SymbolicExpression>;

  /// \brief Get the \ref Block that corresponds to a \ref Node.
  const Block& nodeToBlock(const Node* N) const {
    auto& Index = Blocks.get<by_pointer>();
    auto It = Index.find(const_cast<Node*>(N));
    assert(It != Index.end() &&
           "ByteInterval::nodeToBlock called with block not in interval");
    return *It;
  }

public:
  /// \brief Create a ByteInterval object.
  ///
  /// \param C         The \ref Context in which this interval will be held.
  /// \param Size      The size of this interval in bytes.
  /// \param InitSize  The number of bytes with initialized values. Defaults
  ///                  to the value of Size.
  /// \return          The newly created ByteInterval.
  static ByteInterval* Create(Context& C, uint64_t Size = 0,
                              std::optional<uint64_t> InitSize = std::nullopt) {
    return C.Create<ByteInterval>(C, std::nullopt, Size,
                                  InitSize.value_or(Size));
  }

  /// \brief Create a ByteInterval object.
  ///
  /// \param C         The \ref Context in which this interval will be held.
  /// \param Address   An (optional) fixed address for this interval.
  /// \param Size      The size of this interval in bytes.
  /// \param InitSize  The number of bytes with initialized values. Defaults
  ///                  to the value of Size.
  /// \return          The newly created ByteInterval.
  static ByteInterval* Create(Context& C, std::optional<Addr> Address,
                              uint64_t Size = 0,
                              std::optional<uint64_t> InitSize = std::nullopt) {
    return C.Create<ByteInterval>(C, Address, Size, InitSize.value_or(Size));
  }

  /// \brief Create a ByteInterval object.
  ///
  /// \tparam InputIterator An input iterator yielding bytes.
  /// \param C          The \ref Context in which this interval will be held.
  /// \param BytesBegin The start of the range to copy to the byte vector.
  /// \param BytesEnd   The end of the range to copy to the byte vector.
  /// \param Size       The size of this interval in bytes. Defaults to the
  ///                   size of the range of bytes. If specified, either
  ///                   trucates the range of bytes given or pads it at the end
  ///                   with zeroes.
  /// \param InitSize   The number of bytes with initialized values. Defaults to
  ///                   the size of the range of bytes. If specified, does NOT
  ///                   zero out values from the range past this number.
  /// \return           The newly created ByteInterval.
  template <typename InputIterator>
  static ByteInterval* Create(Context& C, InputIterator Begin,
                              InputIterator End,
                              std::optional<uint64_t> Size = std::nullopt,
                              std::optional<uint64_t> InitSize = std::nullopt) {
    return C.Create<ByteInterval>(
        C, std::nullopt, Size ? *Size : std::distance(Begin, End),
        InitSize ? *InitSize : std::distance(Begin, End), Begin, End);
  }

  /// \brief Create a ByteInterval object.
  ///
  /// \tparam InputIterator An input iterator yielding bytes.
  /// \param C          The \ref Context in which this interval will be held.
  /// \param Address    An (optional) fixed address for this interval.
  /// \param BytesBegin The start of the range to copy to the byte vector.
  /// \param BytesEnd   The end of the range to copy to the byte vector.
  /// \param Size       The size of this interval in bytes. Defaults to the
  ///                   size of the range of bytes. If specified, either
  ///                   trucates the range of bytes given or pads it at the end
  ///                   with zeroes.
  /// \param InitSize   The number of bytes with initialized values. Defaults to
  ///                   the size of the range of bytes. If specified, does NOT
  ///                   zero out values from the range past this number.
  /// \return           The newly created ByteInterval.
  template <typename InputIterator>
  static ByteInterval* Create(Context& C, std::optional<Addr> Address,
                              InputIterator Begin, InputIterator End,
                              std::optional<uint64_t> Size = std::nullopt,
                              std::optional<uint64_t> InitSize = std::nullopt) {
    return C.Create<ByteInterval>(
        C, Address, Size ? *Size : std::distance(Begin, End),
        InitSize ? *InitSize : std::distance(Begin, End), Begin, End);
  }

  /// \brief Get the \ref Section this byte interval belongs to.
  Section* getSection() { return Parent; }
  /// \brief Get the \ref Section this byte interval belongs to.
  const Section* getSection() const { return Parent; }

  /// \brief Get the fixed address of this interval, if present.
  ///
  /// If this field is present, it may indicate the original address at which
  /// this interval was located at in memory, or it may indicate that this
  /// block's address is fixed and must not be changed. If this field is not
  /// present, it indicates that the interval is free to be moved around in
  /// memory while preserving program semantics.
  std::optional<Addr> getAddress() const { return Address; }

  /// \brief Iterator over \ref Block objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using block_iterator =
      boost::transform_iterator<BlockToNode<Node>,
                                BlockSet::index<by_offset>::type::iterator>;
  /// \brief Range of \ref Block objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using block_range = boost::iterator_range<block_iterator>;
  /// \brief Sub-range of blocks overlapping an address or range of addreses.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using block_subrange = boost::iterator_range<boost::transform_iterator<
      BlockPointerToNode<Node>, BlockIntMap::codomain_type::iterator>>;
  /// \brief Const iterator over \ref Block objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_block_iterator = boost::transform_iterator<
      BlockToNode<const Node>,
      BlockSet::index<by_offset>::type::const_iterator>;
  /// \brief Const range of \ref Block objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_block_range = boost::iterator_range<const_block_iterator>;
  /// \brief Const sub-range of blocks overlapping an address or range of
  /// addreses.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_block_subrange = boost::iterator_range<
      boost::transform_iterator<BlockPointerToNode<const Node>,
                                BlockIntMap::codomain_type::const_iterator>>;

  /// \brief Return an iterator to the first \ref Block.
  block_iterator blocks_begin() { return block_iterator(Blocks.begin()); }
  /// \brief Return a const iterator to the first \ref Block.
  const_block_iterator blocks_begin() const {
    return const_block_iterator(Blocks.begin());
  }
  /// \brief Return an iterator to the element following the last \ref Block.
  block_iterator blocks_end() { return block_iterator(Blocks.end()); }
  /// \brief Return a const iterator to the element following the last
  /// \ref Block.
  const_block_iterator blocks_end() const {
    return const_block_iterator(Blocks.end());
  }
  /// \brief Return a range of the \ref Block objects in this interval.
  block_range blocks() {
    return boost::make_iterator_range(blocks_begin(), blocks_end());
  }
  /// \brief Return a const range of the \ref Block objects in this interval.
  const_block_range blocks() const {
    return boost::make_iterator_range(blocks_begin(), blocks_end());
  }

  block_subrange findBlocksIn(Addr A) {
    auto It = BlockAddrs.find(A);
    if (It == BlockAddrs.end())
      return {};
    return boost::make_iterator_range(
        block_subrange::iterator(It->second.begin()),
        block_subrange::iterator(It->second.end()));
  }

  const_block_subrange findBlocksIn(Addr A) const {
    auto it = BlockAddrs.find(A);
    if (it == BlockAddrs.end())
      return {};
    return boost::make_iterator_range(
        const_block_subrange::iterator(it->second.begin()),
        const_block_subrange::iterator(it->second.end()));
  }

  block_range findBlocksAtOffset(uint64_t Off) {
    auto Pair = Blocks.get<by_offset>().equal_range(Off);
    return boost::make_iterator_range(block_iterator(Pair.first),
                                      block_iterator(Pair.second));
  }

  block_range findBlocksAtOffset(uint64_t Low, uint64_t High) {
    auto& Index = Blocks.get<by_offset>();
    return boost::make_iterator_range(block_iterator(Index.lower_bound(Low)),
                                      block_iterator(Index.upper_bound(High)));
  }

  block_range findBlocksAt(Addr A) {
    if (!Address) {
      return {};
    }
    return findBlocksAtOffset(A - *Address);
  }

  block_range findBlocksAt(Addr Low, Addr High) {
    if (!Address) {
      return {};
    }
    return findBlocksAtOffset(Low - *Address, High - *Address);
  }

  const_block_range findBlocksAtOffset(uint64_t Off) const {
    auto Pair = Blocks.get<by_offset>().equal_range(Off);
    return boost::make_iterator_range(const_block_iterator(Pair.first),
                                      const_block_iterator(Pair.second));
  }

  const_block_range findBlocksAtOffset(uint64_t Low, uint64_t High) const {
    auto& Index = Blocks.get<by_offset>();
    return boost::make_iterator_range(
        const_block_iterator(Index.lower_bound(Low)),
        const_block_iterator(Index.upper_bound(High)));
  }

  const_block_range findBlocksAt(Addr A) const {
    if (!Address) {
      return {};
    }
    return findBlocksAtOffset(A - *Address);
  }

  const_block_range findBlocksAt(Addr Low, Addr High) const {
    if (!Address) {
      return {};
    }
    return findBlocksAtOffset(Low - *Address, High - *Address);
  }

  /// \brief Iterator over \ref CodeBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using code_block_iterator = boost::transform_iterator<
      BlockToNode<CodeBlock>,
      boost::filter_iterator<BlockKindEquals<Node::Kind::CodeBlock>,
                             BlockSet::index<by_offset>::type::iterator>>;
  /// \brief Range of \ref CodeBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using code_block_range = boost::iterator_range<code_block_iterator>;
  /// \brief Const iterator over \ref CodeBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_code_block_iterator = boost::transform_iterator<
      BlockToNode<const CodeBlock>,
      boost::filter_iterator<BlockKindEquals<Node::Kind::CodeBlock>,
                             BlockSet::index<by_offset>::type::const_iterator>>;
  /// \brief Const range of \ref CodeBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_code_block_range =
      boost::iterator_range<const_code_block_iterator>;

  /// \brief Return an iterator to the first \ref CodeBlock.
  code_block_iterator code_blocks_begin() {
    return code_block_iterator(
        code_block_iterator::base_type(Blocks.begin(), Blocks.end()));
  }
  /// \brief Return a const iterator to the first \ref CodeBlock.
  const_code_block_iterator code_blocks_begin() const {
    return const_code_block_iterator(
        const_code_block_iterator::base_type(Blocks.begin(), Blocks.end()));
  }
  /// \brief Return an iterator to the element following the last \ref
  /// CodeBlock.
  code_block_iterator code_blocks_end() {
    return code_block_iterator(
        code_block_iterator::base_type(Blocks.end(), Blocks.end()));
  }
  /// \brief Return a const iterator to the element following the last \ref
  /// CodeBlock.
  const_code_block_iterator code_blocks_end() const {
    return const_code_block_iterator(
        const_code_block_iterator::base_type(Blocks.end(), Blocks.end()));
  }
  /// \brief Return a range of the \ref CodeBlock objects in this interval.
  code_block_range code_blocks() {
    return boost::make_iterator_range(code_blocks_begin(), code_blocks_end());
  }
  /// \brief Return a const range of the \ref CodeBlock objects in this
  /// interval.
  const_code_block_range code_blocks() const {
    return boost::make_iterator_range(code_blocks_begin(), code_blocks_end());
  }

  /// \brief Iterator over \ref DataBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using data_block_iterator = boost::transform_iterator<
      BlockToNode<DataBlock>,
      boost::filter_iterator<BlockKindEquals<Node::Kind::DataBlock>,
                             BlockSet::index<by_offset>::type::iterator>>;
  /// \brief Range of \ref DataBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using data_block_range = boost::iterator_range<data_block_iterator>;
  /// \brief Const iterator over \ref DataBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_data_block_iterator = boost::transform_iterator<
      BlockToNode<const DataBlock>,
      boost::filter_iterator<BlockKindEquals<Node::Kind::DataBlock>,
                             BlockSet::index<by_offset>::type::const_iterator>>;
  /// \brief Const range of \ref DataBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_data_block_range =
      boost::iterator_range<const_data_block_iterator>;

  /// \brief Return an iterator to the first \ref DataBlock.
  data_block_iterator data_blocks_begin() {
    return data_block_iterator(
        data_block_iterator::base_type(Blocks.begin(), Blocks.end()));
  }
  /// \brief Return a const iterator to the first \ref DataBlock.
  const_data_block_iterator data_blocks_begin() const {
    return const_data_block_iterator(
        const_data_block_iterator::base_type(Blocks.begin(), Blocks.end()));
  }
  /// \brief Return an iterator to the element following the last \ref
  /// DataBlock.
  data_block_iterator data_blocks_end() {
    return data_block_iterator(
        data_block_iterator::base_type(Blocks.end(), Blocks.end()));
  }
  /// \brief Return a const iterator to the element following the last \ref
  /// DataBlock.
  const_data_block_iterator data_blocks_end() const {
    return const_data_block_iterator(
        const_data_block_iterator::base_type(Blocks.end(), Blocks.end()));
  }
  /// \brief Return a range of the \ref DataBlock objects in this interval.
  data_block_range data_blocks() {
    return boost::make_iterator_range(data_blocks_begin(), data_blocks_end());
  }
  /// \brief Return a const range of the \ref DataBlock objects in this
  /// interval.
  const_data_block_range data_blocks() const {
    return boost::make_iterator_range(data_blocks_begin(), data_blocks_end());
  }

private:
  template <typename SymExprElementType> class SymExprPairToElement {
    using ByteIntervalType =
        decltype(std::declval<SymExprElementType>().getByteInterval());
    ByteIntervalType BI;

  public:
    explicit SymExprPairToElement(ByteIntervalType BI_) : BI{BI_} {}

    SymExprElementType
    operator()(const SymbolicExpressionMap::value_type& Pair) const {
      return SymExprElementType(BI, Pair.first, Pair.second);
    }
  };

public:
  /// \brief Iterator over \ref SymbolicExpression objects.
  ///
  /// Results are yielded in offset order, ascending.
  using symbolic_expression_iterator =
      boost::transform_iterator<SymExprPairToElement<SymbolicExpressionElement>,
                                SymbolicExpressionMap::iterator>;
  /// \brief Range of \ref SymbolicExpression objects.
  ///
  /// Results are yielded in offset order, ascending.
  using symbolic_expression_range =
      boost::iterator_range<symbolic_expression_iterator>;
  /// \brief Const iterator over \ref SymbolicExpression objects.
  ///
  /// Results are yielded in offset order, ascending.
  using const_symbolic_expression_iterator = boost::transform_iterator<
      SymExprPairToElement<ConstSymbolicExpressionElement>,
      SymbolicExpressionMap::const_iterator>;
  /// \brief Const range of \ref SymbolicExpression objects.
  ///
  /// Results are yielded in offset order, ascending.
  using const_symbolic_expression_range =
      boost::iterator_range<const_symbolic_expression_iterator>;

  /// \brief Return an iterator to the first \ref SymbolicExpression.
  symbolic_expression_iterator symbolic_expressions_begin() {
    return boost::make_transform_iterator(
        SymbolicExpressions.begin(),
        SymExprPairToElement<SymbolicExpressionElement>(this));
  }
  /// \brief Return a const iterator to the first \ref SymbolicExpression.
  const_symbolic_expression_iterator symbolic_expressions_begin() const {
    return boost::make_transform_iterator(
        SymbolicExpressions.begin(),
        SymExprPairToElement<ConstSymbolicExpressionElement>(this));
  }
  /// \brief Return an iterator to the element following the last \ref
  /// SymbolicExpression.
  symbolic_expression_iterator symbolic_expressions_end() {
    return boost::make_transform_iterator(
        SymbolicExpressions.end(),
        SymExprPairToElement<SymbolicExpressionElement>(this));
  }
  /// \brief Return a const iterator to the element following the last \ref
  /// SymbolicExpression.
  const_symbolic_expression_iterator symbolic_expressions_end() const {
    return boost::make_transform_iterator(
        SymbolicExpressions.end(),
        SymExprPairToElement<ConstSymbolicExpressionElement>(this));
  }
  /// \brief Return a range of the \ref SymbolicExpression objects in this
  /// interval.
  symbolic_expression_range symbolic_expressions() {
    return boost::make_iterator_range(symbolic_expressions_begin(),
                                      symbolic_expressions_end());
  }
  /// \brief Return a const range of the \ref SymbolicExpression objects in this
  /// interval.
  const_symbolic_expression_range symbolic_expressions() const {
    return boost::make_iterator_range(symbolic_expressions_begin(),
                                      symbolic_expressions_end());
  }

  symbolic_expression_range findSymbolicExpressionsAtOffset(uint64_t Off) {
    auto Pair = SymbolicExpressions.equal_range(Off);
    return boost::make_iterator_range(
        boost::make_transform_iterator(
            Pair.first, SymExprPairToElement<SymbolicExpressionElement>(this)),
        boost::make_transform_iterator(
            Pair.second,
            SymExprPairToElement<SymbolicExpressionElement>(this)));
  }

  symbolic_expression_range findSymbolicExpressionsAtOffset(uint64_t Low,
                                                            uint64_t High) {
    return boost::make_iterator_range(
        boost::make_transform_iterator(
            SymbolicExpressions.lower_bound(Low),
            SymExprPairToElement<SymbolicExpressionElement>(this)),
        boost::make_transform_iterator(
            SymbolicExpressions.upper_bound(High),
            SymExprPairToElement<SymbolicExpressionElement>(this)));
  }

  symbolic_expression_range findSymbolicExpressionsAt(Addr A) {
    if (!Address) {
      return boost::make_iterator_range(symbolic_expressions_end(),
                                        symbolic_expressions_end());
    }
    return findSymbolicExpressionsAtOffset(A - *Address);
  }

  symbolic_expression_range findSymbolicExpressionsAt(Addr Low, Addr High) {
    if (!Address) {
      return boost::make_iterator_range(symbolic_expressions_end(),
                                        symbolic_expressions_end());
    }
    return findSymbolicExpressionsAtOffset(Low - *Address, High - *Address);
  }

  const_symbolic_expression_range
  findSymbolicExpressionsAtOffset(uint64_t Off) const {
    auto Pair = SymbolicExpressions.equal_range(Off);
    return boost::make_iterator_range(
        boost::make_transform_iterator(
            Pair.first,
            SymExprPairToElement<ConstSymbolicExpressionElement>(this)),
        boost::make_transform_iterator(
            Pair.second,
            SymExprPairToElement<ConstSymbolicExpressionElement>(this)));
  }

  const_symbolic_expression_range
  findSymbolicExpressionsAtOffset(uint64_t Low, uint64_t High) const {
    return boost::make_iterator_range(
        boost::make_transform_iterator(
            SymbolicExpressions.lower_bound(Low),
            SymExprPairToElement<ConstSymbolicExpressionElement>(this)),
        boost::make_transform_iterator(
            SymbolicExpressions.upper_bound(High),
            SymExprPairToElement<ConstSymbolicExpressionElement>(this)));
  }

  const_symbolic_expression_range findSymbolicExpressionsAt(Addr A) const {
    if (!Address) {
      return boost::make_iterator_range(symbolic_expressions_end(),
                                        symbolic_expressions_end());
    }
    return findSymbolicExpressionsAtOffset(A - *Address);
  }

  const_symbolic_expression_range findSymbolicExpressionsAt(Addr Low,
                                                            Addr High) const {
    if (!Address) {
      return boost::make_iterator_range(symbolic_expressions_end(),
                                        symbolic_expressions_end());
    }
    return findSymbolicExpressionsAtOffset(Low - *Address, High - *Address);
  }

  /// \brief Remove a block from this interval.
  ///
  /// \tparam BlockType   Either \ref CodeBlock or \ref DataBlock.
  /// \param  N           The block to remove.
  ///
  /// \return Whether or not the operation succeeded. This operation can
  /// fail if the node to remove is not actually part of this node to begin
  /// with.
  template <class BlockType> bool removeBlock(BlockType* N) {
    N->removeFromIndices();
    auto& Index = Blocks.get<by_pointer>();
    if (auto Iter = Index.find(N); Iter != Index.end()) {
      Index.erase(Iter);
      N->setByteInterval(nullptr);
      return true;
    }
    return false;
  }

  /// \brief Move an existing Block to be a part of this interval.
  ///
  /// \tparam BlockType   Either \ref CodeBlock or \ref DataBlock.
  /// \param  Off           The offset to move the block to.
  /// \param  N           The block to move.
  template <typename BlockType>
  BlockType* addBlock(uint64_t Off, BlockType* N) {
    if (N->getByteInterval()) {
      N->getByteInterval()->removeBlock(N);
    }

    N->setByteInterval(this);
    Blocks.emplace(Off, N);
    N->addToIndices();
    return N;
  }

  /// \brief Adds a new \ref SymbolicExpression to this interval.
  ///
  /// \param  Off       The offset to add the new \ref SymbolicExpression at.
  /// \param  SymExpr   An existing \ref SymbolicExpression to copy into this
  ///                   interval.
  /// \return           The newly created \ref SymbolicExpression.
  SymbolicExpression& addSymbolicExpression(uint64_t Off,
                                            const SymbolicExpression& SymExpr) {
    this->mutateIndices([&]() { SymbolicExpressions.emplace(Off, SymExpr); });
    return SymbolicExpressions[Off];
  }

  /// \brief Removes a \ref SymbolicExpression at the given offset, if present.
  ///
  /// \param Off  The offset of the \ref SymbolicExpression to remove.
  ///
  /// \return Whether or not the operation succeeded. This operation can
  /// fail if the node to remove is not actually part of this node to begin
  /// with.
  bool removeSymbolicExpression(uint64_t Off) {
    std::size_t N;
    this->mutateIndices([&]() { N = SymbolicExpressions.erase(Off); });
    return N != 0;
  }

  /// \brief Get the symbolic expression at the given offset, if present.
  ///
  /// \param Off  The offset of the \ref SymbolicExpression to return.
  /// \return   The \ref SymbolicExpression at that offset, or nullptr if there
  ///           is no \ref SymbolicExpression at that offset.
  SymbolicExpression* getSymbolicExpression(uint64_t Off) {
    if (auto It = SymbolicExpressions.find(Off);
        It != SymbolicExpressions.end()) {
      return &It->second;
    }
    return nullptr;
  }

  /// \brief Get the symbolic expression at the given offset, if present.
  ///
  /// \param Off  The offset of the \ref SymbolicExpression to return.
  /// \return   The \ref SymbolicExpression at that offset, or nullptr if there
  ///           is no \ref SymbolicExpression at that offset.
  const SymbolicExpression* getSymbolicExpression(uint64_t Off) const {
    if (auto It = SymbolicExpressions.find(Off);
        It != SymbolicExpressions.end()) {
      return &It->second;
    }
    return nullptr;
  }

  /// \brief Set or clear the address of this interval.
  ///
  /// \param A  Either the new address, or an empty \ref std::optional if you
  ///           wish to remove the address.
  void setAddress(std::optional<Addr> A) {
    this->mutateIndices([this, A]() { Address = A; });
  }

  /// \brief Get the size of this interval in bytes.
  ///
  /// If this number is greater than the value returned by \ref
  /// getInitializedSize, this indicates that the high addresses taken up by
  /// this interval consist of uninitialized bytes. This often occurs in BSS
  /// sections, where data is zero-initialized rather than stored as zeroes in
  /// the binary.
  uint64_t getSize() const { return Size; }

  /// \brief Set the size of this interval.
  ///
  /// This will also adjust \ref getInitializedSize if the size given is less
  /// than the initialized size.
  ///
  /// \param S  The new size.
  void setSize(uint64_t S) {
    this->mutateIndices([this, S]() { Size = S; });
    if (S < getInitializedSize()) {
      setInitializedSize(S);
    }
  }

  /// \brief Get the number of initialized bytes in this interval.
  ///
  /// Not all bytes in this interval may correspond to bytes physically stored
  /// in the underlying file format. This can occur, for example, in BSS
  /// sections, which are zero-initialized at loadtime, but these zeroes are not
  /// stored in the file itself. If this number is smaller than the value
  /// returned by \ref getSize, this indicates that any bytes past this number
  /// are unitialized bytes with values determined at loadtime. As such, all
  /// bytes past this number in this interval's byte vector are truncated when
  /// saving to file.
  ///
  /// This number will never be larger than the value returned by \ref getSize.
  uint64_t getInitializedSize() const { return Bytes.size(); }

  /// \brief Set the number of initialized bytes in this interval.
  ///
  /// Not all bytes in this interval may correspond to bytes physically stored
  /// in the underlying file format. This can occur, for example, in BSS
  /// sections, which are zero-initialized at loadtime, but these zeroes are not
  /// stored in the file itself. If this number is smaller than the value
  /// returned by \ref getSize, this indicates that any bytes past this number
  /// are unitialized bytes with values determined at loadtime. As such, all
  /// bytes past this number in this interval's byte vector are truncated when
  /// saving to file.
  ///
  /// If the number specified is larger than \ref getSize, then
  /// the byte vector is expanded with zeroes to be equal to the new allocated
  /// size.
  void setInitializedSize(uint64_t S) {
    Bytes.resize(S);
    if (S > getSize()) {
      setSize(S);
    }
  }

private:
  /// \class BytesReference
  ///
  /// \brief A reference to a section of the byte interval, allowing for
  /// seamless reading and writing of chunks of data.
  ///
  /// \tparam ByteIntervalType  Either "ByteInterval" or
  ///                           "const ByteInterval".
  /// \tparam T  The type of the data to iterate over. Must be a POD
  ///            type that satisfies Boost's EndianReversible concept.
  template <typename ByteIntervalType, typename T> class BytesReference {
  public:
    BytesReference(ByteIntervalType& BI_, size_t I_,
                   boost::endian::order InputOrder_,
                   boost::endian::order OutputOrder_)
        : BI(BI_), I(I_), InputOrder(InputOrder_), OutputOrder(OutputOrder_) {}

    /// \brief Use this reference as an rvalue.
    ///
    /// This method automatically handles endian conversions.
    /// If uninitlized bytes are read from, then those bytes are treated as
    /// zeroes.
    operator T() const {
      assert(I + sizeof(T) <= BI.Size &&
             "read into interval's bytes out of bounds!");

      auto S = BI.Bytes.size();

      if (I >= S) {
        // anything this far past the end of initialized bytes is composed of
        // all zero bytes, so we return what T would have been interpreted as if
        // all bytes are zero.
        //
        // (note that you may be tempted to replace this with "return T{};", but
        // beware: T might be a non-scalar type whose default constructor
        // differs from the value returned when all bytes are re-interpeted as
        // zeroes. A similar argument exists for "return T{0};".)
        const std::array<uint8_t, sizeof(T)> Array{};
        return *reinterpret_cast<const T*>(Array.data());
      }

      if (I + sizeof(T) > S) {
        // Here, I < S < I + sizeof(T), so we need to partially fill the
        // initialized bytes and combine it with zeroes for the uninitialized
        // bytes.
        std::array<uint8_t, sizeof(T)> Array{};
        // Thanks to math, 0 < S - I < sizeof(T).
        std::copy_n(BI.Bytes.begin() + I, S - I, Array.begin());
        return boost::endian::conditional_reverse(
            *reinterpret_cast<const T*>(Array.data()), InputOrder, OutputOrder);
      }

      return boost::endian::conditional_reverse(
          *reinterpret_cast<const T*>(BI.Bytes.data() + I), InputOrder,
          OutputOrder);
    }

    /// \brief Use this reference as an lvalue.
    ///
    /// This method automatically handles endian conversions.
    /// If uninitlized bytes are written to, then the initialized byte count is
    /// adjusted (see \ref getInitializedSize for details), padding with zeroes
    /// as necesary.
    BytesReference<ByteIntervalType, T>& operator=(const T& rhs) {
      assert(I + sizeof(T) <= BI.Size &&
             "write into interval's bytes out of bounds!");

      if (I + sizeof(T) > BI.Bytes.size()) {
        BI.Bytes.resize(I + sizeof(T));
      }

      *reinterpret_cast<T*>(BI.Bytes.data() + I) =
          boost::endian::conditional_reverse(rhs, OutputOrder, InputOrder);
      return *this;
    }

    ByteIntervalType& BI;
    size_t I;
    boost::endian::order InputOrder;
    boost::endian::order OutputOrder;
  };

  /// \brief An iterator over the bytes in this byte vector.
  ///
  /// \tparam ByteIntervalType  Either "ByteInterval" or
  ///                           "const ByteInterval".
  /// \tparam T  The type of the data to iterate over. Must be a POD
  ///            type that satisfies Boost's EndianReversible concept.
  template <typename ByteIntervalType, typename T>
  class BytesBaseIterator
      : public boost::iterator_facade<BytesBaseIterator<ByteIntervalType, T>, T,
                                      boost::random_access_traversal_tag,
                                      BytesReference<ByteIntervalType, T>> {
  public:
    using self = BytesBaseIterator<ByteIntervalType, T>;
    using reference = BytesReference<ByteIntervalType, T>;

    BytesBaseIterator(ByteIntervalType& BI_, size_t I_,
                      boost::endian::order InputOrder_,
                      boost::endian::order OutputOrder_)
        : BI(BI_), I(I_), InputOrder(InputOrder_), OutputOrder(OutputOrder_) {}

    // Beginning of functions for iterator facade compatibility.
    reference dereference() const {
      return reference(BI, I, InputOrder, OutputOrder);
    }

    bool equal(const self& other) const {
      return &BI == &other.BI && I == other.I;
    }

    void increment() { I += sizeof(T); }

    void decrement() { I -= sizeof(T); }

    void advance(typename self::difference_type n) { I += n * sizeof(T); }

    typename self::difference_type distance_to(const self& other) const {
      return (other.I - I) / sizeof(T);
    }
    // End of functions for iterator facade compatibility.

    /// \brief Convert this iterator into a const iterator.
    operator BytesBaseIterator<const ByteIntervalType, T>() const {
      return BytesBaseIterator<const ByteIntervalType, T>(BI, I, InputOrder,
                                                          OutputOrder);
    }

  private:
    ByteIntervalType& BI;
    size_t I;
    boost::endian::order InputOrder;
    boost::endian::order OutputOrder;

    friend class ByteInterval;
  };

public:
  /// \brief Iterator over bytes.
  ///
  /// \tparam T  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  template <typename T>
  using bytes_iterator = BytesBaseIterator<ByteInterval, T>;
  /// \brief Range over bytes.
  ///
  /// \tparam T  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  template <typename T>
  using bytes_range = boost::iterator_range<bytes_iterator<T>>;
  /// \brief Const iterator over bytes.
  ///
  /// \tparam T  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  template <typename T>
  using const_bytes_iterator = BytesBaseIterator<const ByteInterval, T>;
  /// \brief Const range over bytes.
  ///
  /// \tparam T  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  template <typename T>
  using const_bytes_range = boost::iterator_range<const_bytes_iterator<T>>;

  /// \brief Get an iterator to the beginning of this byte vector.
  ///
  /// \tparam T  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the vector.
  /// \param  OutputOrder The endianess you wish to read out from the vector.
  template <typename T>
  bytes_iterator<T>
  bytes_begin(boost::endian::order InputOrder = boost::endian::order::native,
              boost::endian::order OutputOrder = boost::endian::order::native) {
    return bytes_iterator<T>(*this, 0, InputOrder, OutputOrder);
  }

  /// \brief Get an iterator past the end of this byte vector.
  ///
  /// \tparam T  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the vector.
  /// \param  OutputOrder The endianess you wish to read out from the vector.
  template <typename T>
  bytes_iterator<T>
  bytes_end(boost::endian::order InputOrder = boost::endian::order::native,
            boost::endian::order OutputOrder = boost::endian::order::native) {
    return bytes_iterator<T>(*this, Size, InputOrder, OutputOrder);
  }

  /// \brief Get a range of data in this byte vector.
  ///
  /// \tparam T  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the vector.
  /// \param  OutputOrder The endianess you wish to read out from the vector.
  template <typename T>
  bytes_range<T>
  bytes(boost::endian::order InputOrder = boost::endian::order::native,
        boost::endian::order OutputOrder = boost::endian::order::native) {
    return bytes_range<T>(bytes_begin<T>(InputOrder, OutputOrder),
                          bytes_end<T>(InputOrder, OutputOrder));
  }

  /// \brief Get an iterator to the beginning of this byte vector.
  ///
  /// \tparam T  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the vector.
  /// \param  OutputOrder The endianess you wish to read out from the vector.
  template <typename T>
  const_bytes_iterator<T> bytes_begin(
      boost::endian::order InputOrder = boost::endian::order::native,
      boost::endian::order OutputOrder = boost::endian::order::native) const {
    return const_bytes_iterator<T>(*this, 0, InputOrder, OutputOrder);
  }

  /// \brief Get an iterator past the end of this byte vector.
  ///
  /// \tparam T  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the vector.
  /// \param  OutputOrder The endianess you wish to read out from the vector.
  template <typename T>
  const_bytes_iterator<T> bytes_end(
      boost::endian::order InputOrder = boost::endian::order::native,
      boost::endian::order OutputOrder = boost::endian::order::native) const {
    return const_bytes_iterator<T>(*this, Size, InputOrder, OutputOrder);
  }

  /// \brief Get a range of data in this byte vector.
  ///
  /// \tparam T  The type of data stored in this byte vector. Must be a
  /// POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the vector.
  /// \param  OutputOrder The endianess you wish to read out from the vector.
  template <typename T>
  const_bytes_range<T>
  bytes(boost::endian::order InputOrder = boost::endian::order::native,
        boost::endian::order OutputOrder = boost::endian::order::native) const {
    return const_bytes_range<T>(bytes_begin<T>(InputOrder, OutputOrder),
                                bytes_end<T>(InputOrder, OutputOrder));
  }

  /// \brief Insert a single datum into this byte vector.
  ///
  /// \tparam T  The type of data you wish to insert into the byte
  /// vector. Must be a POD type that satisfies Boost's EndianReversible
  /// concept.
  ///
  /// \param  Pos           The position in the byte vector to insert data at.
  /// \param  X             The data to insert.
  /// \param  VectorOrder   The endianess of the data in the byte vector.
  /// \param  ElementOrder  The endianess of the data to be inserted.
  ///
  /// \return An iterator pointing to the element inserted by this call.
  template <typename T>
  const_bytes_iterator<T> insertBytes(
      const const_bytes_iterator<T> Pos, const T& X,
      boost::endian::order VectorOrder = boost::endian::order::native,
      boost::endian::order ElementOrder = boost::endian::order::native) {
    setSize(Size + sizeof(T));
    // If the position to insert is currently outside the initilized bytes,
    // we let the iterator's operator= handle resizing the byte vector,
    // otherwise we insert zeroes and then overwrite them via said operator=.
    if (Pos.I < Bytes.size()) {
      Bytes.insert(Bytes.begin() + Pos.I, sizeof(T), 0);
    }
    *bytes_iterator<T>(*this, Pos.I, ElementOrder, VectorOrder) = X;
    return Pos;
  }

  /// \brief Insert data into this byte vector.
  ///
  /// \tparam T  The type of data you wish to insert into the byte
  /// vector. Must be a POD type that satisfies Boost's EndianReversible
  /// concept.
  ///
  /// \tparam InputIterator      The type of an iterator yielding T.
  ///
  /// \param  Pos           The position in the byte vector to insert data at.
  /// \param  Begin         The start of the data to insert.
  /// \param  End           The end of the data to insert.
  /// \param  VectorOrder   The endianess of the data in the byte vector.
  /// \param  ElementsOrder The endianess of the data to be inserted.
  ///
  /// \return An iterator pointing to the first element inserted by this call.
  template <typename T, typename InputIterator>
  const_bytes_iterator<T> insertBytes(
      const const_bytes_iterator<T> Pos, InputIterator Begin, InputIterator End,
      boost::endian::order VectorOrder = boost::endian::order::native,
      boost::endian::order ElementsOrder = boost::endian::order::native) {
    auto N = std::distance(Begin, End) * sizeof(T);
    setSize(Size + N);
    // If the position to insert is currently outside the initilized bytes,
    // we let the iterator's operator= handle resizing the byte vector,
    // otherwise we insert zeroes and then overwrite them via said operator=.
    if (Pos.I < Bytes.size()) {
      Bytes.insert(Bytes.begin() + Pos.I, N, 0);
    }
    // std::copy calls operator= one time for every element in the input iter.
    std::copy(Begin, End,
              bytes_iterator<T>(*this, Pos.I, VectorOrder, ElementsOrder));
    return Pos;
  }

  /// \brief Erase data from this byte vector.
  ///
  /// \tparam T  The type of data you wish to erase.
  ///
  /// \param  Begin The start of the data to erase.
  /// \param  End   The end of the data to erase.
  ///
  /// \return An iterator pointing to the first element after those erased by
  /// this call.
  template <typename T>
  const_bytes_iterator<T> eraseBytes(const const_bytes_iterator<T> Begin,
                                     const const_bytes_iterator<T> End) {
    assert(Begin.I <= End.I && "eraseBytes: Begin > End!");
    assert(Begin.I <= Size && "eraseBytes: Begin out of range!");
    assert(End.I <= Size && "eraseBytes: End out of range!");

    // If the beginning iter is outside the init vector, nothing need be done.
    if (Begin.I < Bytes.size()) {
      if (End.I < Bytes.size()) {
        // All positions are within the initilized vector.
        Bytes.erase(Bytes.begin() + Begin.I, Bytes.begin() + End.I);
      } else {
        // The beginning is within vector, the end isn't; clamp to Bytes.end().
        Bytes.erase(Bytes.begin() + Begin.I, Bytes.end());
      }
    }

    setSize(Size - (End.I - Begin.I));
    return Begin;
  }

  /// \brief Return the raw data underlying this byte vector.
  ///
  /// Much like \ref std::vector::data, this function is low-level and
  /// potentially unsafe. This pointer refers to valid memory only where an
  /// iterator would be valid to point to. Modifying the size of the byte
  /// vector may invalidate this pointer. Any endian conversions will not be
  /// performed.
  ///
  /// \tparam T The type of data stored in this byte vector. Must be a POD type.
  template <typename T> T* rawBytes() {
    return reinterpret_cast<T*>(Bytes.data());
  }

  /// \brief Return the raw data underlying this byte vector.
  ///
  /// Much like \ref std::vector::data, this function is low-level and
  /// potentially unsafe. This pointer refers to valid memory only where an
  /// iterator would be valid to point to. Modifying the size of the byte
  /// vector may invalidate this pointer. Any endian conversions will not be
  /// performed.
  ///
  /// \tparam T The type of data stored in this byte vector. Must be a POD type.
  template <typename T> const T* rawBytes() const {
    return reinterpret_cast<const T*>(Bytes.data());
  }

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing ByteInterval.
  using MessageType = proto::ByteInterval;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a ByteInterval from a protobuf message.
  ///
  /// \param C  The Context in which the deserialized ByteInterval will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized ByteInterval object, or null on failure.
  static ByteInterval* fromProtobuf(Context& C, Section* Parent,
                                    const MessageType& Message);

  /// \brief Populate symbolic expressions from a Protobuf message.
  ///
  /// \param C  The Context in which the deserialized SymbolicExpressions will
  /// be held.
  ///
  /// \param Message  The protobuf message from which to deserialize.
  void symbolicExpressionsFromProtobuf(Context& C, const MessageType& Message);

  static bool classof(const Node* N) {
    return N->getKind() == Kind::ByteInterval;
  }
  /// @endcond

private:
  ByteInterval(Context& C) : Node(C, Kind::ByteInterval) {}

  ByteInterval(Context& C, std::optional<Addr> A, uint64_t S, uint64_t InitSize)
      : Node(C, Kind::ByteInterval), Address(A), Size(S), Bytes(InitSize) {}

  template <typename InputIterator>
  ByteInterval(Context& C, std::optional<Addr> A, uint64_t S, uint64_t InitSize,
               InputIterator Begin, InputIterator End)
      : Node(C, Kind::ByteInterval), Address(A), Size(S), Bytes(Begin, End) {
    Bytes.resize(InitSize);
  }

  void setSection(Section* S) { Parent = S; }

  Section* Parent{nullptr};
  std::optional<Addr> Address;
  uint64_t Size{0};
  BlockSet Blocks;
  BlockIntMap BlockAddrs;
  SymbolicExpressionMap SymbolicExpressions;
  std::vector<uint8_t> Bytes;

  friend class Context;   // Friend to enable Context::Create.
  friend class Section;   // Friend to enable Section::(re)moveByteInterval.
  friend class CodeBlock; // Friend to enable CodeBlock::getAddress.
  friend class DataBlock; // Friend to enable DataBlock::getAddress.
  friend class Node;      // Allow Node::mutateIndices, etc. to set indices.
};
} // namespace gtirb

#endif // GTIRB_BYTE_INTERVAL_H
