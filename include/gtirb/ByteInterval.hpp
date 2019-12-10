//===- ByteInterval.hpp -----------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018 GrammaTech, Inc.
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

#include <gtirb/CodeBlock.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <boost/iterator/filter_iterator.hpp>
#include <boost/iterator/iterator_traits.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/key_extractors.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/range/iterator_range.hpp>
#include <map>
#include <optional>
#include <variant>
#include <vector>

/// \file ByteInterval.hpp
/// \brief Class gtirb::ByteInterval.

namespace proto {
class ByteInterval;
} // namespace proto

namespace gtirb {
class Module;
class Section;

/// \class Block
///
/// \brief An entity with an offset held within this interval.
class Block {
  friend class ByteInterval;
  uint64_t offset;
  Node* node;

public:
  Block(uint64_t o, Node* n) : offset(o), node(n) {}

  uint64_t getOffset() const { return offset; }

  Node* getNode() const { return node; }
};

/// @cond INTERNAL
/// \class BlockIs
///
/// \brief A predicate object to discern one type of block.
template <Node::Kind K> struct BlockIs {
  bool operator()(const Block& Variant) {
    return Variant.getNode()->getKind() == K;
  }
};
/// @endcond

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
  using SymbolicExpressionSet = std::map<uint64_t, SymbolicExpression>;
  using ByteVector = std::vector<uint8_t>;

  const Block& nodeToBlock(const Node* N) const {
    auto& index = Blocks.get<by_pointer>();
    auto it = index.find((Node*)N);
    if (it != index.end()) {
      return *it;
    } else {
      throw std::runtime_error(
          "ByteInterval::nodeToBlock called with block not in interval");
    }
  }

public:
  /// \brief Create an unitialized ByteInterval object.
  /// \param C        The Context in which this ByteInterval will be held.
  /// \return         The newly created ByteInterval.
  static ByteInterval* Create(Context& C) { return C.Create<ByteInterval>(C); }

  /// \brief Create a ByteInterval object.
  /// \param C        The Context in which this interval will be held.
  /// \param Parent   The \ref Section this interval belongs to.
  /// \param Address  An (optional) fixed address for this interval.
  /// \param Size     The size of this interval in bytes.
  /// \return         The newly created ByteInterval.
  static ByteInterval* Create(Context& C, Section* Parent,
                              std::optional<Addr> Address, uint64_t Size) {
    return C.Create<ByteInterval>(C, Parent, Address, Size);
  }

  /// \brief Get the \ref Section this byte interval belongs to.
  Section* getSection() const { return Parent; }

  /// \brief Get the fixed address of this interval, if present.
  ///
  /// If this field is present, it may indicate the original address at which
  /// this interval was located at in memory, or it may indicate that this
  /// block's address is fixed and must not be changed. If this field is not
  /// present, it indicates that the interval is free to be moved around in
  /// memory while preserving program semantics.
  std::optional<Addr> getAddress() const { return Address; }

  /// \brief Get the size of this interval in bytes.
  ///
  /// This number may not always be the size of the byte array returned by \ref
  /// getBytes. If this number is larger than the size of this interval's byte
  /// array, this indicates that the high addresses taken up by this interval
  /// consist of uninitialized bytes. This often occurs in BSS sections, where
  /// data is zero-initialized rather than stored as zeroes in the binary.
  ///
  /// It is an error to have an interval in which this number is less than the
  /// size of the byte array returned by \ref getBytes.
  uint64_t getSize() const { return Size; }

  /// \brief Get the bytes stored in this interval.
  ///
  /// \ref CodeBlock and \ref DataBlock objects indicate that ranges of these
  /// bytes belong to program code and data, respectively. They are stored here
  /// rather than in the blocks themselves in the case that blocks overlap. Code
  /// blocks can overlap in some ISAs where, for example, jumps are allowed into
  /// the middle of multibyte instructions, and data blocks can overlap in the
  /// case where, for example, elements of arrays are accessed in a static
  /// manner.
  ByteVector& getBytes() { return Bytes; }
  /// \brief Get the bytes stored in this interval.
  ///
  /// \ref CodeBlock and \ref DataBlock objects indicate that ranges of these
  /// bytes belong to program code and data, respectively. They are stored here
  /// rather than in the blocks themselves in the case that blocks overlap. Code
  /// blocks can overlap in some ISAs where, for example, jumps are allowed into
  /// the middle of multibyte instructions, and data blocks can overlap in the
  /// case where, for example, elements of arrays are accessed in a static
  /// manner.
  const ByteVector& getBytes() const { return Bytes; }

  /// \brief Iterator over \ref Block objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using blocks_iterator = BlockSet::index<by_offset>::type::iterator;
  /// \brief Range of \ref Block objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using blocks_range = boost::iterator_range<blocks_iterator>;
  /// \brief Const iterator over \ref Block objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_blocks_iterator =
      BlockSet::index<by_offset>::type::const_iterator;
  /// \brief Const range of \ref Block objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_blocks_range = boost::iterator_range<const_blocks_iterator>;

  /// \brief Return an iterator to the first \ref Block.
  blocks_iterator blocks_begin() { return Blocks.begin(); }
  /// \brief Return a const iterator to the first \ref Block.
  const_blocks_iterator blocks_begin() const { return Blocks.begin(); }
  /// \brief Return an iterator to the element following the last \ref Block.
  blocks_iterator blocks_end() { return Blocks.end(); }
  /// \brief Return a const iterator to the element following the last
  /// \ref Block.
  const_blocks_iterator blocks_end() const { return Blocks.end(); }
  /// \brief Return a range of the \ref Block objects in this interval.
  blocks_range blocks() {
    return boost::make_iterator_range(blocks_begin(), blocks_end());
  }
  /// \brief Return a const range of the \ref Block objects in this interval.
  const_blocks_range blocks() const {
    return boost::make_iterator_range(blocks_begin(), blocks_end());
  }

  /// \brief Iterator over \ref CodeBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using code_blocks_iterator =
      boost::filter_iterator<BlockIs<Node::Kind::CodeBlock>,
                             BlockSet::index<by_offset>::type::iterator>;
  /// \brief Range of \ref CodeBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using code_blocks_range = boost::iterator_range<code_blocks_iterator>;
  /// \brief Const iterator over \ref CodeBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_code_blocks_iterator =
      boost::filter_iterator<BlockIs<Node::Kind::CodeBlock>,
                             BlockSet::index<by_offset>::type::const_iterator>;
  /// \brief Const range of \ref CodeBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_code_blocks_range =
      boost::iterator_range<const_code_blocks_iterator>;

  /// \brief Return an iterator to the first \ref CodeBlock.
  code_blocks_iterator code_blocks_begin() {
    return code_blocks_iterator(decltype(code_blocks_iterator().predicate())(),
                                Blocks.begin(), Blocks.end());
  }
  /// \brief Return a const iterator to the first \ref CodeBlock.
  const_code_blocks_iterator code_blocks_begin() const {
    return const_code_blocks_iterator(
        decltype(const_code_blocks_iterator().predicate())(), Blocks.begin(),
        Blocks.end());
  }
  /// \brief Return an iterator to the element following the last \ref
  /// CodeBlock.
  code_blocks_iterator code_blocks_end() {
    return code_blocks_iterator(decltype(code_blocks_iterator().predicate())(),
                                Blocks.end(), Blocks.end());
  }
  /// \brief Return a const iterator to the element following the last \ref
  /// CodeBlock.
  const_code_blocks_iterator code_blocks_end() const {
    return const_code_blocks_iterator(
        decltype(const_code_blocks_iterator().predicate())(), Blocks.end(),
        Blocks.end());
  }
  /// \brief Return a range of the \ref CodeBlock objects in this interval.
  code_blocks_range code_blocks() {
    return boost::make_iterator_range(code_blocks_begin(), code_blocks_end());
  }
  /// \brief Return a const range of the \ref CodeBlock objects in this
  /// interval.
  const_code_blocks_range code_blocks() const {
    return boost::make_iterator_range(code_blocks_begin(), code_blocks_end());
  }

  /// \brief Iterator over \ref DataBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using data_blocks_iterator =
      boost::filter_iterator<BlockIs<Node::Kind::DataBlock>,
                             BlockSet::index<by_offset>::type::iterator>;
  /// \brief Range of \ref DataBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using data_blocks_range = boost::iterator_range<data_blocks_iterator>;
  /// \brief Const iterator over \ref DataBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_data_blocks_iterator =
      boost::filter_iterator<BlockIs<Node::Kind::DataBlock>,
                             BlockSet::index<by_offset>::type::const_iterator>;
  /// \brief Const range of \ref DataBlock objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_data_blocks_range =
      boost::iterator_range<const_data_blocks_iterator>;

  /// \brief Return an iterator to the first \ref DataBlock.
  data_blocks_iterator data_blocks_begin() {
    return data_blocks_iterator(decltype(data_blocks_iterator().predicate())(),
                                Blocks.begin(), Blocks.end());
  }
  /// \brief Return a const iterator to the first \ref DataBlock.
  const_data_blocks_iterator data_blocks_begin() const {
    return const_data_blocks_iterator(
        decltype(const_data_blocks_iterator().predicate())(), Blocks.begin(),
        Blocks.end());
  }
  /// \brief Return an iterator to the element following the last \ref
  /// DataBlock.
  data_blocks_iterator data_blocks_end() {
    return data_blocks_iterator(decltype(data_blocks_iterator().predicate())(),
                                Blocks.end(), Blocks.end());
  }
  /// \brief Return a const iterator to the element following the last \ref
  /// DataBlock.
  const_data_blocks_iterator data_blocks_end() const {
    return const_data_blocks_iterator(
        decltype(const_data_blocks_iterator().predicate())(), Blocks.end(),
        Blocks.end());
  }
  /// \brief Return a range of the \ref DataBlock objects in this interval.
  data_blocks_range data_blocks() {
    return boost::make_iterator_range(data_blocks_begin(), data_blocks_end());
  }
  /// \brief Return a const range of the \ref DataBlock objects in this
  /// interval.
  const_data_blocks_range data_blocks() const {
    return boost::make_iterator_range(data_blocks_begin(), data_blocks_end());
  }

  /// \brief Iterator over \ref SymbolicExpression objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using symbolic_expressions_iterator = SymbolicExpressionSet::iterator;
  /// \brief Range of \ref SymbolicExpression objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using symbolic_expressions_range =
      boost::iterator_range<symbolic_expressions_iterator>;
  /// \brief Const iterator over \ref SymbolicExpression objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_symbolic_expressions_iterator =
      SymbolicExpressionSet::const_iterator;
  /// \brief Const range of \ref SymbolicExpression objects.
  ///
  /// Blocks are yielded in offset order, ascending. If two blocks have the
  /// same offset, thier order is not specified.
  using const_symbolic_expressions_range =
      boost::iterator_range<const_symbolic_expressions_iterator>;

  /// \brief Return an iterator to the first \ref SymbolicExpression.
  symbolic_expressions_iterator symbolic_expressions_begin() {
    return SymbolicExpressions.begin();
  }
  /// \brief Return a const iterator to the first \ref SymbolicExpression.
  const_symbolic_expressions_iterator symbolic_expressions_begin() const {
    return SymbolicExpressions.begin();
  }
  /// \brief Return an iterator to the element following the last \ref
  /// SymbolicExpression.
  symbolic_expressions_iterator symbolic_expressions_end() {
    return SymbolicExpressions.end();
  }
  /// \brief Return a const iterator to the element following the last \ref
  /// SymbolicExpression.
  const_symbolic_expressions_iterator symbolic_expressions_end() const {
    return SymbolicExpressions.end();
  }
  /// \brief Return a range of the \ref SymbolicExpression objects in this
  /// interval.
  symbolic_expressions_range symbolic_expressions() {
    return boost::make_iterator_range(symbolic_expressions_begin(),
                                      symbolic_expressions_end());
  }
  /// \brief Return a const range of the \ref SymbolicExpression objects in this
  /// interval.
  const_symbolic_expressions_range symbolic_expressions() const {
    return boost::make_iterator_range(symbolic_expressions_begin(),
                                      symbolic_expressions_end());
  }

  /// \brief Remove a block from this interval.
  ///
  /// \tparam BlockType   Either \ref CodeBlock or \ref DataBlock.
  template <class BlockType> void removeBlock(BlockType* N) {
    auto& index = Blocks.get<by_pointer>();
    index.erase(index.find(N));
    N->setByteInerval(nullptr);
  }

  /// \brief Move an existing Block to be a part of this interval.
  ///
  /// \tparam BlockType   Either \ref CodeBlock or \ref DataBlock.
  template <typename BlockType> void moveBlock(uint64_t O, BlockType* N) {
    if (N->getByteInterval()) {
      N->getByteInterval()->removeBlock(N);
    }

    N->setByteInterval(this);
    Blocks.emplace(O, N);
  }

  /// \brief Creates a new \ref CodeBlock at a given offset.
  ///
  /// \tparam Args  The arguments to construct a \ref CodeBlock.
  template <typename... Args>
  CodeBlock* addCodeBlock(Context& C, uint64_t O, Args... A) {
    auto N = CodeBlock::Create(C, this, A...);
    Blocks.emplace(O, N);
    return N;
  }

  /// \brief Creates a new \ref DataBlock at a given offset.
  ///
  /// \tparam Args  The arguments to construct a \ref DataBlock.
  template <typename... Args>
  DataBlock* addDataBlock(Context& C, uint64_t O, Args... A) {
    auto N = DataBlock::Create(C, this, A...);
    Blocks.emplace(O, N);
    return N;
  }

  template <class ExprType, class... Args>
  SymbolicExpression& addSymbolicExpression(uint64_t O, Args... A) {
    SymbolicExpressions.emplace(O, ExprType{A...});
    return SymbolicExpressions[O];
  }

  void removeSymbolicExpression(uint64_t O) { SymbolicExpressions.erase(O); }

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
  /// @endcond

private:
  ByteInterval(Context& C) : Node(C, Kind::ByteInterval) {}
  ByteInterval(Context& C, Section* P, std::optional<Addr> A, uint64_t S)
      : Node(C, Kind::ByteInterval), Parent(P), Address(A), Size(S) {}

  void setSection(Section* S) { Parent = S; }

  Section* Parent;
  std::optional<Addr> Address{};
  uint64_t Size{0};
  BlockSet Blocks;
  SymbolicExpressionSet SymbolicExpressions;
  ByteVector Bytes;

  friend class Context;   // to enable Context::Create
  friend class Section;   // to enable Section::(re)moveByteInterval
  friend class CodeBlock; // to enable CodeBlock::getAddress
  friend class DataBlock; // to enable DataBlock::getAddress
};
} // namespace gtirb

#endif // GTIRB_BYTE_INTERVAL_H
