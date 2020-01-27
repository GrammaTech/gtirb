//===- Section.hpp ----------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_SECTION_H
#define GTIRB_SECTION_H

#include <gtirb/Addr.hpp>
#include <gtirb/ByteInterval.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/Utility.hpp>
#include <boost/icl/interval_map.hpp>
#include <boost/iterator/indirect_iterator.hpp>
#include <boost/iterator/iterator_traits.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/key_extractors.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/range/iterator_range.hpp>
#include <cstdint>

/// \file Section.hpp
/// \brief Class gtirb::Section.

namespace proto {
class Section;
}

namespace gtirb {
class Module; // Forward declared for the backpointer.

/// \class Section
///
/// \brief Represents a named section of the binary.
///
/// Does not directly store the contents of the section, which are kept in
/// \ref ImageByteMap.
class GTIRB_EXPORT_API Section : public Node {
  Section(Context& C) : Node(C, Kind::Section) {}
  Section(Context& C, Module* P, const std::string& N)
      : Node(C, Kind::Section), Parent(P), Name(N) {}

  struct by_address {};
  struct by_pointer {};

  using ByteIntervalSet = boost::multi_index::multi_index_container<
      ByteInterval*,
      boost::multi_index::indexed_by<
          boost::multi_index::ordered_non_unique<
              boost::multi_index::tag<by_address>,
              boost::multi_index::global_fun<
                  const ByteInterval&, AddressLess<ByteInterval>::key_type,
                  &AddressLess<ByteInterval>::key>>,
          boost::multi_index::hashed_unique<
              boost::multi_index::tag<by_pointer>,
              boost::multi_index::identity<ByteInterval*>>>>;
  using ByteIntervalIntMap = boost::icl::interval_map<
      Addr, std::set<ByteInterval*, AddressLess<ByteInterval>>>;

public:
  /// \brief Create an unitialized Section object.
  /// \param C        The Context in which this Section will be held.
  /// \return         The newly created Section.
  static Section* Create(Context& C) { return C.Create<Section>(C); }

  /// \brief Create a Section object.
  ///
  /// \param C        The Context in which this object will be held.
  /// \param Parent   The \ref Module this section belongs to.
  /// \param Name     The name of the section.
  ///
  /// \return The newly created object.
  static Section* Create(Context& C, Module* Parent, const std::string& Name) {
    return C.Create<Section>(C, Parent, Name);
  }

  /// \brief Equality operator overload.
  bool operator==(const Section& Other) const;

  /// \brief Inequality operator overload.
  bool operator!=(const Section& Other) const;

  /// \brief Get the \ref Module this section belongs to.
  Module* getModule() { return Parent; }
  /// \brief Get the \ref Module this section belongs to.
  const Module* getModule() const { return Parent; }

  /// \brief Get the name of a Section.
  ///
  /// \return The name.
  const std::string& getName() const { return Name; }

  /// \brief Iterator over \ref ByteInterval objects.
  using byte_interval_iterator =
      boost::indirect_iterator<ByteIntervalSet::iterator>;
  /// \brief Range of \ref ByteInterval objects.
  using byte_interval_range = boost::iterator_range<byte_interval_iterator>;
  /// \brief Sub-range of \ref ByteInterval objects overlapping addresses.
  using byte_interval_subrange = boost::iterator_range<
      boost::indirect_iterator<ByteIntervalIntMap::codomain_type::iterator>>;
  /// \brief Const iterator over \ref ByteInterval objects.
  using const_byte_interval_iterator =
      boost::indirect_iterator<ByteIntervalSet::const_iterator,
                               const ByteInterval>;
  /// \brief Const range of \ref ByteInterval objects.
  using const_byte_interval_range =
      boost::iterator_range<const_byte_interval_iterator>;
  /// \brief Const sub-range of \ref ByteInterval objects overlapping addresses.
  using const_byte_interval_subrange =
      boost::iterator_range<boost::indirect_iterator<
          ByteIntervalIntMap::codomain_type::const_iterator>>;

  /// \brief Return an iterator to the first \ref ByteInterval.
  byte_interval_iterator byte_intervals_begin() {
    return ByteIntervals.begin();
  }
  /// \brief Return a const iterator to the first \ref ByteInterval.
  const_byte_interval_iterator byte_intervals_begin() const {
    return ByteIntervals.begin();
  }
  /// \brief Return an iterator to the element following the last \ref
  /// ByteInterval.
  byte_interval_iterator byte_intervals_end() { return ByteIntervals.end(); }
  /// \brief Return a const iterator to the element following the last
  /// \ref ByteInterval.
  const_byte_interval_iterator byte_intervals_end() const {
    return ByteIntervals.end();
  }
  /// \brief Return a range of the \ref ByteInterval objects in this section.
  byte_interval_range byte_intervals() {
    return boost::make_iterator_range(byte_intervals_begin(),
                                      byte_intervals_end());
  }
  /// \brief Return a const range of the \ref ByteInterval objects in this
  /// section.
  const_byte_interval_range byte_intervals() const {
    return boost::make_iterator_range(byte_intervals_begin(),
                                      byte_intervals_end());
  }

  /// \brief Find all the intervals that have bytes that lie within the address
  /// specified.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref ByteInterval objects that intersect the address \p
  /// A.
  byte_interval_subrange findByteIntervalsIn(Addr A) {
    if (auto It = ByteIntervalAddrs.find(A); It != ByteIntervalAddrs.end()) {
      return boost::make_iterator_range(
          boost::make_indirect_iterator(It->second.begin()),
          boost::make_indirect_iterator(It->second.end()));
    }
    return {};
  }

  /// \brief Find all the intervals that have bytes that lie within the address
  /// specified.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref ByteInterval objects that intersect the address \p
  /// A.
  const_byte_interval_subrange findByteIntervalsIn(Addr A) const {
    if (auto It = ByteIntervalAddrs.find(A); It != ByteIntervalAddrs.end()) {
      return boost::make_iterator_range(
          boost::make_indirect_iterator(It->second.begin()),
          boost::make_indirect_iterator(It->second.end()));
    }
    return {};
  }

  /// \brief Find all the intervals that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref ByteInterval objects that are at the address \p A.
  byte_interval_range findByteIntervalsAt(Addr A) {
    auto Pair = ByteIntervals.get<by_address>().equal_range(A);
    return boost::make_iterator_range(byte_interval_iterator(Pair.first),
                                      byte_interval_iterator(Pair.second));
  }

  /// \brief Find all the intervals that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref ByteInterval objects that are between the
  /// addresses.
  byte_interval_range findByteIntervalsAt(Addr Low, Addr High) {
    auto& Index = ByteIntervals.get<by_address>();
    return boost::make_iterator_range(
        byte_interval_iterator(Index.lower_bound(Low)),
        byte_interval_iterator(Index.lower_bound(High)));
  }

  /// \brief Find all the intervals that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref ByteInterval objects that are at the address \p A.
  const_byte_interval_range findByteIntervalsAt(Addr A) const {
    auto Pair = ByteIntervals.get<by_address>().equal_range(A);
    return boost::make_iterator_range(
        const_byte_interval_iterator(Pair.first),
        const_byte_interval_iterator(Pair.second));
  }

  /// \brief Find all the intervals that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref ByteInterval objects that are between the
  /// addresses.
  const_byte_interval_range findByteIntervalsAt(Addr Low, Addr High) const {
    auto& Index = ByteIntervals.get<by_address>();
    return boost::make_iterator_range(
        const_byte_interval_iterator(Index.lower_bound(Low)),
        const_byte_interval_iterator(Index.lower_bound(High)));
  }

  /// \brief Return the address of this section, if known.
  ///
  /// The address is calculated from the \ref ByteInterval objects in this
  /// section. More specifically, if the address of all byte intervals in this
  /// section are fixed, then it will return the address of the interval lowest
  /// in memory. If any one interval does not have an address, then this
  /// function will return \ref std::nullopt, as the address is not calculable
  /// in that case. Note that a section with no intervals in it has no address
  /// or size, so it will return \ref std::nullopt in that case.
  std::optional<Addr> getAddress() const {
    if (ByteIntervals.empty()) {
      return std::nullopt;
    }

    Addr result{std::numeric_limits<Addr::value_type>::max()};
    for (const auto* Interval : ByteIntervals) {
      if (auto A = Interval->getAddress()) {
        result = std::min(result, *A);
      } else {
        return std::nullopt;
      }
    }
    return result;
  }

  /// \brief Return the size of this section, if known.
  ///
  /// The size is calculated from the \ref ByteInterval objects in this section.
  /// More specifically, if the address of all byte intervals in this section
  /// are fixed, then it will return the difference between the lowest and
  /// highest address among the intervals. If any one interval does not have an
  /// address, then this function will return \ref std::nullopt, as the size is
  /// not calculable in that case. Note that a section with no intervals in it
  /// has no address or size, so it will return \ref std::nullopt in that case.
  std::optional<uint64_t> getSize() const {
    if (ByteIntervals.empty()) {
      return std::nullopt;
    }

    Addr LowAddr{std::numeric_limits<Addr::value_type>::max()};
    Addr HighAddr{0};

    for (const auto* Interval : ByteIntervals) {
      if (auto A = Interval->getAddress()) {
        LowAddr = std::min(LowAddr, *A);
        HighAddr = std::max(HighAddr, *A + Interval->getSize());
      } else {
        return std::nullopt;
      }
    }

    return static_cast<uint64_t>(HighAddr - LowAddr);
  }

  /// \brief Remove an interval from this section.
  ///
  /// \return Whether or not the operation succeeded. This operation can
  /// fail if the node to remove is not actually part of this node to begin
  /// with.
  bool removeByteInterval(ByteInterval* N) {
    N->removeFromIndices();
    auto& Index = ByteIntervals.get<by_pointer>();
    if (auto Iter = Index.find(N); Iter != Index.end()) {
      Index.erase(Iter);
      N->setSection(nullptr);
      return true;
    }
    return false;
  }

  /// \brief Move an existing \ref ByteInterval to be a part of this section.
  ByteInterval* addByteInterval(ByteInterval* N) {
    if (N->getSection()) {
      N->getSection()->removeByteInterval(N);
    }
    N->setSection(this);
    N->addToIndices();
    this->mutateIndices([this, N]() { ByteIntervals.emplace(N); });
    return N;
  }

  /// \brief Creates a new \ref ByteInterval in this section.
  ///
  /// \tparam Args  The arguments to construct a \ref ByteInterval.
  /// \param  C     The Context in which this object will be held.
  /// \param  A     The arguments to construct a \ref ByteInterval.
  template <typename... Args>
  ByteInterval* addByteInterval(Context& C, Args... A) {
    return addByteInterval(ByteInterval::Create(C, this, A...));
  }

  /// \brief Set this section's name.
  void setName(const std::string& N) {
    this->mutateIndices([this, &N]() { Name = N; });
  }

  /// \brief Iterator over blocks.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using block_iterator =
      MergeSortedIterator<ByteInterval::block_iterator, BlockAddressLess>;
  /// \brief Range of blocks.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using block_range = boost::iterator_range<block_iterator>;
  /// \brief Sub-range of blocks overlapping an address or range of addreses.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using block_subrange = boost::iterator_range<MergeSortedIterator<
      ByteInterval::block_subrange::iterator, BlockAddressLess>>;
  /// \brief Const iterator over blocks.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_block_iterator =
      MergeSortedIterator<ByteInterval::const_block_iterator, BlockAddressLess>;
  /// \brief Const range of blocks.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_block_range = boost::iterator_range<const_block_iterator>;
  /// \brief Const sub-range of blocks overlapping an address or range of
  /// addreses.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_block_subrange = boost::iterator_range<MergeSortedIterator<
      ByteInterval::const_block_subrange::iterator, BlockAddressLess>>;

  /// \brief Return an iterator to the first block.
  block_iterator blocks_begin() {
    return block_iterator(
        boost::make_transform_iterator(this->byte_intervals_begin(),
                                       NodeToBlockRange<ByteInterval>()),
        boost::make_transform_iterator(this->byte_intervals_end(),
                                       NodeToBlockRange<ByteInterval>()));
  }

  /// \brief Return an iterator to the element following the last block.
  block_iterator blocks_end() { return block_iterator(); }

  /// \brief Return a range of all the blocks.
  block_range blocks() {
    return boost::make_iterator_range(blocks_begin(), blocks_end());
  }

  /// \brief Return an iterator to the first block.
  const_block_iterator blocks_begin() const {
    return const_block_iterator(
        boost::make_transform_iterator(this->byte_intervals_begin(),
                                       NodeToBlockRange<const ByteInterval>()),
        boost::make_transform_iterator(this->byte_intervals_end(),
                                       NodeToBlockRange<const ByteInterval>()));
  }

  /// \brief Return an iterator to the element following the last block.
  const_block_iterator blocks_end() const { return const_block_iterator(); }

  /// \brief Return a range of all the blocks.
  const_block_range blocks() const {
    return boost::make_iterator_range(blocks_begin(), blocks_end());
  }

  /// \brief Find all the blocks that have bytes that lie within the address
  /// specified.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref Node objects, which are either \ref DataBlock
  /// objects or \ref CodeBlock objects, that intersect the address \p A.
  block_subrange findBlocksIn(Addr A) {
    return block_subrange(
        block_subrange::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocksIn<ByteInterval>(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocksIn<ByteInterval>(A))),
        block_subrange::iterator());
  }

  /// \brief Find all the blocks that have bytes that lie within the address
  /// specified.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref Node objects, which are either \ref DataBlock
  /// objects or \ref CodeBlock objects, that intersect the address \p A.
  const_block_subrange findBlocksIn(Addr A) const {
    return const_block_subrange(
        const_block_subrange::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocksIn<const ByteInterval>(A)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindBlocksIn<const ByteInterval>(A))),
        const_block_subrange::iterator());
  }

  /// \brief Find all the blocks that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref Node objects, which are either \ref DataBlock
  /// objects or \ref CodeBlock objects, that are at the address \p A.
  block_range findBlocksAt(Addr A) {
    return block_range(
        block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocksAt<ByteInterval>(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocksAt<ByteInterval>(A))),
        block_range::iterator());
  }

  /// \brief Find all the blocks that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref Node objects, which are either \ref DataBlock
  /// objects or \ref CodeBlock objects, that are between the addresses.
  block_range findBlocksAt(Addr Low, Addr High) {
    return block_range(
        block_range::iterator(boost::make_transform_iterator(
                                  this->byte_intervals_begin(),
                                  FindBlocksBetween<ByteInterval>(Low, High)),
                              boost::make_transform_iterator(
                                  this->byte_intervals_end(),
                                  FindBlocksBetween<ByteInterval>(Low, High))),
        block_range::iterator());
  }

  /// \brief Find all the blocks that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref Node objects, which are either \ref DataBlock
  /// objects or \ref CodeBlock objects, that are at the address \p A.
  const_block_range findBlocksAt(Addr A) const {
    return const_block_range(
        const_block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocksAt<const ByteInterval>(A)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindBlocksAt<const ByteInterval>(A))),
        const_block_range::iterator());
  }

  /// \brief Find all the blocks that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref Node objects, which are either \ref DataBlock
  /// objects or \ref CodeBlock objects, that are between the addresses.
  const_block_range findBlocksAt(Addr Low, Addr High) const {
    return const_block_range(
        const_block_range::iterator(
            boost::make_transform_iterator(
                this->byte_intervals_begin(),
                FindBlocksBetween<const ByteInterval>(Low, High)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindBlocksBetween<const ByteInterval>(Low, High))),
        const_block_range::iterator());
  }

  /// \brief Iterator over \ref CodeBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using code_block_iterator =
      MergeSortedIterator<ByteInterval::code_block_iterator,
                          AddressLess<CodeBlock>>;
  /// \brief Range of \ref CodeBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using code_block_range = boost::iterator_range<code_block_iterator>;
  /// \brief Sub-range of \ref CodeBlock objects overlapping an address or range
  /// of addreses.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using code_block_subrange = boost::iterator_range<MergeSortedIterator<
      ByteInterval::code_block_subrange::iterator, BlockAddressLess>>;
  /// \brief Iterator over \ref CodeBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_code_block_iterator =
      MergeSortedIterator<ByteInterval::const_code_block_iterator,
                          AddressLess<CodeBlock>>;
  /// \brief Range of \ref CodeBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_code_block_range =
      boost::iterator_range<const_code_block_iterator>;
  /// \brief Sub-range of \ref CodeBlock objects overlapping an address or range
  /// of addreses.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_code_block_subrange = boost::iterator_range<MergeSortedIterator<
      ByteInterval::const_code_block_subrange::iterator, BlockAddressLess>>;

  /// \brief Return an iterator to the first \ref CodeBlock.
  code_block_iterator code_blocks_begin() {
    return code_block_iterator(
        boost::make_transform_iterator(this->byte_intervals_begin(),
                                       NodeToCodeBlockRange<ByteInterval>()),
        boost::make_transform_iterator(this->byte_intervals_end(),
                                       NodeToCodeBlockRange<ByteInterval>()));
  }

  /// \brief Return an iterator to the element following the last \ref
  /// CodeBlock.
  code_block_iterator code_blocks_end() { return code_block_iterator(); }

  /// \brief Return a range of all the \ref CodeBlock objects.
  code_block_range code_blocks() {
    return boost::make_iterator_range(code_blocks_begin(), code_blocks_end());
  }

  /// \brief Return an iterator to the first \ref CodeBlock.
  const_code_block_iterator code_blocks_begin() const {
    return const_code_block_iterator(
        boost::make_transform_iterator(
            this->byte_intervals_begin(),
            NodeToCodeBlockRange<const ByteInterval>()),
        boost::make_transform_iterator(
            this->byte_intervals_end(),
            NodeToCodeBlockRange<const ByteInterval>()));
  }

  /// \brief Return an iterator to the element following the last \ref
  /// CodeBlock.
  const_code_block_iterator code_blocks_end() const {
    return const_code_block_iterator();
  }

  /// \brief Return a range of all the \ref CodeBlock objects.
  const_code_block_range code_blocks() const {
    return boost::make_iterator_range(code_blocks_begin(), code_blocks_end());
  }

  /// \brief Find all the code blocks that have bytes that lie within the
  /// address specified.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref CodeNode object that intersect the address \p A.
  code_block_subrange findCodeBlocksIn(Addr A) {
    return code_block_subrange(
        code_block_subrange::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindCodeBlocksIn<ByteInterval>(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindCodeBlocksIn<ByteInterval>(A))),
        code_block_subrange::iterator());
  }

  /// \brief Find all the code blocks that have bytes that lie within the
  /// address specified.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref CodeBlock objects that intersect the address \p A.
  const_code_block_subrange findCodeBlocksIn(Addr A) const {
    return const_code_block_subrange(
        const_code_block_subrange::iterator(
            boost::make_transform_iterator(
                this->byte_intervals_begin(),
                FindCodeBlocksIn<const ByteInterval>(A)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindCodeBlocksIn<const ByteInterval>(A))),
        const_code_block_subrange::iterator());
  }

  /// \brief Find all the code blocks that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref CodeBlock objects that are at the address \p A.
  code_block_range findCodeBlocksAt(Addr A) {
    return code_block_range(
        code_block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindCodeBlocksAt<ByteInterval>(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindCodeBlocksAt<ByteInterval>(A))),
        code_block_range::iterator());
  }

  /// \brief Find all the code blocks that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref CodeBlock objects that are between the addresses.
  code_block_range findCodeBlocksAt(Addr Low, Addr High) {
    return code_block_range(
        code_block_range::iterator(
            boost::make_transform_iterator(
                this->byte_intervals_begin(),
                FindCodeBlocksBetween<ByteInterval>(Low, High)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindCodeBlocksBetween<ByteInterval>(Low, High))),
        code_block_range::iterator());
  }

  /// \brief Find all the code blocks that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref CodeBlock objects that are at the address \p A.
  const_code_block_range findCodeBlocksAt(Addr A) const {
    return const_code_block_range(
        const_code_block_range::iterator(
            boost::make_transform_iterator(
                this->byte_intervals_begin(),
                FindCodeBlocksAt<const ByteInterval>(A)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindCodeBlocksAt<const ByteInterval>(A))),
        const_code_block_range::iterator());
  }

  /// \brief Find all the code blocks that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref CodeBlock objects that are between the addresses.
  const_code_block_range findCodeBlocksAt(Addr Low, Addr High) const {
    return const_code_block_range(
        const_code_block_range::iterator(
            boost::make_transform_iterator(
                this->byte_intervals_begin(),
                FindCodeBlocksBetween<const ByteInterval>(Low, High)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindCodeBlocksBetween<const ByteInterval>(Low, High))),
        const_code_block_range::iterator());
  }

  /// \brief Iterator over \ref DataBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using data_block_iterator =
      MergeSortedIterator<ByteInterval::data_block_iterator,
                          AddressLess<DataBlock>>;
  /// \brief Range of \ref DataBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using data_block_range = boost::iterator_range<data_block_iterator>;
  /// \brief Sub-range of \ref DataBlock objects overlapping an address or range
  /// of addreses.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using data_block_subrange = boost::iterator_range<MergeSortedIterator<
      ByteInterval::data_block_subrange::iterator, BlockAddressLess>>;
  /// \brief Iterator over \ref DataBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_data_block_iterator =
      MergeSortedIterator<ByteInterval::const_data_block_iterator,
                          AddressLess<DataBlock>>;
  /// \brief Range of \ref DataBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_data_block_range =
      boost::iterator_range<const_data_block_iterator>;
  /// \brief Sub-range of \ref DataBlock objects overlapping an address or range
  /// of addreses.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_data_block_subrange = boost::iterator_range<MergeSortedIterator<
      ByteInterval::const_data_block_subrange::iterator, BlockAddressLess>>;

  /// \brief Return an iterator to the first \ref DataBlock.
  data_block_iterator data_blocks_begin() {
    return data_block_iterator(
        boost::make_transform_iterator(this->byte_intervals_begin(),
                                       NodeToDataBlockRange<ByteInterval>()),
        boost::make_transform_iterator(this->byte_intervals_end(),
                                       NodeToDataBlockRange<ByteInterval>()));
  }

  /// \brief Return an iterator to the element following the last \ref
  /// DataBlock.
  data_block_iterator data_blocks_end() { return data_block_iterator(); }

  /// \brief Return a range of all the \ref DataBlock objects.
  data_block_range data_blocks() {
    return boost::make_iterator_range(data_blocks_begin(), data_blocks_end());
  }

  /// \brief Return an iterator to the first \ref DataBlock.
  const_data_block_iterator data_blocks_begin() const {
    return const_data_block_iterator(
        boost::make_transform_iterator(
            this->byte_intervals_begin(),
            NodeToDataBlockRange<const ByteInterval>()),
        boost::make_transform_iterator(
            this->byte_intervals_end(),
            NodeToDataBlockRange<const ByteInterval>()));
  }

  /// \brief Return an iterator to the element following the last \ref
  /// DataBlock.
  const_data_block_iterator data_blocks_end() const {
    return const_data_block_iterator();
  }

  /// \brief Return a range of all the \ref DataBlock objects.
  const_data_block_range data_blocks() const {
    return boost::make_iterator_range(data_blocks_begin(), data_blocks_end());
  }

  /// \brief Find all the data blocks that have bytes that lie within the
  /// address specified.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref DataNode object that intersect the address \p A.
  data_block_subrange findDataBlocksIn(Addr A) {
    return data_block_subrange(
        data_block_subrange::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindDataBlocksIn<ByteInterval>(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindDataBlocksIn<ByteInterval>(A))),
        data_block_subrange::iterator());
  }

  /// \brief Find all the data blocks that have bytes that lie within the
  /// address specified.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref DataBlock objects that intersect the address \p A.
  const_data_block_subrange findDataBlocksIn(Addr A) const {
    return const_data_block_subrange(
        const_data_block_subrange::iterator(
            boost::make_transform_iterator(
                this->byte_intervals_begin(),
                FindDataBlocksIn<const ByteInterval>(A)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindDataBlocksIn<const ByteInterval>(A))),
        const_data_block_subrange::iterator());
  }

  /// \brief Find all the data blocks that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref DataBlock objects that are at the address \p A.
  data_block_range findDataBlocksAt(Addr A) {
    return data_block_range(
        data_block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindDataBlocksAt<ByteInterval>(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindDataBlocksAt<ByteInterval>(A))),
        data_block_range::iterator());
  }

  /// \brief Find all the data blocks that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref DataBlock objects that are between the addresses.
  data_block_range findDataBlocksAt(Addr Low, Addr High) {
    return data_block_range(
        data_block_range::iterator(
            boost::make_transform_iterator(
                this->byte_intervals_begin(),
                FindDataBlocksBetween<ByteInterval>(Low, High)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindDataBlocksBetween<ByteInterval>(Low, High))),
        data_block_range::iterator());
  }

  /// \brief Find all the data blocks that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref DataBlock objects that are at the address \p A.
  const_data_block_range findDataBlocksAt(Addr A) const {
    return const_data_block_range(
        const_data_block_range::iterator(
            boost::make_transform_iterator(
                this->byte_intervals_begin(),
                FindDataBlocksAt<const ByteInterval>(A)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindDataBlocksAt<const ByteInterval>(A))),
        const_data_block_range::iterator());
  }

  /// \brief Find all the data blocks that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref DataBlock objects that are between the addresses.
  const_data_block_range findDataBlocksAt(Addr Low, Addr High) const {
    return const_data_block_range(
        const_data_block_range::iterator(
            boost::make_transform_iterator(
                this->byte_intervals_begin(),
                FindDataBlocksBetween<const ByteInterval>(Low, High)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindDataBlocksBetween<const ByteInterval>(Low, High))),
        const_data_block_range::iterator());
  }

  /// \brief Iterator over \ref SymbolicExpressionElement objects.
  ///
  /// Results are yielded in address order, ascending.
  using symbolic_expression_iterator =
      MergeSortedIterator<ByteInterval::symbolic_expression_iterator,
                          ByteInterval::SymbolicExpressionElement::AddressLess>;
  /// \brief Range of \ref SymbolicExpressionElement objects.
  ///
  /// Results are yielded in address order, ascending.
  using symbolic_expression_range =
      boost::iterator_range<symbolic_expression_iterator>;
  /// \brief Iterator over \ref SymbolicExpressionElement objects.
  ///
  /// Results are yielded in address order, ascending.
  using const_symbolic_expression_iterator = MergeSortedIterator<
      ByteInterval::const_symbolic_expression_iterator,
      ByteInterval::ConstSymbolicExpressionElement::AddressLess>;
  /// \brief Range of \ref SymbolicExpressionElement objects.
  ///
  /// Results are yielded in address order, ascending.
  using const_symbolic_expression_range =
      boost::iterator_range<const_symbolic_expression_iterator>;

  /// \brief Return an iterator to the first \ref SymbolicExpression.
  symbolic_expression_iterator symbolic_expressions_begin() {
    return symbolic_expression_iterator(
        boost::make_transform_iterator(
            this->byte_intervals_begin(),
            NodeToSymbolicExpressionRange<ByteInterval>()),
        boost::make_transform_iterator(
            this->byte_intervals_end(),
            NodeToSymbolicExpressionRange<ByteInterval>()));
  }

  /// \brief Return an iterator to the element following the last \ref
  /// SymbolicExpression.
  symbolic_expression_iterator symbolic_expressions_end() {
    return symbolic_expression_iterator();
  }

  /// \brief Return a range of all the \ref SymbolicExpression objects.
  symbolic_expression_range symbolic_expressions() {
    return boost::make_iterator_range(symbolic_expressions_begin(),
                                      symbolic_expressions_end());
  }

  /// \brief Return an iterator to the first \ref SymbolicExpression.
  const_symbolic_expression_iterator symbolic_expressions_begin() const {
    return const_symbolic_expression_iterator(
        boost::make_transform_iterator(
            this->byte_intervals_begin(),
            NodeToSymbolicExpressionRange<const ByteInterval>()),
        boost::make_transform_iterator(
            this->byte_intervals_end(),
            NodeToSymbolicExpressionRange<const ByteInterval>()));
  }

  /// \brief Return an iterator to the element following the last \ref
  /// SymbolicExpression.
  const_symbolic_expression_iterator symbolic_expressions_end() const {
    return const_symbolic_expression_iterator();
  }

  /// \brief Return a range of all the \ref SymbolicExpression objects.
  const_symbolic_expression_range symbolic_expressions() const {
    return boost::make_iterator_range(symbolic_expressions_begin(),
                                      symbolic_expressions_end());
  }

  /// \brief Find all the symbolic expressions that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref SymbolicExpression objects that are at the address
  /// \p A.
  symbolic_expression_range findSymbolicExpressionsAt(Addr A) {
    return symbolic_expression_range(
        symbolic_expression_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindSymExprsAt<ByteInterval>(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindSymExprsAt<ByteInterval>(A))),
        symbolic_expression_range::iterator());
  }

  /// \brief Find all the symbolic expressions that start between a range of
  /// addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref SymbolicExpression objects that are between the
  /// addresses.
  symbolic_expression_range findSymbolicExpressionsAt(Addr Low, Addr High) {
    return symbolic_expression_range(
        symbolic_expression_range::iterator(
            boost::make_transform_iterator(
                this->byte_intervals_begin(),
                FindSymExprsBetween<ByteInterval>(Low, High)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindSymExprsBetween<ByteInterval>(Low, High))),
        symbolic_expression_range::iterator());
  }

  /// \brief Find all the symbolic expressions that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref SymbolicExpression objects that are at the address
  /// \p A.
  const_symbolic_expression_range findSymbolicExpressionsAt(Addr A) const {
    return const_symbolic_expression_range(
        const_symbolic_expression_range::iterator(
            boost::make_transform_iterator(
                this->byte_intervals_begin(),
                FindSymExprsAt<const ByteInterval>(A)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindSymExprsAt<const ByteInterval>(A))),
        const_symbolic_expression_range::iterator());
  }

  /// \brief Find all the symbolic expressions that start between a range of
  /// addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref SymbolicExpression objects that are between the
  /// addresses.
  const_symbolic_expression_range findSymbolicExpressionsAt(Addr Low,
                                                            Addr High) const {
    return const_symbolic_expression_range(
        const_symbolic_expression_range::iterator(
            boost::make_transform_iterator(
                this->byte_intervals_begin(),
                FindSymExprsBetween<const ByteInterval>(Low, High)),
            boost::make_transform_iterator(
                this->byte_intervals_end(),
                FindSymExprsBetween<const ByteInterval>(Low, High))),
        const_symbolic_expression_range::iterator());
  }

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing Section.
  using MessageType = proto::Section;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a Section from a protobuf message.
  ///
  /// \param C   The Context in which the deserialized Section will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized Section object, or null on failure.
  static Section* fromProtobuf(Context& C, Module* Parent,
                               const MessageType& Message);

  static bool classof(const Node* N) { return N->getKind() == Kind::Section; }
  /// @endcond

private:
  Module* Parent{nullptr};
  std::string Name;
  ByteIntervalSet ByteIntervals;
  ByteIntervalIntMap ByteIntervalAddrs;

  void setModule(Module* M) { Parent = M; }

  friend class Context; // Allow Context to construct sections.
  friend class Module;  // Allow Module to call setModule.
  friend class Node;    // Allow Node::mutateIndices, etc. to set indices.
};
} // namespace gtirb

#endif // GTIRB_SECTION_H
