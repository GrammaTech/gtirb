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
#include <gtirb/CodeBlock.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/Observer.hpp>
#include <gtirb/Utility.hpp>
#include <gtirb/proto/Section.pb.h>
#include <algorithm>
#include <boost/icl/interval_map.hpp>
#include <boost/iterator/indirect_iterator.hpp>
#include <boost/iterator/iterator_traits.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/key_extractors.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/range/iterator_range.hpp>
#include <cstdint>
#include <functional>
#include <set>

/// \file Section.hpp
/// \brief Class gtirb::Section.

namespace gtirb {
class Module; // Forward declared for the backpointer.
class SectionObserver;

/// \enum SectionFlag
///
/// \brief Idenfities the flags used for a section.
enum class SectionFlag : uint8_t {
  Undefined = proto::SectionFlag::Section_Undefined,
  Readable = proto::SectionFlag::Readable,
  Writable = proto::SectionFlag::Writable,
  Executable = proto::SectionFlag::Executable,
  Loaded = proto::SectionFlag::Loaded,
  Initialized = proto::SectionFlag::Initialized,
  ThreadLocal = proto::SectionFlag::ThreadLocal,
};

/// \class Section
///
/// \brief Represents a named section of the binary.
///
/// Does not directly store the contents of the section, which are kept in
/// \ref ImageByteMap.
class GTIRB_EXPORT_API Section : public Node {
  Section(Context& C);
  Section(Context& C, const std::string& N);
  Section(Context& C, const std::string& N, const UUID& U);

  static Section* Create(Context& C, const std::string& Name, const UUID& U) {
    return C.Create<Section>(C, Name, U);
  }

  struct by_address {};
  struct by_pointer {};

  using ByteIntervalSet = boost::multi_index::multi_index_container<
      ByteInterval*,
      boost::multi_index::indexed_by<
          boost::multi_index::ordered_non_unique<
              boost::multi_index::tag<by_address>,
              boost::multi_index::identity<ByteInterval*>, AddressLess>,
          boost::multi_index::hashed_unique<
              boost::multi_index::tag<by_pointer>,
              boost::multi_index::identity<ByteInterval*>>>>;
  using ByteIntervalIntMap =
      boost::icl::interval_map<Addr, std::set<ByteInterval*, AddressLess>>;

  class ByteIntervalObserverImpl;

public:
  /// \brief Create an unitialized Section object.
  /// \param C        The Context in which this Section will be held.
  /// \return         The newly created Section.
  static Section* Create(Context& C) { return C.Create<Section>(C); }

  /// \brief Create a Section object.
  ///
  /// \param C        The Context in which this object will be held.
  /// \param Name     The name of the section.
  ///
  /// \return The newly created object.
  static Section* Create(Context& C, const std::string& Name) {
    return C.Create<Section>(C, Name);
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

  /// \brief Adds the flag to the Section.
  ///
  /// \param F The flag to be added.
  void addFlag(SectionFlag F) { Flags.insert(F); }

  /// \brief Adds all of the flags to the Section.
  /// \tparam Fs A pack of \ref SectionFlag flags.
  /// \param F The flags to be added to the Section.
  template <typename... Fs> void addFlags(Fs... F) { (addFlag(F), ...); }

  /// \brief Removes the flag from the Section.
  ///
  /// \param F The flag to be removed.
  void removeFlag(SectionFlag F) { Flags.erase(F); }

  /// \brief Tests whether the given flag is set for the Section.
  ///
  /// \param F The flag to test.
  /// \return true if the flag is set, false otherwise.
  bool isFlagSet(SectionFlag F) const {
    return std::find(Flags.begin(), Flags.end(), F) != Flags.end();
  }

  /// \brief Iterator over \ref SectionFlag flags.
  using const_section_flag_iterator = std::set<SectionFlag>::const_iterator;
  /// \brief Range of \ref SectionFlag flags.
  using const_section_flag_range =
      boost::iterator_range<const_section_flag_iterator>;

  /// \brief Return a const iterator to the first \ref SectionFlag.
  const_section_flag_iterator flags_begin() const { return Flags.begin(); }
  /// \brief Return a const iterator to the element following the last \ref
  /// SectionFlag.
  const_section_flag_iterator flags_end() const { return Flags.end(); }
  /// \brief Return a range of the \ref SectionFlag flags set for the Section.
  const_section_flag_range flags() const {
    return boost::make_iterator_range(flags_begin(), flags_end());
  }

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
  byte_interval_subrange findByteIntervalsOn(Addr A) {
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
  const_byte_interval_subrange findByteIntervalsOn(Addr A) const {
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
    if (High < Low) {
      return boost::make_iterator_range(Index.end(), Index.end());
    }
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
    if (High < Low) {
      return boost::make_iterator_range(Index.end(), Index.end());
    }
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
    if (Extent) {
      return Extent->lower();
    }
    return std::nullopt;
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
    if (Extent) {
      return Extent->size();
    }
    return std::nullopt;
  }

  /// \brief Remove an interval from this section.
  ///
  /// \return Whether the operation succeeded (\c Accepted), no change was made
  /// (\c NoChange), or the operation could not be completed (\c Rejected). In
  /// particular, if the node to remove is not actually part of this node to
  /// begin with, the result will be \c NoChange.
  ChangeStatus removeByteInterval(ByteInterval* N);

  /// \brief Move an existing \ref ByteInterval to be a part of this section.
  ///
  /// \return a ChangeStatus indicating whether the insertion took place
  /// (\c Accepted), was unnecessary because this node already contained the
  /// ByteInterval (\c NoChange), or could not be completed (\c Rejected).
  ChangeStatus addByteInterval(ByteInterval* BI);

  /// \brief Creates a new \ref ByteInterval in this section.
  ///
  /// \tparam Args  The arguments to construct a \ref ByteInterval.
  /// \param  C     The Context in which this object will be held.
  /// \param  A     The arguments to construct a \ref ByteInterval.
  template <typename... Args>
  ByteInterval* addByteInterval(Context& C, Args&&... A) {
    ByteInterval* BI = ByteInterval::Create(C, std::forward<Args>(A)...);
    [[maybe_unused]] ChangeStatus status = addByteInterval(BI);
    // addByteInterval(ByteInterval*) does not currently reject any insertions
    // and the result cannot be NoChange because we just inserted a newly
    // created ByteInterval.
    assert(status == ChangeStatus::Accepted &&
           "unexpected result when inserting ByteInterval");
    return BI;
  }

  /// \brief Set this section's name.
  void setName(const std::string& N);

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
  block_subrange findBlocksOn(Addr A) {
    byte_interval_subrange Intervals = findByteIntervalsOn(A);
    return block_subrange(
        block_subrange::iterator(
            boost::make_transform_iterator(Intervals.begin(),
                                           FindBlocksIn<ByteInterval>(A)),
            boost::make_transform_iterator(Intervals.end(),
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
  const_block_subrange findBlocksOn(Addr A) const {
    const_byte_interval_subrange Intervals = findByteIntervalsOn(A);
    return const_block_subrange(
        const_block_subrange::iterator(
            boost::make_transform_iterator(Intervals.begin(),
                                           FindBlocksIn<const ByteInterval>(A)),
            boost::make_transform_iterator(
                Intervals.end(), FindBlocksIn<const ByteInterval>(A))),
        const_block_subrange::iterator());
  }

  /// \brief Find all the blocks that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref Node objects, which are either \ref DataBlock
  /// objects or \ref CodeBlock objects, that are at the address \p A.
  block_range findBlocksAt(Addr A) {
    byte_interval_subrange Intervals = findByteIntervalsOn(A);
    return block_range(
        block_range::iterator(
            boost::make_transform_iterator(Intervals.begin(),
                                           FindBlocksAt<ByteInterval>(A)),
            boost::make_transform_iterator(Intervals.end(),
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
    std::vector<ByteInterval::block_range> Ranges;
    for (ByteInterval& BI : findByteIntervalsOn(Low))
      Ranges.push_back(BI.findBlocksAt(Low, High));
    for (ByteInterval& BI : findByteIntervalsAt(Low + 1, High))
      Ranges.push_back(BI.findBlocksAt(Low, High));
    return block_range(block_iterator(Ranges), block_iterator());
  }

  /// \brief Find all the blocks that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref Node objects, which are either \ref DataBlock
  /// objects or \ref CodeBlock objects, that are at the address \p A.
  const_block_range findBlocksAt(Addr A) const {
    const_byte_interval_subrange Intervals = findByteIntervalsOn(A);
    return const_block_range(
        const_block_range::iterator(
            boost::make_transform_iterator(Intervals.begin(),
                                           FindBlocksAt<const ByteInterval>(A)),
            boost::make_transform_iterator(
                Intervals.end(), FindBlocksAt<const ByteInterval>(A))),
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
    std::vector<ByteInterval::const_block_range> Ranges;
    for (const ByteInterval& BI : findByteIntervalsOn(Low))
      Ranges.push_back(BI.findBlocksAt(Low, High));
    for (const ByteInterval& BI : findByteIntervalsAt(Low + 1, High))
      Ranges.push_back(BI.findBlocksAt(Low, High));
    return const_block_range(const_block_iterator(Ranges),
                             const_block_iterator());
  }

  /// \brief Iterator over \ref CodeBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using code_block_iterator =
      MergeSortedIterator<ByteInterval::code_block_iterator, AddressLess>;
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
      ByteInterval::code_block_subrange::iterator, AddressLess>>;
  /// \brief Iterator over \ref CodeBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_code_block_iterator =
      MergeSortedIterator<ByteInterval::const_code_block_iterator, AddressLess>;
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
      ByteInterval::const_code_block_subrange::iterator, AddressLess>>;

private:
  code_block_range makeCodeBlockRange(ByteIntervalSet::iterator Begin,
                                      ByteIntervalSet::iterator End) {
    NodeToCodeBlockRange<ByteInterval> Transformer;
    return boost::make_iterator_range(
        code_block_iterator(boost::make_transform_iterator(
                                byte_interval_iterator(Begin), Transformer),
                            boost::make_transform_iterator(
                                byte_interval_iterator(End), Transformer)),
        code_block_iterator());
  }

public:
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
  code_block_subrange findCodeBlocksOn(Addr A) {
    byte_interval_subrange Intervals = findByteIntervalsOn(A);
    return code_block_subrange(
        code_block_subrange::iterator(
            boost::make_transform_iterator(Intervals.begin(),
                                           FindCodeBlocksIn<ByteInterval>(A)),
            boost::make_transform_iterator(Intervals.end(),
                                           FindCodeBlocksIn<ByteInterval>(A))),
        code_block_subrange::iterator());
  }

  /// \brief Find all the code blocks that have bytes that lie within the
  /// address specified.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref CodeBlock objects that intersect the address \p A.
  const_code_block_subrange findCodeBlocksOn(Addr A) const {
    const_byte_interval_subrange Intervals = findByteIntervalsOn(A);
    return const_code_block_subrange(
        const_code_block_subrange::iterator(
            boost::make_transform_iterator(
                Intervals.begin(), FindCodeBlocksIn<const ByteInterval>(A)),
            boost::make_transform_iterator(
                Intervals.end(), FindCodeBlocksIn<const ByteInterval>(A))),
        const_code_block_subrange::iterator());
  }

  /// \brief Find all the code blocks that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref CodeBlock objects that are at the address \p A.
  code_block_range findCodeBlocksAt(Addr A) {
    byte_interval_subrange Intervals = findByteIntervalsOn(A);
    return code_block_range(
        code_block_range::iterator(
            boost::make_transform_iterator(Intervals.begin(),
                                           FindCodeBlocksAt<ByteInterval>(A)),
            boost::make_transform_iterator(Intervals.end(),
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
    std::vector<ByteInterval::code_block_range> Ranges;
    for (ByteInterval& BI : findByteIntervalsOn(Low))
      Ranges.push_back(BI.findCodeBlocksAt(Low, High));
    for (ByteInterval& BI : findByteIntervalsAt(Low + 1, High))
      Ranges.push_back(BI.findCodeBlocksAt(Low, High));
    return code_block_range(code_block_iterator(Ranges), code_block_iterator());
  }

  /// \brief Find all the code blocks that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref CodeBlock objects that are at the address \p A.
  const_code_block_range findCodeBlocksAt(Addr A) const {
    const_byte_interval_subrange Intervals = findByteIntervalsOn(A);
    return const_code_block_range(
        const_code_block_range::iterator(
            boost::make_transform_iterator(
                Intervals.begin(), FindCodeBlocksAt<const ByteInterval>(A)),
            boost::make_transform_iterator(
                Intervals.end(), FindCodeBlocksAt<const ByteInterval>(A))),
        const_code_block_range::iterator());
  }

  /// \brief Find all the code blocks that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref CodeBlock objects that are between the addresses.
  const_code_block_range findCodeBlocksAt(Addr Low, Addr High) const {
    std::vector<ByteInterval::const_code_block_range> Ranges;
    for (const ByteInterval& BI : findByteIntervalsOn(Low))
      Ranges.push_back(BI.findCodeBlocksAt(Low, High));
    for (const ByteInterval& BI : findByteIntervalsAt(Low + 1, High))
      Ranges.push_back(BI.findCodeBlocksAt(Low, High));
    return const_code_block_range(const_code_block_iterator(Ranges),
                                  const_code_block_iterator());
  }

  /// \brief Iterator over \ref DataBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using data_block_iterator =
      MergeSortedIterator<ByteInterval::data_block_iterator, AddressLess>;
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
      ByteInterval::data_block_subrange::iterator, AddressLess>>;
  /// \brief Iterator over \ref DataBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_data_block_iterator =
      MergeSortedIterator<ByteInterval::const_data_block_iterator, AddressLess>;
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
      ByteInterval::const_data_block_subrange::iterator, AddressLess>>;

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
  data_block_subrange findDataBlocksOn(Addr A) {
    byte_interval_subrange Intervals = findByteIntervalsOn(A);
    return data_block_subrange(
        data_block_subrange::iterator(
            boost::make_transform_iterator(Intervals.begin(),
                                           FindDataBlocksIn<ByteInterval>(A)),
            boost::make_transform_iterator(Intervals.end(),
                                           FindDataBlocksIn<ByteInterval>(A))),
        data_block_subrange::iterator());
  }

  /// \brief Find all the data blocks that have bytes that lie within the
  /// address specified.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref DataBlock objects that intersect the address \p A.
  const_data_block_subrange findDataBlocksOn(Addr A) const {
    const_byte_interval_subrange Intervals = findByteIntervalsOn(A);
    return const_data_block_subrange(
        const_data_block_subrange::iterator(
            boost::make_transform_iterator(
                Intervals.begin(), FindDataBlocksIn<const ByteInterval>(A)),
            boost::make_transform_iterator(
                Intervals.end(), FindDataBlocksIn<const ByteInterval>(A))),
        const_data_block_subrange::iterator());
  }

  /// \brief Find all the data blocks that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref DataBlock objects that are at the address \p A.
  data_block_range findDataBlocksAt(Addr A) {
    byte_interval_subrange Intervals = findByteIntervalsOn(A);
    return data_block_range(
        data_block_range::iterator(
            boost::make_transform_iterator(Intervals.begin(),
                                           FindDataBlocksAt<ByteInterval>(A)),
            boost::make_transform_iterator(Intervals.end(),
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
    std::vector<ByteInterval::data_block_range> Ranges;
    for (ByteInterval& BI : findByteIntervalsOn(Low))
      Ranges.push_back(BI.findDataBlocksAt(Low, High));
    for (ByteInterval& BI : findByteIntervalsAt(Low + 1, High))
      Ranges.push_back(BI.findDataBlocksAt(Low, High));
    return data_block_range(data_block_iterator(Ranges), data_block_iterator());
  }

  /// \brief Find all the data blocks that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref DataBlock objects that are at the address \p A.
  const_data_block_range findDataBlocksAt(Addr A) const {
    const_byte_interval_subrange Intervals = findByteIntervalsOn(A);
    return const_data_block_range(
        const_data_block_range::iterator(
            boost::make_transform_iterator(
                Intervals.begin(), FindDataBlocksAt<const ByteInterval>(A)),
            boost::make_transform_iterator(
                Intervals.end(), FindDataBlocksAt<const ByteInterval>(A))),
        const_data_block_range::iterator());
  }

  /// \brief Find all the data blocks that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref DataBlock objects that are between the addresses.
  const_data_block_range findDataBlocksAt(Addr Low, Addr High) const {
    std::vector<ByteInterval::const_data_block_range> Ranges;
    for (const ByteInterval& BI : findByteIntervalsOn(Low))
      Ranges.push_back(BI.findDataBlocksAt(Low, High));
    for (const ByteInterval& BI : findByteIntervalsAt(Low + 1, High))
      Ranges.push_back(BI.findDataBlocksAt(Low, High));
    return const_data_block_range(const_data_block_iterator(Ranges),
                                  const_data_block_iterator());
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
  static bool classof(const Node* N) { return N->getKind() == Kind::Section; }
  /// @endcond

private:
  Module* Parent{nullptr};
  SectionObserver* Observer{nullptr};
  std::string Name;
  ByteIntervalSet ByteIntervals;
  ByteIntervalIntMap ByteIntervalAddrs;
  std::optional<AddrRange> Extent;
  std::set<SectionFlag> Flags;

  std::unique_ptr<ByteIntervalObserver> BIO;

  /// \brief Remove a ByteInterval from ByteIntervalAddrs.
  void removeByteIntervalAddrs(ByteInterval* BI);

  /// \brief Add a ByteInterval to ByteIntervalAddrs.
  ///
  /// The caller is responsible for ensuring that the ByteInterval is owned
  /// by this Section.
  void insertByteIntervalAddrs(ByteInterval* BI);

  /// \brief Update the extent after adding/removing a ByteInterval.
  ChangeStatus updateExtent();

  void setParent(Module* M, SectionObserver* O) {
    Parent = M;
    Observer = O;
  }

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
  static Section* fromProtobuf(Context& C, const MessageType& Message);

  // Present for testing purposes only.
  void save(std::ostream& Out) const;

  // Present for testing purposes only.
  static Section* load(Context& C, std::istream& In);

  friend class Context; // Allow Context to construct sections.
  friend class Module;  // Allow Module to call setModule, Create, etc.
  // Allows serializaton from Module via sequenceToProtobuf.
  template <typename T> friend typename T::MessageType toProtobuf(const T&);
  friend class SerializationTestHarness; // Testing support.
};

/// \class SectionObserver
///
/// \brief Interface for notifying observers when the Section is modified.
///

class GTIRB_EXPORT_API SectionObserver {
public:
  virtual ~SectionObserver() = default;

  /// \brief Notify the parent when this Section's name changes.
  ///
  /// Called after the Section updates its internal state.
  ///
  /// \param S        the Section whose name changed.
  /// \param OldName  the Section's previous name.
  /// \param NewName  the new name of this section.
  ///
  /// \return indication of whether the observer accepts the change.
  virtual ChangeStatus nameChange(Section* S, const std::string& OldName,
                                  const std::string& NewName) = 0;

  /// \brief Notify the parent when new CodeBlocks are added to the Section.
  ///
  /// Called after the Section updates its internal state.
  ///
  /// \param S       the Section to which CodeBlocks were added.
  /// \param Blocks  a range containing the new CodeBlocks.
  ///
  /// \return indication of whether the observer accepts the change.
  virtual ChangeStatus addCodeBlocks(Section* S,
                                     Section::code_block_range Blocks) = 0;

  /// \brief Notify the parent when the addresses of existing CodeBlocks change.
  ///
  /// Called after the Section updates its internal state.
  ///
  /// \param S       the Section containing the CodeBlocks.
  /// \param Blocks  a range containing the CodeBlocks that moved.
  ///
  /// \return indication of whether the observer accepts the change.
  virtual ChangeStatus moveCodeBlocks(Section* S,
                                      Section::code_block_range Blocks) = 0;

  /// \brief Notify the parent when CodeBlocks are removed from the Section.
  ///
  /// Called before the Section updates its internal state.
  ///
  /// \param S       the Section from which CodeBlocks will be removed.
  /// \param Blocks  a range containing the CodeBlocks to remove.
  ///
  /// \return indication of whether the observer accepts the change.
  virtual ChangeStatus removeCodeBlocks(Section* S,
                                        Section::code_block_range Blocks) = 0;

  /// \brief Notify the parent when new DataBlocks are added to the Section.
  ///
  /// Called after the Section updates its internal state.
  ///
  /// \param S       the Section to which DataBlocks were added.
  /// \param Blocks  a range containing the new DataBlocks.
  ///
  /// \return indication of whether the observer accepts the change.
  virtual ChangeStatus addDataBlocks(Section* S,
                                     Section::data_block_range Blocks) = 0;

  /// \brief Notify the parent when the addresses of existing DataBlocks change.
  ///
  /// Called after the Section updates its internal state.
  ///
  /// \param S       the Section containing the DataBlocks.
  /// \param Blocks  a range containing the DataBlocks that moved.
  ///
  /// \return indication of whether the observer accepts the change.
  virtual ChangeStatus moveDataBlocks(Section* S,
                                      Section::data_block_range Blocks) = 0;

  /// \brief Notify the parent when DataBlocks are removed from the Section.
  ///
  /// Called before the Section updates its internal state.
  ///
  /// \param S       the Section from which DataBlocks will be removed.
  /// \param Blocks  a range containing the DataBlocks to remove.
  ///
  /// \return indication of whether the observer accepts the change.
  virtual ChangeStatus removeDataBlocks(Section* S,
                                        Section::data_block_range Blocks) = 0;

  /// \brief Notify parent when the range of addresses in the Section changes.
  ///
  /// Called before the Section's extent changes. This method should invoke the
  /// callback with \p S to update its extent.
  ///
  /// \param S         the Section that changed.
  /// \param Callback  callable to update the ByteInterval's extent.
  ///
  /// \return indication of whether the observer accepts the change.
  virtual ChangeStatus changeExtent(Section* S,
                                    std::function<void(Section*)> Callback) = 0;
};

inline void Section::setName(const std::string& X) {
  if (Observer) {
    std::string OldName = X;
    std::swap(Name, OldName);
    [[maybe_unused]] ChangeStatus status =
        Observer->nameChange(this, OldName, Name);
    // The known observers do not reject insertions. If that changes, this
    // method must be updated.
    assert(status != ChangeStatus::Rejected &&
           "recovering from rejected name change is unimplemented");
  } else {
    Name = X;
  }
}
} // namespace gtirb

#endif // GTIRB_SECTION_H
