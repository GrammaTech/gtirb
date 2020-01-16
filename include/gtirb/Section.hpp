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
#include <unordered_set>

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
  Section(Context& C, const std::string& N) : Node(C, Kind::Section), Name(N) {}

  struct by_address {};
  struct by_pointer {};

  using ByteIntervalSet = boost::multi_index::multi_index_container<
      ByteInterval*,
      boost::multi_index::indexed_by<
          boost::multi_index::ordered_non_unique<
              boost::multi_index::tag<by_address>,
              boost::multi_index::global_fun<
                  const ByteInterval&, AddressOrder<ByteInterval>::key_type,
                  &AddressOrder<ByteInterval>::key>>,
          boost::multi_index::hashed_unique<
              boost::multi_index::tag<by_pointer>,
              boost::multi_index::identity<ByteInterval*>>>>;
  using ByteIntervalIntMap = boost::icl::interval_map<
      Addr, std::set<ByteInterval*, AddressOrder<ByteInterval>>>;

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

  /// \brief Iterator over \ref ByteInterval objects.
  using byte_interval_iterator =
      boost::indirect_iterator<ByteIntervalSet::iterator>;
  /// \brief Range of \ref ByteInterval objects.
  using byte_interval_range = boost::iterator_range<byte_interval_iterator>;
  using byte_interval_subrange = boost::iterator_range<
      boost::indirect_iterator<ByteIntervalIntMap::codomain_type::iterator>>;
  /// \brief Const iterator over \ref ByteInterval objects.
  using const_byte_interval_iterator =
      boost::indirect_iterator<ByteIntervalSet::const_iterator,
                               const ByteInterval>;
  /// \brief Const range of \ref ByteInterval objects.
  using const_byte_interval_range =
      boost::iterator_range<const_byte_interval_iterator>;
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

  byte_interval_subrange findByteIntervalsIn(Addr A) {
    auto It = ByteIntervalAddrs.find(A);
    if (It == ByteIntervalAddrs.end())
      return {};
    return boost::make_iterator_range(
        boost::make_indirect_iterator(It->second.begin()),
        boost::make_indirect_iterator(It->second.end()));
  }

  const_byte_interval_subrange findByteIntervalsIn(Addr A) const {
    auto It = ByteIntervalAddrs.find(A);
    if (It == ByteIntervalAddrs.end())
      return {};
    return boost::make_iterator_range(
        boost::make_indirect_iterator(It->second.begin()),
        boost::make_indirect_iterator(It->second.end()));
  }

  byte_interval_range findByteIntervalsAt(Addr A) {
    auto Pair = ByteIntervals.get<by_address>().equal_range(A);
    return boost::make_iterator_range(byte_interval_iterator(Pair.first),
                                      byte_interval_iterator(Pair.second));
  }

  byte_interval_range findByteIntervalsAt(Addr Low, Addr High) {
    auto& Index = ByteIntervals.get<by_address>();
    return boost::make_iterator_range(
        byte_interval_iterator(Index.lower_bound(Low)),
        byte_interval_iterator(Index.upper_bound(High)));
  }

  const_byte_interval_range findByteIntervalsAt(Addr A) const {
    auto Pair = ByteIntervals.get<by_address>().equal_range(A);
    return boost::make_iterator_range(
        const_byte_interval_iterator(Pair.first),
        const_byte_interval_iterator(Pair.second));
  }

  const_byte_interval_range findByteIntervalsAt(Addr Low, Addr High) const {
    auto& Index = ByteIntervals.get<by_address>();
    return boost::make_iterator_range(
        const_byte_interval_iterator(Index.lower_bound(Low)),
        const_byte_interval_iterator(Index.upper_bound(High)));
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

  /// \brief Set this section's name.
  void setName(const std::string& N) {
    this->mutateIndices([this, &N]() { Name = N; });
  }

  using block_iterator =
      MergeSortedIterator<ByteInterval::block_iterator, BlockAddressOrder>;
  using block_range = boost::iterator_range<block_iterator>;
  using block_subrange = boost::iterator_range<MergeSortedIterator<
      ByteInterval::block_subrange::iterator, BlockAddressOrder>>;
  using const_block_iterator =
      MergeSortedIterator<ByteInterval::const_block_iterator,
                          BlockAddressOrder>;
  using const_block_range = boost::iterator_range<const_block_iterator>;
  using const_block_subrange = boost::iterator_range<MergeSortedIterator<
      ByteInterval::const_block_subrange::iterator, BlockAddressOrder>>;

  block_iterator blocks_begin() {
    return block_iterator(
        boost::make_transform_iterator(this->byte_intervals_begin(),
                                       NodeToBlockRange<ByteInterval>()),
        boost::make_transform_iterator(this->byte_intervals_end(),
                                       NodeToBlockRange<ByteInterval>()));
  }

  block_iterator blocks_end() { return block_iterator(); }

  block_range blocks() {
    return boost::make_iterator_range(blocks_begin(), blocks_end());
  }

  const_block_iterator blocks_begin() const {
    return const_block_iterator(
        boost::make_transform_iterator(this->byte_intervals_begin(),
                                       NodeToBlockRange<const ByteInterval>()),
        boost::make_transform_iterator(this->byte_intervals_end(),
                                       NodeToBlockRange<const ByteInterval>()));
  }

  const_block_iterator blocks_end() const { return const_block_iterator(); }

  const_block_range blocks() const {
    return boost::make_iterator_range(blocks_begin(), blocks_end());
  }

  block_subrange findBlocksIn(Addr A) {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      ByteInterval::block_subrange operator()(ByteInterval& N) const {
        return N.findBlocksIn(A);
      }
    };

    return block_subrange(block_subrange::iterator(
                              boost::make_transform_iterator(
                                  this->byte_intervals_begin(), FindBlocks(A)),
                              boost::make_transform_iterator(
                                  this->byte_intervals_end(), FindBlocks(A))),
                          block_subrange::iterator());
  }

  const_block_subrange findBlocksIn(Addr A) const {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      ByteInterval::const_block_subrange
      operator()(const ByteInterval& N) const {
        return N.findBlocksIn(A);
      }
    };

    return const_block_subrange(
        const_block_subrange::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(A))),
        const_block_subrange::iterator());
  }

  block_range findBlocksAt(Addr A) {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      ByteInterval::block_range operator()(ByteInterval& N) const {
        return N.findBlocksAt(A);
      }
    };

    return block_range(
        block_range::iterator(boost::make_transform_iterator(
                                  this->byte_intervals_begin(), FindBlocks(A)),
                              boost::make_transform_iterator(
                                  this->byte_intervals_end(), FindBlocks(A))),
        block_range::iterator());
  }

  block_range findBlocksAt(Addr Low, Addr High) {
    struct FindBlocks {
      Addr Low, High;
      FindBlocks(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      ByteInterval::block_range operator()(ByteInterval& N) const {
        return N.findBlocksAt(Low, High);
      }
    };

    return block_range(
        block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(Low, High)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(Low, High))),
        block_range::iterator());
  }

  const_block_range findBlocksAt(Addr A) const {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      ByteInterval::const_block_range operator()(const ByteInterval& N) const {
        return N.findBlocksAt(A);
      }
    };

    return const_block_range(
        const_block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(A))),
        const_block_range::iterator());
  }

  const_block_range findBlocksAt(Addr Low, Addr High) const {
    struct FindBlocks {
      Addr Low, High;
      FindBlocks(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      ByteInterval::const_block_range operator()(const ByteInterval& N) const {
        return N.findBlocksAt(Low, High);
      }
    };

    return const_block_range(
        const_block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(Low, High)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(Low, High))),
        const_block_range::iterator());
  }

  using code_block_iterator =
      MergeSortedIterator<ByteInterval::code_block_iterator,
                          AddressOrder<CodeBlock>>;
  using code_block_range = boost::iterator_range<code_block_iterator>;
  using code_block_subrange = boost::iterator_range<MergeSortedIterator<
      ByteInterval::code_block_subrange::iterator, BlockAddressOrder>>;
  using const_code_block_iterator =
      MergeSortedIterator<ByteInterval::const_code_block_iterator,
                          AddressOrder<CodeBlock>>;
  using const_code_block_range =
      boost::iterator_range<const_code_block_iterator>;
  using const_code_block_subrange = boost::iterator_range<MergeSortedIterator<
      ByteInterval::const_code_block_subrange::iterator, BlockAddressOrder>>;

  code_block_iterator code_blocks_begin() {
    return code_block_iterator(
        boost::make_transform_iterator(this->byte_intervals_begin(),
                                       NodeToCodeBlockRange<ByteInterval>()),
        boost::make_transform_iterator(this->byte_intervals_end(),
                                       NodeToCodeBlockRange<ByteInterval>()));
  }

  code_block_iterator code_blocks_end() { return code_block_iterator(); }

  code_block_range code_blocks() {
    return boost::make_iterator_range(code_blocks_begin(), code_blocks_end());
  }

  const_code_block_iterator code_blocks_begin() const {
    return const_code_block_iterator(
        boost::make_transform_iterator(
            this->byte_intervals_begin(),
            NodeToCodeBlockRange<const ByteInterval>()),
        boost::make_transform_iterator(
            this->byte_intervals_end(),
            NodeToCodeBlockRange<const ByteInterval>()));
  }

  const_code_block_iterator code_blocks_end() const {
    return const_code_block_iterator();
  }

  const_code_block_range code_blocks() const {
    return boost::make_iterator_range(code_blocks_begin(), code_blocks_end());
  }

  code_block_subrange findCodeBlocksIn(Addr A) {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      ByteInterval::code_block_subrange operator()(ByteInterval& N) const {
        return N.findCodeBlocksIn(A);
      }
    };

    return code_block_subrange(
        code_block_subrange::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(A))),
        code_block_subrange::iterator());
  }

  const_code_block_subrange findCodeBlocksIn(Addr A) const {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      ByteInterval::const_code_block_subrange
      operator()(const ByteInterval& N) const {
        return N.findCodeBlocksIn(A);
      }
    };

    return const_code_block_subrange(
        const_code_block_subrange::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(A))),
        const_code_block_subrange::iterator());
  }

  code_block_range findCodeBlocksAt(Addr A) {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      ByteInterval::code_block_range operator()(ByteInterval& N) const {
        return N.findCodeBlocksAt(A);
      }
    };

    return code_block_range(
        code_block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(A))),
        code_block_range::iterator());
  }

  code_block_range findCodeBlocksAt(Addr Low, Addr High) {
    struct FindBlocks {
      Addr Low, High;
      FindBlocks(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      ByteInterval::code_block_range operator()(ByteInterval& N) const {
        return N.findCodeBlocksAt(Low, High);
      }
    };

    return code_block_range(
        code_block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(Low, High)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(Low, High))),
        code_block_range::iterator());
  }

  const_code_block_range findCodeBlocksAt(Addr A) const {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      ByteInterval::const_code_block_range
      operator()(const ByteInterval& N) const {
        return N.findCodeBlocksAt(A);
      }
    };

    return const_code_block_range(
        const_code_block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(A))),
        const_code_block_range::iterator());
  }

  const_code_block_range findCodeBlocksAt(Addr Low, Addr High) const {
    struct FindBlocks {
      Addr Low, High;
      FindBlocks(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      ByteInterval::const_code_block_range
      operator()(const ByteInterval& N) const {
        return N.findCodeBlocksAt(Low, High);
      }
    };

    return const_code_block_range(
        const_code_block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(Low, High)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(Low, High))),
        const_code_block_range::iterator());
  }

  using data_block_iterator =
      MergeSortedIterator<ByteInterval::data_block_iterator,
                          AddressOrder<DataBlock>>;
  using data_block_range = boost::iterator_range<data_block_iterator>;
  using data_block_subrange = boost::iterator_range<MergeSortedIterator<
      ByteInterval::data_block_subrange::iterator, BlockAddressOrder>>;
  using const_data_block_iterator =
      MergeSortedIterator<ByteInterval::const_data_block_iterator,
                          AddressOrder<DataBlock>>;
  using const_data_block_range =
      boost::iterator_range<const_data_block_iterator>;
  using const_data_block_subrange = boost::iterator_range<MergeSortedIterator<
      ByteInterval::const_data_block_subrange::iterator, BlockAddressOrder>>;

  data_block_iterator data_blocks_begin() {
    return data_block_iterator(
        boost::make_transform_iterator(this->byte_intervals_begin(),
                                       NodeToDataBlockRange<ByteInterval>()),
        boost::make_transform_iterator(this->byte_intervals_end(),
                                       NodeToDataBlockRange<ByteInterval>()));
  }

  data_block_iterator data_blocks_end() { return data_block_iterator(); }

  data_block_range data_blocks() {
    return boost::make_iterator_range(data_blocks_begin(), data_blocks_end());
  }

  const_data_block_iterator data_blocks_begin() const {
    return const_data_block_iterator(
        boost::make_transform_iterator(
            this->byte_intervals_begin(),
            NodeToDataBlockRange<const ByteInterval>()),
        boost::make_transform_iterator(
            this->byte_intervals_end(),
            NodeToDataBlockRange<const ByteInterval>()));
  }

  const_data_block_iterator data_blocks_end() const {
    return const_data_block_iterator();
  }

  const_data_block_range data_blocks() const {
    return boost::make_iterator_range(data_blocks_begin(), data_blocks_end());
  }

  data_block_subrange findDataBlocksIn(Addr A) {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      ByteInterval::data_block_subrange operator()(ByteInterval& N) const {
        return N.findDataBlocksIn(A);
      }
    };

    return data_block_subrange(
        data_block_subrange::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(A))),
        data_block_subrange::iterator());
  }

  const_data_block_subrange findDataBlocksIn(Addr A) const {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      ByteInterval::const_data_block_subrange
      operator()(const ByteInterval& N) const {
        return N.findDataBlocksIn(A);
      }
    };

    return const_data_block_subrange(
        const_data_block_subrange::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(A))),
        const_data_block_subrange::iterator());
  }

  data_block_range findDataBlocksAt(Addr A) {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      ByteInterval::data_block_range operator()(ByteInterval& N) const {
        return N.findDataBlocksAt(A);
      }
    };

    return data_block_range(
        data_block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(A))),
        data_block_range::iterator());
  }

  data_block_range findDataBlocksAt(Addr Low, Addr High) {
    struct FindBlocks {
      Addr Low, High;
      FindBlocks(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      ByteInterval::data_block_range operator()(ByteInterval& N) const {
        return N.findDataBlocksAt(Low, High);
      }
    };

    return data_block_range(
        data_block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(Low, High)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(Low, High))),
        data_block_range::iterator());
  }

  const_data_block_range findDataBlocksAt(Addr A) const {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      ByteInterval::const_data_block_range
      operator()(const ByteInterval& N) const {
        return N.findDataBlocksAt(A);
      }
    };

    return const_data_block_range(
        const_data_block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(A))),
        const_data_block_range::iterator());
  }

  const_data_block_range findDataBlocksAt(Addr Low, Addr High) const {
    struct FindBlocks {
      Addr Low, High;
      FindBlocks(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      ByteInterval::const_data_block_range
      operator()(const ByteInterval& N) const {
        return N.findDataBlocksAt(Low, High);
      }
    };

    return const_data_block_range(
        const_data_block_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindBlocks(Low, High)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindBlocks(Low, High))),
        const_data_block_range::iterator());
  }

  using symbolic_expression_iterator = MergeSortedIterator<
      ByteInterval::symbolic_expression_iterator,
      ByteInterval::SymbolicExpressionElement::AddressOrder>;
  using symbolic_expression_range =
      boost::iterator_range<symbolic_expression_iterator>;
  using const_symbolic_expression_iterator = MergeSortedIterator<
      ByteInterval::const_symbolic_expression_iterator,
      ByteInterval::ConstSymbolicExpressionElement::AddressOrder>;
  using const_symbolic_expression_range =
      boost::iterator_range<const_symbolic_expression_iterator>;

  symbolic_expression_iterator symbolic_expressions_begin() {
    return symbolic_expression_iterator(
        boost::make_transform_iterator(
            this->byte_intervals_begin(),
            NodeToSymbolicExpressionRange<ByteInterval>()),
        boost::make_transform_iterator(
            this->byte_intervals_end(),
            NodeToSymbolicExpressionRange<ByteInterval>()));
  }

  symbolic_expression_iterator symbolic_expressions_end() {
    return symbolic_expression_iterator();
  }

  symbolic_expression_range symbolic_expressions() {
    return boost::make_iterator_range(symbolic_expressions_begin(),
                                      symbolic_expressions_end());
  }

  const_symbolic_expression_iterator symbolic_expressions_begin() const {
    return const_symbolic_expression_iterator(
        boost::make_transform_iterator(
            this->byte_intervals_begin(),
            NodeToSymbolicExpressionRange<const ByteInterval>()),
        boost::make_transform_iterator(
            this->byte_intervals_end(),
            NodeToSymbolicExpressionRange<const ByteInterval>()));
  }

  const_symbolic_expression_iterator symbolic_expressions_end() const {
    return const_symbolic_expression_iterator();
  }

  const_symbolic_expression_range symbolic_expressions() const {
    return boost::make_iterator_range(symbolic_expressions_begin(),
                                      symbolic_expressions_end());
  }

  symbolic_expression_range findSymbolicExpressionsAt(Addr A) {
    struct FindSymExprs {
      Addr A;
      FindSymExprs(Addr A_) : A{A_} {}
      ByteInterval::symbolic_expression_range
      operator()(ByteInterval& N) const {
        return N.findSymbolicExpressionsAt(A);
      }
    };

    return symbolic_expression_range(
        symbolic_expression_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindSymExprs(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindSymExprs(A))),
        symbolic_expression_range::iterator());
  }

  symbolic_expression_range findSymbolicExpressionsAt(Addr Low, Addr High) {
    struct FindSymExprs {
      Addr Low, High;
      FindSymExprs(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      ByteInterval::symbolic_expression_range
      operator()(ByteInterval& N) const {
        return N.findSymbolicExpressionsAt(Low, High);
      }
    };

    return symbolic_expression_range(
        symbolic_expression_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindSymExprs(Low, High)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindSymExprs(Low, High))),
        symbolic_expression_range::iterator());
  }

  const_symbolic_expression_range findSymbolicExpressionsAt(Addr A) const {
    struct FindSymExprs {
      Addr A;
      FindSymExprs(Addr A_) : A{A_} {}
      ByteInterval::const_symbolic_expression_range
      operator()(const ByteInterval& N) const {
        return N.findSymbolicExpressionsAt(A);
      }
    };

    return const_symbolic_expression_range(
        const_symbolic_expression_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindSymExprs(A)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindSymExprs(A))),
        const_symbolic_expression_range::iterator());
  }

  const_symbolic_expression_range findSymbolicExpressionsAt(Addr Low,
                                                            Addr High) const {
    struct FindSymExprs {
      Addr Low, High;
      FindSymExprs(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      ByteInterval::const_symbolic_expression_range
      operator()(const ByteInterval& N) const {
        return N.findSymbolicExpressionsAt(Low, High);
      }
    };

    return const_symbolic_expression_range(
        const_symbolic_expression_range::iterator(
            boost::make_transform_iterator(this->byte_intervals_begin(),
                                           FindSymExprs(Low, High)),
            boost::make_transform_iterator(this->byte_intervals_end(),
                                           FindSymExprs(Low, High))),
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
