//===- IR.hpp ---------------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_IR_H
#define GTIRB_IR_H

#include <gtirb/Addr.hpp>
#include <gtirb/AuxData.hpp>
#include <gtirb/AuxDataContainer.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/Utility.hpp>
#include <gtirb/version.h>
#include <boost/iterator/indirect_iterator.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/range/iterator_range.hpp>
#include <map>
#include <string>
#include <vector>

/// \file IR.hpp
/// \brief Class gtirb::IR.

namespace proto {
class IR;
}
namespace gtirb {
/// \class IR
///
/// \brief A complete internal representation consisting of Modules
/// (\ref Module).
///
/// \dot
/// digraph example {
///     node [shape=record, fontname=Helvetica, fontsize=10];
///
///     ir [ label="IR" URL="\ref IR"];
///     module [ label="Module" URL="\ref Module"];
///     auxData [ label="AuxData" URL="\ref AuxData"];
///     imageByteMap [label="ImageByteMap" URL="\ref ImageByteMap"]
///     blocks [label="Block" URL="\ref Block"]
///     data [label="DataBlock"  URL="\ref DataBlock"]
///     symbolicExpressions  [label="SymbolicExpression"
///                           URL="\ref SymbolicExpression"]
///     sections [label="Section" URL="\ref Section"]
///     symbols [label="Symbol" URL="\ref Symbol"]
///     cfg [label="CFG" URL="\ref CFG"]
///
///     ir -> module;
///     ir -> auxData;
///     module -> cfg;
///     module -> data
///     module -> imageByteMap;
///     module -> sections
///     module -> symbols;
///     module -> symbolicExpressions
///     cfg -> blocks;
/// }
/// \enddot
///

// Examples
// The location for these is arbitrary as Doxygen puts all examples in a
// separate section.

/// \example data-symbols.cpp
/// Open an IR and print every symbol pointing to data.

/// \example cfg-paths.cpp
/// Open an IR and print every path from some point to some other point.

/// \example functions.cpp
/// Open an IR with function information in an auxiliary data store and print
/// every function along with the number of other functions it calls.

/// \example jumps.cpp
/// Open an IR and print the Address of every jump instruction,
/// along with the jump targets (if known).

/// \example data-symbols.py
/// Open an IR via protobuf and print every symbol pointing to data.

/// \example cfg-paths.py
/// Open an IR via protobuf and print every path from some point to some
/// other point.

/// \example datasymbols.java
/// Open an IR via protobuf and print every symbol pointing to data.

class GTIRB_EXPORT_API IR : public AuxDataContainer {
  IR(Context& C) : AuxDataContainer(C, Kind::IR) {}

  struct by_name {};
  struct by_pointer {};

  using ModuleSet = boost::multi_index::multi_index_container<
      Module*, boost::multi_index::indexed_by<
                   boost::multi_index::ordered_non_unique<
                       boost::multi_index::tag<by_name>,
                       boost::multi_index::const_mem_fun<
                           Module, const std::string&, &Module::getName>>,
                   boost::multi_index::hashed_unique<
                       boost::multi_index::tag<by_pointer>,
                       boost::multi_index::identity<Module*>>>>;

public:
  /// \brief Create an IR object in its default state.
  ///
  /// \param C  The Context in which this object will be held.
  ///
  /// \return The newly created object.
  static IR* Create(Context& C) { return C.Create<IR>(C); }

  /// \name Module-Related Public Types and Functions
  /// @{

  /// \brief Iterator over \ref Module "Modules".
  ///
  /// Modules are returned in name order. If more than one module has the same
  /// name, the order in which they are returned is unspecified.
  using module_iterator = boost::indirect_iterator<ModuleSet::iterator>;
  /// \brief Constant iterator over \ref Module "Modules".
  ///
  /// Modules are returned in name order. If more than one module has the same
  /// name, the order in which they are returned is unspecified.
  using const_module_iterator =
      boost::indirect_iterator<ModuleSet::const_iterator, const Module>;

  /// \brief Returns an iterator to the first Module.
  module_iterator modules_begin() { return Modules.begin(); }
  /// \brief Returns an iterator to the element following the last Module.
  module_iterator modules_end() { return Modules.end(); }
  /// \brief Returns a constant iterator to the first Module.
  const_module_iterator modules_begin() const { return Modules.begin(); }
  /// \brief Returns a constant iterator to the element following the last
  /// Module.
  const_module_iterator modules_end() const { return Modules.end(); }

  /// \brief Range of \ref Module "Modules".
  ///
  /// Modules are returned in name order. If more than one module has the same
  /// name, the order in which they are returned is unspecified.
  using module_range = boost::iterator_range<module_iterator>;
  /// \brief Constant range of \ref Module "Modules".
  ///
  /// Modules are returned in name order. If more than one module has the same
  /// name, the order in which they are returned is unspecified.
  using const_module_range = boost::iterator_range<const_module_iterator>;

  /// \brief Returns a range of the \ref Module "Modules".
  module_range modules() {
    return boost::make_iterator_range(modules_begin(), modules_end());
  }
  /// \brief Returns a constant range of the \ref Module "Modules".
  const_module_range modules() const {
    return boost::make_iterator_range(modules_begin(), modules_end());
  }

  /// \brief Remove a \ref Module object located in this IR.
  ///
  /// \param S The \ref Module object to remove.
  ///
  /// \return Whether or not the operation succeeded. This operation can
  /// fail if the node to remove is not actually part of this node to begin
  /// with.
  bool removeModule(Module* S) {
    auto& Index = Modules.get<by_pointer>();
    if (auto Iter = Index.find(S); Iter != Index.end()) {
      Index.erase(Iter);
      S->setIR(nullptr);
      return true;
    }
    return false;
  }

  /// \brief Move a \ref Module object to be located in this IR.
  ///
  /// \param S The \ref Module object to add.
  Module* addModule(Module* M) {
    if (M->getIR()) {
      M->getIR()->removeModule(M);
    }
    Modules.emplace(M);
    M->setIR(this);
    return M;
  }

  /// @}
  // (end Module-Related Public Types and Functions)

  /// \brief Serialize to an output stream in binary format.
  ///
  /// \param Out The output stream.
  ///
  /// \return void
  void save(std::ostream& Out) const;

  /// \brief Serialize to an output stream in JSON format.
  ///
  /// \param Out The output stream.
  ///
  /// \return void
  void saveJSON(std::ostream& Out) const;

  /// \brief Deserialize binary format from an input stream.
  ///
  /// \param C   The Context in which this IR will be loaded.
  /// \param In  The input stream.
  ///
  /// \return The deserialized IR object.
  static IR* load(Context& C, std::istream& In);

  /// \brief Deserialize JSON format from an input stream.
  ///
  /// \param C   The Context in which this IR will be loaded.
  /// \param In  The input stream.
  ///
  /// \return The deserialized IR object.
  static IR* loadJSON(Context& C, std::istream& In);

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing IR.
  using MessageType = proto::IR;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a IR from a protobuf message.
  ///
  /// \param C   The Context in which the deserialized IR will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized IR object, or null on failure.
  static IR* fromProtobuf(Context& C, const MessageType& Message);
  /// @endcond

  /// \name ProxyBlock-Related Public Types and Functions
  /// @{

  /// \brief Iterator over \ref ProxyBlock objects.
  using proxy_block_iterator = MergeSortedIterator<Module::proxy_block_iterator,
                                                   ArbitraryLess<ProxyBlock>>;
  /// \brief Range over \ref ProxyBlock objects.
  using proxy_block_range = boost::iterator_range<proxy_block_iterator>;
  /// \brief Iterator over \ref ProxyBlock objects.
  using const_proxy_block_iterator =
      MergeSortedIterator<Module::const_proxy_block_iterator,
                          ArbitraryLess<ProxyBlock>>;
  /// \brief Range over \ref ProxyBlock objects.
  using const_proxy_block_range =
      boost::iterator_range<const_proxy_block_iterator>;

  /// \brief Return an iterator to the first \ref ProxyBlock.
  proxy_block_iterator proxy_blocks_begin() {
    return proxy_block_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToProxyBlockRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToProxyBlockRange<Module>()));
  }

  /// \brief Return an iterator to the element following the last \ref
  /// ProxyBlock.
  proxy_block_iterator proxy_blocks_end() { return proxy_block_iterator(); }

  /// \brief Return a range of all the \ref ProxyBlock objects.
  proxy_block_range proxy_blocks() {
    return boost::make_iterator_range(proxy_blocks_begin(), proxy_blocks_end());
  }

  /// \brief Return an iterator to the first \ref ProxyBlock.
  const_proxy_block_iterator proxy_blocks_begin() const {
    return const_proxy_block_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToProxyBlockRange<const Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToProxyBlockRange<const Module>()));
  }

  /// \brief Return an iterator to the element following the last \ref
  /// ProxyBlock.
  const_proxy_block_iterator proxy_blocks_end() const {
    return const_proxy_block_iterator();
  }

  /// \brief Return an iterator to the first \ref ProxyBlock.
  const_proxy_block_range proxy_blocks() const {
    return boost::make_iterator_range(proxy_blocks_begin(), proxy_blocks_end());
  }
  /// @}
  // (end ProxyBlock-Related Public Types and Functions)

  /// \name Symbol-Related Public Types and Functions
  /// @{

  /// \brief Iterator over \ref Symbol objects.
  ///
  /// This iterator returns symbols in an arbitrary order.
  using symbol_iterator =
      MergeSortedIterator<Module::symbol_iterator, ArbitraryLess<Symbol>>;
  /// \brief Range of \ref Symbol objects.
  ///
  /// This range returns symbols in an arbitrary order.
  using symbol_range = boost::iterator_range<symbol_iterator>;
  /// \brief Iterator over \ref Symbol objects.
  ///
  /// This iterator returns symbols in an arbitrary order.
  using const_symbol_iterator =
      MergeSortedIterator<Module::const_symbol_iterator, ArbitraryLess<Symbol>>;
  /// \brief Range of \ref Symbol objects.
  ///
  /// This range returns symbols in an arbitrary order.
  using const_symbol_range = boost::iterator_range<const_symbol_iterator>;

  /// \brief Return an iterator to the first \ref Symbol.
  symbol_iterator symbols_begin() {
    return symbol_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToSymbolRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToSymbolRange<Module>()));
  }

  /// \brief Return an iterator to the element following the last \ref Symbol.
  symbol_iterator symbols_end() { return symbol_iterator(); }

  /// \brief Return a range of the \ref Symbol objects.
  symbol_range symbols() {
    return boost::make_iterator_range(symbols_begin(), symbols_end());
  }

  /// \brief Return an iterator to the first \ref Symbol.
  const_symbol_iterator symbols_begin() const {
    return const_symbol_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToSymbolRange<const Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToSymbolRange<const Module>()));
  }

  /// \brief Return an iterator to the element following the last \ref Symbol.
  const_symbol_iterator symbols_end() const { return const_symbol_iterator(); }

  /// \brief Return a range of the \ref Symbol objects.
  const_symbol_range symbols() const {
    return boost::make_iterator_range(symbols_begin(), symbols_end());
  }
  /// @}
  // (end Symbol-Related Public Types and Functions)

  /// \name Section-Related Public Types and Functions
  /// @{

  /// \brief Iterator over \ref Section objects.
  using section_iterator =
      MergeSortedIterator<Module::section_iterator, AddressLess<Section>>;
  /// \brief Range of \ref Section objects.
  using section_range = boost::iterator_range<section_iterator>;
  /// \brief Sub-range of \ref Section objects overlapping an address.
  using section_subrange = boost::iterator_range<MergeSortedIterator<
      Module::section_subrange::iterator, AddressLess<Section>>>;
  /// \brief Iterator over \ref Section objects.
  using const_section_iterator =
      MergeSortedIterator<Module::const_section_iterator, AddressLess<Section>>;
  /// \brief Range of \ref Section objects.
  using const_section_range = boost::iterator_range<const_section_iterator>;
  /// \brief Sub-range of \ref Section objects overlapping an address.
  using const_section_subrange = boost::iterator_range<MergeSortedIterator<
      Module::const_section_subrange::iterator, AddressLess<Section>>>;

  /// \brief Return an iterator to the first \ref Section.
  section_iterator sections_begin() {
    return section_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToSectionRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToSectionRange<Module>()));
  }

  /// \brief Return an iterator to the element following the last \ref Section.
  section_iterator sections_end() { return section_iterator(); }

  /// \brief Return a range of all the \ref Section objects.
  section_range sections() {
    return boost::make_iterator_range(sections_begin(), sections_end());
  }

  /// \brief Return an iterator to the first \ref Section.
  const_section_iterator sections_begin() const {
    return const_section_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToSectionRange<const Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToSectionRange<const Module>()));
  }

  /// \brief Return an iterator to the element following the last \ref Section.
  const_section_iterator sections_end() const {
    return const_section_iterator();
  }

  /// \brief Return a range of all the \ref Section objects.
  const_section_range sections() const {
    return boost::make_iterator_range(sections_begin(), sections_end());
  }

  /// \brief Find a Section containing an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return The range of Sections containing the address.
  section_subrange findSectionsIn(Addr A) {
    return section_subrange(
        section_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindSectionsIn<Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindSectionsIn<Module>(A))),
        section_subrange::iterator());
  }

  /// \brief Find a Section containing an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return The range of Sections containing the address.
  const_section_subrange findSectionsIn(Addr A) const {
    return const_section_subrange(
        const_section_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindSectionsIn<const Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindSectionsIn<const Module>(A))),
        const_section_subrange::iterator());
  }

  /// \brief Find all the sections that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref Section objects that are at the address \p A.
  section_range findSectionsAt(Addr A) {
    return section_range(
        section_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindSectionsAt<Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindSectionsAt<Module>(A))),
        section_range::iterator());
  }

  /// \brief Find all the sections that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref Section objects that are between the addresses.
  section_range findSectionsAt(Addr Low, Addr High) {
    return section_range(
        section_range::iterator(
            boost::make_transform_iterator(
                this->modules_begin(), FindSectionsBetween<Module>(Low, High)),
            boost::make_transform_iterator(
                this->modules_end(), FindSectionsBetween<Module>(Low, High))),
        section_range::iterator());
  }

  /// \brief Find all the sections that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref Section objects that are at the address \p A.
  const_section_range findSectionsAt(Addr A) const {
    return const_section_range(
        const_section_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindSectionsAt<const Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindSectionsAt<const Module>(A))),
        const_section_range::iterator());
  }

  /// \brief Find all the sections that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref Section objects that are between the addresses.
  const_section_range findSectionsAt(Addr Low, Addr High) const {
    return const_section_range(
        const_section_range::iterator(
            boost::make_transform_iterator(
                this->modules_begin(),
                FindSectionsBetween<const Module>(Low, High)),
            boost::make_transform_iterator(
                this->modules_end(),
                FindSectionsBetween<const Module>(Low, High))),
        const_section_range::iterator());
  }
  /// @}
  // (end Section-Related Public Types and Functions)

  /// \name ByteInterval-Related Public Types and Functions
  /// @{

  /// \brief Iterator over \ref ByteInterval objects.
  using byte_interval_iterator =
      MergeSortedIterator<Module::byte_interval_iterator,
                          AddressLess<ByteInterval>>;
  /// \brief Range of \ref ByteInterval objects.
  using byte_interval_range = boost::iterator_range<byte_interval_iterator>;
  /// \brief Sub-range of \ref ByteInterval objects overlapping addresses.
  using byte_interval_subrange = boost::iterator_range<MergeSortedIterator<
      Module::byte_interval_subrange::iterator, AddressLess<ByteInterval>>>;
  /// \brief Const iterator over \ref ByteInterval objects.
  using const_byte_interval_iterator =
      MergeSortedIterator<Module::const_byte_interval_iterator,
                          AddressLess<ByteInterval>>;
  /// \brief Const range of \ref ByteInterval objects.
  using const_byte_interval_range =
      boost::iterator_range<const_byte_interval_iterator>;
  /// \brief Sub-range of \ref ByteInterval objects overlapping addresses.
  using const_byte_interval_subrange = boost::iterator_range<
      MergeSortedIterator<Module::const_byte_interval_subrange::iterator,
                          AddressLess<ByteInterval>>>;

  /// \brief Return an iterator to the first \ref ByteInterval.
  byte_interval_iterator byte_intervals_begin() {
    return byte_interval_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToByteIntervalRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToByteIntervalRange<Module>()));
  }

  /// \brief Return an iterator to the element following the last \ref
  /// ByteInterval.
  byte_interval_iterator byte_intervals_end() {
    return byte_interval_iterator();
  }

  /// \brief Return a range of all the \ref ByteInterval objects.
  byte_interval_range byte_intervals() {
    return boost::make_iterator_range(byte_intervals_begin(),
                                      byte_intervals_end());
  }

  /// \brief Return an iterator to the first \ref ByteInterval.
  const_byte_interval_iterator byte_intervals_begin() const {
    return const_byte_interval_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToByteIntervalRange<const Module>()),
        boost::make_transform_iterator(
            this->modules_end(), NodeToByteIntervalRange<const Module>()));
  }

  /// \brief Return an iterator to the element following the last \ref
  /// ByteInterval.
  const_byte_interval_iterator byte_intervals_end() const {
    return const_byte_interval_iterator();
  }

  /// \brief Return a range of all the \ref ByteInterval objects.
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
    return byte_interval_subrange(
        byte_interval_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindByteIntervalsIn<Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindByteIntervalsIn<Module>(A))),
        byte_interval_subrange::iterator());
  }

  /// \brief Find all the intervals that have bytes that lie within the address
  /// specified.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref ByteInterval objects that intersect the address \p
  /// A.
  const_byte_interval_subrange findByteIntervalsIn(Addr A) const {
    return const_byte_interval_subrange(
        const_byte_interval_subrange::iterator(
            boost::make_transform_iterator(
                this->modules_begin(), FindByteIntervalsIn<const Module>(A)),
            boost::make_transform_iterator(
                this->modules_end(), FindByteIntervalsIn<const Module>(A))),
        const_byte_interval_subrange::iterator());
  }

  /// \brief Find all the intervals that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref ByteInterval objects that are at the address \p A.
  byte_interval_range findByteIntervalsAt(Addr A) {
    return byte_interval_range(
        byte_interval_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindByteIntervalsAt<Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindByteIntervalsAt<Module>(A))),
        byte_interval_range::iterator());
  }

  /// \brief Find all the intervals that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref ByteInterval objects that are between the
  /// addresses.
  byte_interval_range findByteIntervalsAt(Addr Low, Addr High) {
    return byte_interval_range(
        byte_interval_range::iterator(
            boost::make_transform_iterator(
                this->modules_begin(),
                FindByteIntervalsBetween<Module>(Low, High)),
            boost::make_transform_iterator(
                this->modules_end(),
                FindByteIntervalsBetween<Module>(Low, High))),
        byte_interval_range::iterator());
  }

  /// \brief Find all the intervals that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref ByteInterval objects that are at the address \p A.
  const_byte_interval_range findByteIntervalsAt(Addr A) const {
    return const_byte_interval_range(
        const_byte_interval_range::iterator(
            boost::make_transform_iterator(
                this->modules_begin(), FindByteIntervalsAt<const Module>(A)),
            boost::make_transform_iterator(
                this->modules_end(), FindByteIntervalsAt<const Module>(A))),
        const_byte_interval_range::iterator());
  }

  /// \brief Find all the intervals that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref ByteInterval objects that are between the
  /// addresses.
  const_byte_interval_range findByteIntervalsAt(Addr Low, Addr High) const {
    return const_byte_interval_range(
        const_byte_interval_range::iterator(
            boost::make_transform_iterator(
                this->modules_begin(),
                FindByteIntervalsBetween<const Module>(Low, High)),
            boost::make_transform_iterator(
                this->modules_end(),
                FindByteIntervalsBetween<const Module>(Low, High))),
        const_byte_interval_range::iterator());
  }
  /// @}
  // (end group of ByteInterval-related types and functions)

  /// \name Block-Related Public Types and Functions
  /// @{

  /// \brief Iterator over blocks.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using block_iterator =
      MergeSortedIterator<Module::block_iterator, BlockAddressLess>;
  /// \brief Range of blocks.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using block_range = boost::iterator_range<block_iterator>;
  /// \brief Sub-range of blocks overlapping an address or range of addreses.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using block_subrange = boost::iterator_range<
      MergeSortedIterator<Module::block_subrange::iterator, BlockAddressLess>>;
  /// \brief Iterator over blocks.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_block_iterator =
      MergeSortedIterator<Module::const_block_iterator, BlockAddressLess>;
  /// \brief Range of blocks.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_block_range = boost::iterator_range<const_block_iterator>;
  /// \brief Sub-range of blocks overlapping an address or range of addreses.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_block_subrange = boost::iterator_range<MergeSortedIterator<
      Module::const_block_subrange::iterator, BlockAddressLess>>;

  /// \brief Return an iterator to the first block.
  block_iterator blocks_begin() {
    return block_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToBlockRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToBlockRange<Module>()));
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
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToBlockRange<const Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToBlockRange<const Module>()));
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
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocksIn<Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindBlocksIn<Module>(A))),
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
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocksIn<const Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindBlocksIn<const Module>(A))),
        const_block_subrange::iterator());
  }

  /// \brief Find all the blocks that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref Node objects, which are either \ref DataBlock
  /// objects or \ref CodeBlock objects, that are at the address \p A.
  block_range findBlocksAt(Addr A) {
    return block_range(block_range::iterator(
                           boost::make_transform_iterator(
                               this->modules_begin(), FindBlocksAt<Module>(A)),
                           boost::make_transform_iterator(
                               this->modules_end(), FindBlocksAt<Module>(A))),
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
        block_range::iterator(
            boost::make_transform_iterator(
                this->modules_begin(), FindBlocksBetween<Module>(Low, High)),
            boost::make_transform_iterator(
                this->modules_end(), FindBlocksBetween<Module>(Low, High))),
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
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocksAt<const Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindBlocksAt<const Module>(A))),
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
                this->modules_begin(),
                FindBlocksBetween<const Module>(Low, High)),
            boost::make_transform_iterator(
                this->modules_end(),
                FindBlocksBetween<const Module>(Low, High))),
        const_block_range::iterator());
  }
  /// @}
  // (end group of Block-related types and functions)

  /// \name CodeBlock-Related Public Types and Functions
  /// @{

  /// \brief Iterator over \ref CodeBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using code_block_iterator =
      MergeSortedIterator<Module::code_block_iterator, AddressLess<CodeBlock>>;
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
      Module::code_block_subrange::iterator, AddressLess<CodeBlock>>>;
  /// \brief Iterator over \ref CodeBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_code_block_iterator =
      MergeSortedIterator<Module::const_code_block_iterator,
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
      Module::const_code_block_subrange::iterator, AddressLess<CodeBlock>>>;

  /// \brief Return an iterator to the first \ref CodeBlock.
  code_block_iterator code_blocks_begin() {
    return code_block_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToCodeBlockRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToCodeBlockRange<Module>()));
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
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToCodeBlockRange<const Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToCodeBlockRange<const Module>()));
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
            boost::make_transform_iterator(this->modules_begin(),
                                           FindCodeBlocksIn<Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindCodeBlocksIn<Module>(A))),
        code_block_subrange::iterator());
  }

  /// \brief Find all the code blocks that have bytes that lie within the
  /// address specified.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref CodeNode object that intersect the address \p A.
  const_code_block_subrange findCodeBlocksIn(Addr A) const {
    return const_code_block_subrange(
        const_code_block_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindCodeBlocksIn<const Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindCodeBlocksIn<const Module>(A))),
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
            boost::make_transform_iterator(this->modules_begin(),
                                           FindCodeBlocksAt<Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindCodeBlocksAt<Module>(A))),
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
                this->modules_begin(),
                FindCodeBlocksBetween<Module>(Low, High)),
            boost::make_transform_iterator(
                this->modules_end(), FindCodeBlocksBetween<Module>(Low, High))),
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
            boost::make_transform_iterator(this->modules_begin(),
                                           FindCodeBlocksAt<const Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindCodeBlocksAt<const Module>(A))),
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
                this->modules_begin(),
                FindCodeBlocksBetween<const Module>(Low, High)),
            boost::make_transform_iterator(
                this->modules_end(),
                FindCodeBlocksBetween<const Module>(Low, High))),
        const_code_block_range::iterator());
  }
  /// @}
  // (end group of CodeBlock-related types and functions)

  /// \name DataBlock-Related Public Types and Functions
  /// @{

  /// \brief Iterator over \ref DataBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using data_block_iterator =
      MergeSortedIterator<Module::data_block_iterator, AddressLess<DataBlock>>;
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
      Module::data_block_subrange::iterator, AddressLess<DataBlock>>>;
  /// \brief Iterator over \ref DataBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_data_block_iterator =
      MergeSortedIterator<Module::const_data_block_iterator,
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
      Module::const_data_block_subrange::iterator, AddressLess<DataBlock>>>;

  /// \brief Return an iterator to the first \ref DataBlock.
  data_block_iterator data_blocks_begin() {
    return data_block_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToDataBlockRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToDataBlockRange<Module>()));
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
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToDataBlockRange<const Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToDataBlockRange<const Module>()));
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
            boost::make_transform_iterator(this->modules_begin(),
                                           FindDataBlocksIn<Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindDataBlocksIn<Module>(A))),
        data_block_subrange::iterator());
  }

  /// \brief Find all the data blocks that have bytes that lie within the
  /// address specified.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref DataNode object that intersect the address \p A.
  const_data_block_subrange findDataBlocksIn(Addr A) const {
    return const_data_block_subrange(
        const_data_block_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindDataBlocksIn<const Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindDataBlocksIn<const Module>(A))),
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
            boost::make_transform_iterator(this->modules_begin(),
                                           FindDataBlocksAt<Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindDataBlocksAt<Module>(A))),
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
                this->modules_begin(),
                FindDataBlocksBetween<Module>(Low, High)),
            boost::make_transform_iterator(
                this->modules_end(), FindDataBlocksBetween<Module>(Low, High))),
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
            boost::make_transform_iterator(this->modules_begin(),
                                           FindDataBlocksAt<const Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindDataBlocksAt<const Module>(A))),
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
                this->modules_begin(),
                FindDataBlocksBetween<const Module>(Low, High)),
            boost::make_transform_iterator(
                this->modules_end(),
                FindDataBlocksBetween<const Module>(Low, High))),
        const_data_block_range::iterator());
  }
  /// @}
  // (end group of DataBlock-related types and functions)

  /// \name SymbolicExpression-Related Public Types and Functions
  /// @{

  /// \brief Iterator over \ref SymbolicExpressionElement objects.
  ///
  /// Results are yielded in address order, ascending.
  using symbolic_expression_iterator =
      MergeSortedIterator<Module::symbolic_expression_iterator,
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
      Module::const_symbolic_expression_iterator,
      ByteInterval::ConstSymbolicExpressionElement::AddressLess>;
  /// \brief Range of \ref SymbolicExpressionElement objects.
  ///
  /// Results are yielded in address order, ascending.
  using const_symbolic_expression_range =
      boost::iterator_range<const_symbolic_expression_iterator>;

  /// \brief Return an iterator to the first \ref SymbolicExpression.
  symbolic_expression_iterator symbolic_expressions_begin() {
    return symbolic_expression_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToSymbolicExpressionRange<Module>()),
        boost::make_transform_iterator(
            this->modules_end(), NodeToSymbolicExpressionRange<Module>()));
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
            this->modules_begin(),
            NodeToSymbolicExpressionRange<const Module>()),
        boost::make_transform_iterator(
            this->modules_end(),
            NodeToSymbolicExpressionRange<const Module>()));
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
            boost::make_transform_iterator(this->modules_begin(),
                                           FindSymExprsAt<Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindSymExprsAt<Module>(A))),
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
                this->modules_begin(), FindSymExprsBetween<Module>(Low, High)),
            boost::make_transform_iterator(
                this->modules_end(), FindSymExprsBetween<Module>(Low, High))),
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
            boost::make_transform_iterator(this->modules_begin(),
                                           FindSymExprsAt<const Module>(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindSymExprsAt<const Module>(A))),
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
                this->modules_begin(),
                FindSymExprsBetween<const Module>(Low, High)),
            boost::make_transform_iterator(
                this->modules_end(),
                FindSymExprsBetween<const Module>(Low, High))),
        const_symbolic_expression_range::iterator());
  }
  /// @}
  // (end group of SymbolicExpression-related types and functions)

  /// \cond INTERNAL
  static bool classof(const Node* N) { return N->getKind() == Kind::IR; }
  /// \endcond

  /// \brief Get the version of the Protobuf used when creating this IR.
  ///
  /// Backwards-incompatible changes to the Protobuf structure of GTIRB cause
  /// this verison number to increment.
  uint32_t getVersion() const { return Version; }

  /// \brief Set the version of the Protobuf used when creating this IR.
  ///
  /// Backwards-incompatible changes to the Protobuf structure of GTIRB cause
  /// this verison number to increment. This function is useful when, for
  /// example, migrating GTIRB from old versions to new versions of the Protobuf
  /// format.
  void setVersion(uint32_t V) { Version = V; }

private:
  ModuleSet Modules;
  uint32_t Version{GTIRB_PROTOBUF_VERSION};

  friend class Context; // Allow Context to construct new IRs.
  friend class Node;    // Allow Node::mutateIndices, etc. to set indices.
};
} // namespace gtirb

#endif // GTIRB_IR_H
