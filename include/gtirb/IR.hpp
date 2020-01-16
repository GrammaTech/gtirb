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
#include <gtirb/MergeSortedIterator.hpp>
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
  using range = boost::iterator_range<module_iterator>;
  /// \brief Constant range of \ref Module "Modules".
  ///
  /// Modules are returned in name order. If more than one module has the same
  /// name, the order in which they are returned is unspecified.
  using const_range = boost::iterator_range<const_module_iterator>;

  /// \brief Returns a range of the \ref Module "Modules".
  range modules() {
    return boost::make_iterator_range(modules_begin(), modules_end());
  }
  /// \brief Returns a constant range of the \ref Module "Modules".
  const_range modules() const {
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

  /// \name ProxyBlock-Related Public Types and Functions
  /// @{
  using proxy_block_iterator = MergeSortedIterator<Module::proxy_block_iterator,
                                                   ArbitraryOrder<ProxyBlock>>;
  using proxy_block_range = boost::iterator_range<proxy_block_iterator>;
  using const_proxy_block_iterator =
      MergeSortedIterator<Module::const_proxy_block_iterator,
                          ArbitraryOrder<ProxyBlock>>;
  using const_proxy_block_range =
      boost::iterator_range<const_proxy_block_iterator>;

  proxy_block_iterator proxy_blocks_begin() {
    return proxy_block_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToProxyBlockRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToProxyBlockRange<Module>()));
  }

  proxy_block_iterator proxy_blocks_end() { return proxy_block_iterator(); }

  proxy_block_range proxy_blocks() {
    return boost::make_iterator_range(proxy_blocks_begin(), proxy_blocks_end());
  }

  const_proxy_block_iterator proxy_blocks_begin() const {
    return const_proxy_block_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToProxyBlockRange<const Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToProxyBlockRange<const Module>()));
  }

  const_proxy_block_iterator proxy_blocks_end() const {
    return const_proxy_block_iterator();
  }

  const_proxy_block_range proxy_blocks() const {
    return boost::make_iterator_range(proxy_blocks_begin(), proxy_blocks_end());
  }
  /// @}
  // (end ProxyBlock-Related Public Types and Functions)

  /// \name Symbol-Related Public Types and Functions
  /// @{
  using symbol_iterator =
      MergeSortedIterator<Module::symbol_iterator, ArbitraryOrder<Symbol>>;
  using symbol_range = boost::iterator_range<symbol_iterator>;
  using const_symbol_iterator =
      MergeSortedIterator<Module::const_symbol_iterator,
                          ArbitraryOrder<Symbol>>;
  using const_symbol_range = boost::iterator_range<const_symbol_iterator>;

  symbol_iterator symbols_begin() {
    return symbol_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToSymbolRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToSymbolRange<Module>()));
  }

  symbol_iterator symbols_end() { return symbol_iterator(); }

  symbol_range symbols() {
    return boost::make_iterator_range(symbols_begin(), symbols_end());
  }

  const_symbol_iterator symbols_begin() const {
    return const_symbol_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToSymbolRange<const Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToSymbolRange<const Module>()));
  }

  const_symbol_iterator symbols_end() const { return const_symbol_iterator(); }

  const_symbol_range symbols() const {
    return boost::make_iterator_range(symbols_begin(), symbols_end());
  }
  /// @}
  // (end Symbol-Related Public Types and Functions)

  /// \name Section-Related Public Types and Functions
  /// @{
  using section_iterator =
      MergeSortedIterator<Module::section_iterator, AddressOrder<Section>>;
  using section_range = boost::iterator_range<section_iterator>;
  using section_subrange = boost::iterator_range<MergeSortedIterator<
      Module::section_subrange::iterator, AddressOrder<Section>>>;
  using const_section_iterator =
      MergeSortedIterator<Module::const_section_iterator,
                          AddressOrder<Section>>;
  using const_section_range = boost::iterator_range<const_section_iterator>;
  using const_section_subrange = boost::iterator_range<MergeSortedIterator<
      Module::const_section_subrange::iterator, AddressOrder<Section>>>;

  section_iterator sections_begin() {
    return section_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToSectionRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToSectionRange<Module>()));
  }

  section_iterator sections_end() { return section_iterator(); }

  section_range sections() {
    return boost::make_iterator_range(sections_begin(), sections_end());
  }

  const_section_iterator sections_begin() const {
    return const_section_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToSectionRange<const Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToSectionRange<const Module>()));
  }

  const_section_iterator sections_end() const {
    return const_section_iterator();
  }

  const_section_range sections() const {
    return boost::make_iterator_range(sections_begin(), sections_end());
  }

  section_subrange findSectionsIn(Addr A) {
    struct FindSections {
      Addr A;
      FindSections(Addr A_) : A{A_} {}
      Module::section_subrange operator()(Module& N) const {
        return N.findSectionsIn(A);
      }
    };

    return section_subrange(
        section_subrange::iterator(boost::make_transform_iterator(
                                       this->modules_begin(), FindSections(A)),
                                   boost::make_transform_iterator(
                                       this->modules_end(), FindSections(A))),
        section_subrange::iterator());
  }

  const_section_subrange findSectionsIn(Addr A) const {
    struct FindSections {
      Addr A;
      FindSections(Addr A_) : A{A_} {}
      Module::const_section_subrange operator()(const Module& N) const {
        return N.findSectionsIn(A);
      }
    };

    return const_section_subrange(
        const_section_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindSections(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindSections(A))),
        const_section_subrange::iterator());
  }

  section_range findSectionsAt(Addr A) {
    struct FindSections {
      Addr A;
      FindSections(Addr A_) : A{A_} {}
      Module::section_range operator()(Module& N) const {
        return N.findSectionsAt(A);
      }
    };

    return section_range(
        section_range::iterator(boost::make_transform_iterator(
                                    this->modules_begin(), FindSections(A)),
                                boost::make_transform_iterator(
                                    this->modules_end(), FindSections(A))),
        section_range::iterator());
  }

  section_range findSectionsAt(Addr Low, Addr High) {
    struct FindSections {
      Addr Low, High;
      FindSections(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      Module::section_range operator()(Module& N) const {
        return N.findSectionsAt(Low, High);
      }
    };

    return section_range(
        section_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindSections(Low, High)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindSections(Low, High))),
        section_range::iterator());
  }

  const_section_range findSectionsAt(Addr A) const {
    struct FindSections {
      Addr A;
      FindSections(Addr A_) : A{A_} {}
      Module::const_section_range operator()(const Module& N) const {
        return N.findSectionsAt(A);
      }
    };

    return const_section_range(const_section_range::iterator(
                                   boost::make_transform_iterator(
                                       this->modules_begin(), FindSections(A)),
                                   boost::make_transform_iterator(
                                       this->modules_end(), FindSections(A))),
                               const_section_range::iterator());
  }

  const_section_range findSectionsAt(Addr Low, Addr High) const {
    struct FindSections {
      Addr Low, High;
      FindSections(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      Module::const_section_range operator()(const Module& N) const {
        return N.findSectionsAt(Low, High);
      }
    };

    return const_section_range(
        const_section_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindSections(Low, High)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindSections(Low, High))),
        const_section_range::iterator());
  }
  /// @}
  // (end Section-Related Public Types and Functions)

  /// \name ByteInterval-Related Public Types and Functions
  /// @{
  using byte_interval_iterator =
      MergeSortedIterator<Module::byte_interval_iterator,
                          AddressOrder<ByteInterval>>;
  using byte_interval_range = boost::iterator_range<byte_interval_iterator>;
  using byte_interval_subrange = boost::iterator_range<MergeSortedIterator<
      Module::byte_interval_subrange::iterator, AddressOrder<ByteInterval>>>;
  using const_byte_interval_iterator =
      MergeSortedIterator<Module::const_byte_interval_iterator,
                          AddressOrder<ByteInterval>>;
  using const_byte_interval_range =
      boost::iterator_range<const_byte_interval_iterator>;
  using const_byte_interval_subrange = boost::iterator_range<
      MergeSortedIterator<Module::const_byte_interval_subrange::iterator,
                          AddressOrder<ByteInterval>>>;

  byte_interval_iterator byte_intervals_begin() {
    return byte_interval_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToByteIntervalRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToByteIntervalRange<Module>()));
  }

  byte_interval_iterator byte_intervals_end() {
    return byte_interval_iterator();
  }

  byte_interval_range byte_intervals() {
    return boost::make_iterator_range(byte_intervals_begin(),
                                      byte_intervals_end());
  }

  const_byte_interval_iterator byte_intervals_begin() const {
    return const_byte_interval_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToByteIntervalRange<const Module>()),
        boost::make_transform_iterator(
            this->modules_end(), NodeToByteIntervalRange<const Module>()));
  }

  const_byte_interval_iterator byte_intervals_end() const {
    return const_byte_interval_iterator();
  }

  const_byte_interval_range byte_intervals() const {
    return boost::make_iterator_range(byte_intervals_begin(),
                                      byte_intervals_end());
  }

  byte_interval_subrange findByteIntervalsIn(Addr A) {
    struct FindByteIntervals {
      Addr A;
      FindByteIntervals(Addr A_) : A{A_} {}
      Module::byte_interval_subrange operator()(Module& N) const {
        return N.findByteIntervalsIn(A);
      }
    };

    return byte_interval_subrange(
        byte_interval_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindByteIntervals(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindByteIntervals(A))),
        byte_interval_subrange::iterator());
  }

  const_byte_interval_subrange findByteIntervalsIn(Addr A) const {
    struct FindByteIntervals {
      Addr A;
      FindByteIntervals(Addr A_) : A{A_} {}
      Module::const_byte_interval_subrange operator()(const Module& N) const {
        return N.findByteIntervalsIn(A);
      }
    };

    return const_byte_interval_subrange(
        const_byte_interval_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindByteIntervals(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindByteIntervals(A))),
        const_byte_interval_subrange::iterator());
  }

  byte_interval_range findByteIntervalsAt(Addr A) {
    struct FindByteIntervals {
      Addr A;
      FindByteIntervals(Addr A_) : A{A_} {}
      Module::byte_interval_range operator()(Module& N) const {
        return N.findByteIntervalsAt(A);
      }
    };

    return byte_interval_range(
        byte_interval_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindByteIntervals(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindByteIntervals(A))),
        byte_interval_range::iterator());
  }

  byte_interval_range findByteIntervalsAt(Addr Low, Addr High) {
    struct FindByteIntervals {
      Addr Low, High;
      FindByteIntervals(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      Module::byte_interval_range operator()(Module& N) const {
        return N.findByteIntervalsAt(Low, High);
      }
    };

    return byte_interval_range(
        byte_interval_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindByteIntervals(Low, High)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindByteIntervals(Low, High))),
        byte_interval_range::iterator());
  }

  const_byte_interval_range findByteIntervalsAt(Addr A) const {
    struct FindByteIntervals {
      Addr A;
      FindByteIntervals(Addr A_) : A{A_} {}
      Module::const_byte_interval_range operator()(const Module& N) const {
        return N.findByteIntervalsAt(A);
      }
    };

    return const_byte_interval_range(
        const_byte_interval_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindByteIntervals(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindByteIntervals(A))),
        const_byte_interval_range::iterator());
  }

  const_byte_interval_range findByteIntervalsAt(Addr Low, Addr High) const {
    struct FindByteIntervals {
      Addr Low, High;
      FindByteIntervals(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      Module::const_byte_interval_range operator()(const Module& N) const {
        return N.findByteIntervalsAt(Low, High);
      }
    };

    return const_byte_interval_range(
        const_byte_interval_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindByteIntervals(Low, High)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindByteIntervals(Low, High))),
        const_byte_interval_range::iterator());
  }
  /// @}
  // (end group of ByteInterval-related types and functions)

  /// \name Block-Related Public Types and Functions
  /// @{
  using block_iterator =
      MergeSortedIterator<Module::block_iterator, BlockAddressOrder>;
  using block_range = boost::iterator_range<block_iterator>;
  using block_subrange = boost::iterator_range<
      MergeSortedIterator<Module::block_subrange::iterator, BlockAddressOrder>>;
  using const_block_iterator =
      MergeSortedIterator<Module::const_block_iterator, BlockAddressOrder>;
  using const_block_range = boost::iterator_range<const_block_iterator>;
  using const_block_subrange = boost::iterator_range<MergeSortedIterator<
      Module::const_block_subrange::iterator, BlockAddressOrder>>;

  block_iterator blocks_begin() {
    return block_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToBlockRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToBlockRange<Module>()));
  }

  block_iterator blocks_end() { return block_iterator(); }

  block_range blocks() {
    return boost::make_iterator_range(blocks_begin(), blocks_end());
  }

  const_block_iterator blocks_begin() const {
    return const_block_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToBlockRange<const Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToBlockRange<const Module>()));
  }

  const_block_iterator blocks_end() const { return const_block_iterator(); }

  const_block_range blocks() const {
    return boost::make_iterator_range(blocks_begin(), blocks_end());
  }

  block_subrange findBlocksIn(Addr A) {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      Module::block_subrange operator()(Module& N) const {
        return N.findBlocksIn(A);
      }
    };

    return block_subrange(
        block_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->modules_end(), FindBlocks(A))),
        block_subrange::iterator());
  }

  const_block_subrange findBlocksIn(Addr A) const {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      Module::const_block_subrange operator()(const Module& N) const {
        return N.findBlocksIn(A);
      }
    };

    return const_block_subrange(
        const_block_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->modules_end(), FindBlocks(A))),
        const_block_subrange::iterator());
  }

  block_range findBlocksAt(Addr A) {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      Module::block_range operator()(Module& N) const {
        return N.findBlocksAt(A);
      }
    };

    return block_range(
        block_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->modules_end(), FindBlocks(A))),
        block_range::iterator());
  }

  block_range findBlocksAt(Addr Low, Addr High) {
    struct FindBlocks {
      Addr Low, High;
      FindBlocks(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      Module::block_range operator()(Module& N) const {
        return N.findBlocksAt(Low, High);
      }
    };

    return block_range(
        block_range::iterator(boost::make_transform_iterator(
                                  this->modules_begin(), FindBlocks(Low, High)),
                              boost::make_transform_iterator(
                                  this->modules_end(), FindBlocks(Low, High))),
        block_range::iterator());
  }

  const_block_range findBlocksAt(Addr A) const {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      Module::const_block_range operator()(const Module& N) const {
        return N.findBlocksAt(A);
      }
    };

    return const_block_range(
        const_block_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->modules_end(), FindBlocks(A))),
        const_block_range::iterator());
  }

  const_block_range findBlocksAt(Addr Low, Addr High) const {
    struct FindBlocks {
      Addr Low, High;
      FindBlocks(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      Module::const_block_range operator()(const Module& N) const {
        return N.findBlocksAt(Low, High);
      }
    };

    return const_block_range(
        const_block_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(Low, High)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindBlocks(Low, High))),
        const_block_range::iterator());
  }
  /// @}
  // (end group of Block-related types and functions)

  /// \name CodeBlock-Related Public Types and Functions
  /// @{
  using code_block_iterator =
      MergeSortedIterator<Module::code_block_iterator, AddressOrder<CodeBlock>>;
  using code_block_range = boost::iterator_range<code_block_iterator>;
  using code_block_subrange = boost::iterator_range<MergeSortedIterator<
      Module::code_block_subrange::iterator, AddressOrder<CodeBlock>>>;
  using const_code_block_iterator =
      MergeSortedIterator<Module::const_code_block_iterator,
                          AddressOrder<CodeBlock>>;
  using const_code_block_range =
      boost::iterator_range<const_code_block_iterator>;
  using const_code_block_subrange = boost::iterator_range<MergeSortedIterator<
      Module::const_code_block_subrange::iterator, AddressOrder<CodeBlock>>>;

  code_block_iterator code_blocks_begin() {
    return code_block_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToCodeBlockRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToCodeBlockRange<Module>()));
  }

  code_block_iterator code_blocks_end() { return code_block_iterator(); }

  code_block_range code_blocks() {
    return boost::make_iterator_range(code_blocks_begin(), code_blocks_end());
  }

  const_code_block_iterator code_blocks_begin() const {
    return const_code_block_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToCodeBlockRange<const Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToCodeBlockRange<const Module>()));
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
      Module::code_block_subrange operator()(Module& N) const {
        return N.findCodeBlocksIn(A);
      }
    };

    return code_block_subrange(
        code_block_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->modules_end(), FindBlocks(A))),
        code_block_subrange::iterator());
  }

  const_code_block_subrange findCodeBlocksIn(Addr A) const {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      Module::const_code_block_subrange operator()(const Module& N) const {
        return N.findCodeBlocksIn(A);
      }
    };

    return const_code_block_subrange(
        const_code_block_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->modules_end(), FindBlocks(A))),
        const_code_block_subrange::iterator());
  }

  code_block_range findCodeBlocksAt(Addr A) {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      Module::code_block_range operator()(Module& N) const {
        return N.findCodeBlocksAt(A);
      }
    };

    return code_block_range(
        code_block_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->modules_end(), FindBlocks(A))),
        code_block_range::iterator());
  }

  code_block_range findCodeBlocksAt(Addr Low, Addr High) {
    struct FindBlocks {
      Addr Low, High;
      FindBlocks(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      Module::code_block_range operator()(Module& N) const {
        return N.findCodeBlocksAt(Low, High);
      }
    };

    return code_block_range(
        code_block_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(Low, High)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindBlocks(Low, High))),
        code_block_range::iterator());
  }

  const_code_block_range findCodeBlocksAt(Addr A) const {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      Module::const_code_block_range operator()(const Module& N) const {
        return N.findCodeBlocksAt(A);
      }
    };

    return const_code_block_range(
        const_code_block_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->modules_end(), FindBlocks(A))),
        const_code_block_range::iterator());
  }

  const_code_block_range findCodeBlocksAt(Addr Low, Addr High) const {
    struct FindBlocks {
      Addr Low, High;
      FindBlocks(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      Module::const_code_block_range operator()(const Module& N) const {
        return N.findCodeBlocksAt(Low, High);
      }
    };

    return const_code_block_range(
        const_code_block_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(Low, High)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindBlocks(Low, High))),
        const_code_block_range::iterator());
  }
  /// @}
  // (end group of CodeBlock-related types and functions)

  /// \name DataBlock-Related Public Types and Functions
  /// @{
  using data_block_iterator =
      MergeSortedIterator<Module::data_block_iterator, AddressOrder<DataBlock>>;
  using data_block_range = boost::iterator_range<data_block_iterator>;
  using data_block_subrange = boost::iterator_range<MergeSortedIterator<
      Module::data_block_subrange::iterator, AddressOrder<DataBlock>>>;
  using const_data_block_iterator =
      MergeSortedIterator<Module::const_data_block_iterator,
                          AddressOrder<DataBlock>>;
  using const_data_block_range =
      boost::iterator_range<const_data_block_iterator>;
  using const_data_block_subrange = boost::iterator_range<MergeSortedIterator<
      Module::const_data_block_subrange::iterator, AddressOrder<DataBlock>>>;

  data_block_iterator data_blocks_begin() {
    return data_block_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToDataBlockRange<Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToDataBlockRange<Module>()));
  }

  data_block_iterator data_blocks_end() { return data_block_iterator(); }

  data_block_range data_blocks() {
    return boost::make_iterator_range(data_blocks_begin(), data_blocks_end());
  }

  const_data_block_iterator data_blocks_begin() const {
    return const_data_block_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToDataBlockRange<const Module>()),
        boost::make_transform_iterator(this->modules_end(),
                                       NodeToDataBlockRange<const Module>()));
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
      Module::data_block_subrange operator()(Module& N) const {
        return N.findDataBlocksIn(A);
      }
    };

    return data_block_subrange(
        data_block_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->modules_end(), FindBlocks(A))),
        data_block_subrange::iterator());
  }

  const_data_block_subrange findDataBlocksIn(Addr A) const {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      Module::const_data_block_subrange operator()(const Module& N) const {
        return N.findDataBlocksIn(A);
      }
    };

    return const_data_block_subrange(
        const_data_block_subrange::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->modules_end(), FindBlocks(A))),
        const_data_block_subrange::iterator());
  }

  data_block_range findDataBlocksAt(Addr A) {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      Module::data_block_range operator()(Module& N) const {
        return N.findDataBlocksAt(A);
      }
    };

    return data_block_range(
        data_block_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->modules_end(), FindBlocks(A))),
        data_block_range::iterator());
  }

  data_block_range findDataBlocksAt(Addr Low, Addr High) {
    struct FindBlocks {
      Addr Low, High;
      FindBlocks(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      Module::data_block_range operator()(Module& N) const {
        return N.findDataBlocksAt(Low, High);
      }
    };

    return data_block_range(
        data_block_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(Low, High)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindBlocks(Low, High))),
        data_block_range::iterator());
  }

  const_data_block_range findDataBlocksAt(Addr A) const {
    struct FindBlocks {
      Addr A;
      FindBlocks(Addr A_) : A{A_} {}
      Module::const_data_block_range operator()(const Module& N) const {
        return N.findDataBlocksAt(A);
      }
    };

    return const_data_block_range(
        const_data_block_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(A)),
            boost::make_transform_iterator(this->modules_end(), FindBlocks(A))),
        const_data_block_range::iterator());
  }

  const_data_block_range findDataBlocksAt(Addr Low, Addr High) const {
    struct FindBlocks {
      Addr Low, High;
      FindBlocks(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      Module::const_data_block_range operator()(const Module& N) const {
        return N.findDataBlocksAt(Low, High);
      }
    };

    return const_data_block_range(
        const_data_block_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindBlocks(Low, High)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindBlocks(Low, High))),
        const_data_block_range::iterator());
  }
  /// @}
  // (end group of DataBlock-related types and functions)

  /// \name SymbolicExpression-Related Public Types and Functions
  /// @{
  using symbolic_expression_iterator = MergeSortedIterator<
      Module::symbolic_expression_iterator,
      ByteInterval::SymbolicExpressionElement::AddressOrder>;
  using symbolic_expression_range =
      boost::iterator_range<symbolic_expression_iterator>;
  using const_symbolic_expression_iterator = MergeSortedIterator<
      Module::const_symbolic_expression_iterator,
      ByteInterval::ConstSymbolicExpressionElement::AddressOrder>;
  using const_symbolic_expression_range =
      boost::iterator_range<const_symbolic_expression_iterator>;

  symbolic_expression_iterator symbolic_expressions_begin() {
    return symbolic_expression_iterator(
        boost::make_transform_iterator(this->modules_begin(),
                                       NodeToSymbolicExpressionRange<Module>()),
        boost::make_transform_iterator(
            this->modules_end(), NodeToSymbolicExpressionRange<Module>()));
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
            this->modules_begin(),
            NodeToSymbolicExpressionRange<const Module>()),
        boost::make_transform_iterator(
            this->modules_end(),
            NodeToSymbolicExpressionRange<const Module>()));
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
      Module::symbolic_expression_range operator()(Module& N) const {
        return N.findSymbolicExpressionsAt(A);
      }
    };

    return symbolic_expression_range(
        symbolic_expression_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindSymExprs(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindSymExprs(A))),
        symbolic_expression_range::iterator());
  }

  symbolic_expression_range findSymbolicExpressionsAt(Addr Low, Addr High) {
    struct FindSymExprs {
      Addr Low, High;
      FindSymExprs(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      Module::symbolic_expression_range operator()(Module& N) const {
        return N.findSymbolicExpressionsAt(Low, High);
      }
    };

    return symbolic_expression_range(
        symbolic_expression_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindSymExprs(Low, High)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindSymExprs(Low, High))),
        symbolic_expression_range::iterator());
  }

  const_symbolic_expression_range findSymbolicExpressionsAt(Addr A) const {
    struct FindSymExprs {
      Addr A;
      FindSymExprs(Addr A_) : A{A_} {}
      Module::const_symbolic_expression_range
      operator()(const Module& N) const {
        return N.findSymbolicExpressionsAt(A);
      }
    };

    return const_symbolic_expression_range(
        const_symbolic_expression_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindSymExprs(A)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindSymExprs(A))),
        const_symbolic_expression_range::iterator());
  }

  const_symbolic_expression_range findSymbolicExpressionsAt(Addr Low,
                                                            Addr High) const {
    struct FindSymExprs {
      Addr Low, High;
      FindSymExprs(Addr Low_, Addr High_) : Low{Low_}, High{High_} {}
      Module::const_symbolic_expression_range
      operator()(const Module& N) const {
        return N.findSymbolicExpressionsAt(Low, High);
      }
    };

    return const_symbolic_expression_range(
        const_symbolic_expression_range::iterator(
            boost::make_transform_iterator(this->modules_begin(),
                                           FindSymExprs(Low, High)),
            boost::make_transform_iterator(this->modules_end(),
                                           FindSymExprs(Low, High))),
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
