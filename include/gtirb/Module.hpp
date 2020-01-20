//===- Module.hpp -----------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_MODULE_H
#define GTIRB_MODULE_H

#include <gtirb/Addr.hpp>
#include <gtirb/AuxDataContainer.hpp>
#include <gtirb/ByteInterval.hpp>
#include <gtirb/CFG.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/Export.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <gtirb/Utility.hpp>
#include <proto/Module.pb.h>
#include <algorithm>
#include <boost/icl/interval_map.hpp>
#include <boost/iterator/indirect_iterator.hpp>
#include <boost/iterator/iterator_traits.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/key_extractors.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/range/iterator_range.hpp>
#include <cstdint>
#include <functional>
#include <optional>
#include <string>

/// \file Module.hpp
/// \brief Class gtirb::Module and related functions and types.

namespace gtirb {
class IR;

/// \enum FileFormat
///
/// \brief Identifies an exectuable file format.
enum class FileFormat : uint8_t {
  Undefined = proto::Format_Undefined, ///< Default value indicates an
                                       ///< uninitialized state.
  COFF = proto::COFF,                  ///< Common Object File Format (COFF)
  ELF = proto::ELF, ///< Executable and Linkable Format (ELF, formerly named
                    ///< Extensible Linking Format)
  PE = proto::PE,   ///< Microsoft Portable Executable (PE) format.
  IdaProDb32 = proto::IdaProDb32, ///< IDA Pro database file
  IdaProDb64 = proto::IdaProDb64, ///< IDA Pro database file
  XCOFF = proto::XCOFF, ///< Non-COFF (files start with ANON_OBJECT_HEADER*)
  MACHO = proto::MACHO, ///< Mach object file format
  RAW = proto::RAW      ///< Raw binary file (no format)
};

/// \enum ISAID
///
/// \brief Idenfities an instruction set.
enum class ISA : uint8_t {
  Undefined = proto::ISA_Undefined, ///< Default value to indicates an
                                    ///< uninitialized state.
  IA32 = proto::IA32,   ///< Intel Architecture, 32-bit. Also known as i386.
  PPC32 = proto::PPC32, ///< Performance Optimization With Enhanced RISC –
                        ///< Performance Computing, 32-bit.
  X64 = proto::X64,     ///< The generic name for the 64-bit
                        ///< extensions to both Intel's and AMD's 32-bit
                        ///< x86 instruction set architecture (ISA).
  ARM = proto::ARM,     ///< Advanced RISC Machine. also known as Acorn RISC
                        ///< Machine.
  ValidButUnsupported = proto::ValidButUnsupported
};

/// \class Module
///
/// \brief Represents a single binary (library or executable).
class GTIRB_EXPORT_API Module : public AuxDataContainer {
  struct by_address {};
  struct by_name {};
  struct by_pointer {};
  struct by_referent {};

  // Helper function for extracting the referent of a Symbol.
  static const Node* get_symbol_referent(const Symbol& S) {
    if (std::optional<const Node*> Res =
            S.visit([](const Node* N) { return N; })) {
      return *Res;
    }
    return nullptr;
  }

  using ProxyBlockSet = std::unordered_set<ProxyBlock*>;

  using SectionSet = boost::multi_index::multi_index_container<
      Section*, boost::multi_index::indexed_by<
                    boost::multi_index::ordered_non_unique<
                        boost::multi_index::tag<by_address>,
                        boost::multi_index::global_fun<
                            const Section&, AddressLess<Section>::key_type,
                            &AddressLess<Section>::key>>,
                    boost::multi_index::ordered_non_unique<
                        boost::multi_index::tag<by_name>,
                        boost::multi_index::const_mem_fun<
                            Section, const std::string&, &Section::getName>>,
                    boost::multi_index::hashed_unique<
                        boost::multi_index::tag<by_pointer>,
                        boost::multi_index::identity<Section*>>>>;
  using SectionIntMap =
      boost::icl::interval_map<Addr, std::set<Section*, AddressLess<Section>>>;

  using SymbolSet = boost::multi_index::multi_index_container<
      Symbol*,
      boost::multi_index::indexed_by<
          boost::multi_index::ordered_non_unique<
              boost::multi_index::tag<by_address>,
              boost::multi_index::const_mem_fun<Symbol, std::optional<Addr>,
                                                &Symbol::getAddress>>,
          boost::multi_index::ordered_non_unique<
              boost::multi_index::tag<by_name>,
              boost::multi_index::const_mem_fun<Symbol, const std::string&,
                                                &Symbol::getName>>,
          boost::multi_index::hashed_unique<
              boost::multi_index::tag<by_pointer>,
              boost::multi_index::identity<Symbol*>>,
          boost::multi_index::hashed_non_unique<
              boost::multi_index::tag<by_referent>,
              boost::multi_index::global_fun<const Symbol&, const Node*,
                                             &get_symbol_referent>>>>;

  Module(Context& C) : AuxDataContainer(C, Kind::Module) {}
  Module(Context& C, const std::string& N)
      : AuxDataContainer(C, Kind::Module), Name(N) {}

public:
  /// \brief Create a Module object in its default state.
  ///
  /// \param C      The Context in which this object will be held.
  /// \param Parent The \ref IR this module belongs to.
  ///
  /// \return The newly created object.
  static Module* Create(Context& C) { return C.Create<Module>(C); }

  /// \brief Create a Module object.
  ///
  /// \param C      The Context in which this object will be held.
  /// \param Name   The name of this module.
  ///
  /// \return The newly created object.
  static Module* Create(Context& C, const std::string& Name) {
    return C.Create<Module>(C, Name);
  }

  /// \brief Get the \ref IR this module belongs to.
  const IR* getIR() const { return Parent; }
  /// \brief Get the \ref IR this module belongs to.
  IR* getIR() { return Parent; }

  /// \brief Set the location of the corresponding binary on disk.
  ///
  /// This is for informational purposes only and will not be used to open
  /// the image, so it does not need to be the path of an existing file.
  ///
  /// \param X The path name to use.
  void setBinaryPath(const std::string& X) { BinaryPath = X; }

  /// \brief Get the location of the corresponding binary on disk.
  ///
  /// \return   The path to the corresponding binary on disk.
  const std::string& getBinaryPath() const { return BinaryPath; }

  /// \brief Set the format of the binary pointed to by getBinaryPath().
  ///
  /// \param X   The format of the binary associated with \c this, as a
  ///            gtirb::FileFormat enumerator.
  void setFileFormat(gtirb::FileFormat X) { this->FileFormat = X; }

  /// \brief Get the format of the binary pointed to by getBinaryPath().
  ///
  /// \return   The format of the binary associated with \c this, as a
  ///           gtirb::FileFormat enumerator.
  gtirb::FileFormat getFileFormat() const { return this->FileFormat; }

  /// \brief Set the difference between this module's
  /// \ref Module::setPreferredAddr "preferred address" and the address where it
  /// was actually loaded.
  ///
  /// \param X The rebase delta.
  ///
  /// \return void
  void setRebaseDelta(int64_t X) { RebaseDelta = X; }

  /// \brief Get the difference between this module's
  /// \ref Module::setPreferredAddr "preferred address" and
  /// the address where it was actually loaded.
  ///
  /// \return The rebase delta.
  int64_t getRebaseDelta() const { return RebaseDelta; }

  /// \brief Set the preferred address for loading this module.
  ///
  /// \param X The address to use.
  ///
  /// \return void
  /// \sa setRebaseDelta
  void setPreferredAddr(gtirb::Addr X) { PreferredAddr = X; }

  /// \brief Get the preferred address for loading this module.
  ///
  /// \return The preferred address.
  /// \sa getRebaseDelta
  gtirb::Addr getPreferredAddr() const { return PreferredAddr; }

  /// \brief Has the image been loaded somewhere other than its preferred
  /// address?
  ///
  /// \return \c true if the loaded image has been relocated, \c false
  /// otherwise.
  ///
  /// \sa getPreferredAddr
  /// \sa getRebaseDelta
  bool isRelocated() const { return RebaseDelta != 0; }

  /// \brief Set the ISA of the instructions in this Module.
  ///
  /// \param X The ISA ID to set.
  void setISA(gtirb::ISA X) { Isa = X; }

  /// \brief Get the ISA of the instructions in this Module.
  ///
  /// \return The ISA ID.
  gtirb::ISA getISA() const { return Isa; }

  /// \brief Get the entry point of this module, or null if not present.
  const CodeBlock* getEntryPoint() const { return EntryPoint; }
  /// \brief Get the entry point of this module, or null if not present.
  CodeBlock* getEntryPoint() { return EntryPoint; }

  /// \brief Set the entry point of this module.
  ///
  /// \param CB The entry point of this module, or null if not present.
  void setEntryPoint(CodeBlock* CB) { EntryPoint = CB; }

  /// \name ProxyBlock-Related Public Types and Functions
  /// @{

  /// \brief Iterator over proxy_blocks (\ref ProxyBlock).
  using proxy_block_iterator =
      boost::indirect_iterator<ProxyBlockSet::iterator>;
  /// \brief Range of proxy_blocks (\ref ProxyBlock).
  using proxy_block_range = boost::iterator_range<proxy_block_iterator>;
  /// \brief Constant iterator over proxy_blocks (\ref ProxyBlock).
  using const_proxy_block_iterator =
      boost::indirect_iterator<ProxyBlockSet::const_iterator, const ProxyBlock>;
  /// \brief Constant range of proxy_blocks (\ref ProxyBlock).
  using const_proxy_block_range =
      boost::iterator_range<const_proxy_block_iterator>;

  /// \brief Return an iterator to the first ProxyBlock.
  proxy_block_iterator proxy_blocks_begin() {
    return proxy_block_iterator(ProxyBlocks.begin());
  }
  /// \brief Return a constant iterator to the first ProxyBlock.
  const_proxy_block_iterator proxy_blocks_begin() const {
    return const_proxy_block_iterator(ProxyBlocks.begin());
  }
  /// \brief Return an iterator to the element following the last ProxyBlock.
  proxy_block_iterator proxy_blocks_end() {
    return proxy_block_iterator(ProxyBlocks.end());
  }
  /// \brief Return a constant iterator to the element following the last
  /// ProxyBlock.
  const_proxy_block_iterator proxy_blocks_end() const {
    return const_proxy_block_iterator(ProxyBlocks.end());
  }
  /// \brief Return a range of the proxy_blocks (\ref ProxyBlock).
  proxy_block_range proxy_blocks() {
    return boost::make_iterator_range(proxy_blocks_begin(), proxy_blocks_end());
  }
  /// \brief Return a constant range of the proxy_blocks (\ref ProxyBlock).
  const_proxy_block_range proxy_blocks() const {
    return boost::make_iterator_range(proxy_blocks_begin(), proxy_blocks_end());
  }

  /// \brief Remove a \ref ProxyBlock object located in this module.
  ///
  /// \param S The \ref ProxyBlock object to remove.
  ///
  /// \return Whether or not the operation succeeded. This operation can
  /// fail if the node to remove is not actually part of this node to begin
  /// with.
  bool removeProxyBlock(ProxyBlock* B) {
    auto N = ProxyBlocks.erase(B);
    B->setModule(nullptr);
    // TODO: removeVertex(B, Cfg);
    return N != 0;
  }

  /// \brief Move a \ref ProxyBlock object to be located in this module.
  ///
  /// \param S The \ref ProxyBlock object to add.
  ProxyBlock* addProxyBlock(ProxyBlock* B) {
    if (B->getModule()) {
      B->getModule()->removeProxyBlock(B);
    }
    ProxyBlocks.insert(B);
    B->setModule(this);
    addVertex(B, Cfg);
    return B;
  }

  /// @}
  // (end of ProxyBlock-Related Public Types and Functions)

  /// \name Symbol-Related Public Types and Functions
  /// @{

  /// \brief Iterator over symbols (\ref Symbol).
  ///
  /// This iterator returns symbols in an arbitrary order.
  using symbol_iterator =
      boost::indirect_iterator<SymbolSet::index<by_pointer>::type::iterator>;
  /// \brief Range of symbols (\ref Symbol).
  ///
  /// This range returns symbols in an arbitrary order.
  using symbol_range = boost::iterator_range<symbol_iterator>;
  /// \brief Constant iterator over symbols (\ref Symbol).
  ///
  /// This iterator returns symbols in an arbitrary order.
  using const_symbol_iterator = boost::indirect_iterator<
      SymbolSet::index<by_pointer>::type::const_iterator, const Symbol>;
  /// \brief Constant range of symbols (\ref Symbol).
  ///
  /// This range returns symbols in an arbitrary order.
  using const_symbol_range = boost::iterator_range<const_symbol_iterator>;

  /// \brief Iterator over symbols (\ref Symbol).
  ///
  /// This iterator returns symbols in name order. If two Symbols have the same
  /// name, their order is unspecified.
  using symbol_name_iterator =
      boost::indirect_iterator<SymbolSet::index<by_name>::type::iterator>;
  /// \brief Range of symbols (\ref Symbol).
  ///
  /// This range returns symbols in name order. If two Symbols have the same
  /// name, their order is unspecified.
  using symbol_name_range = boost::iterator_range<symbol_name_iterator>;
  /// \brief Constant iterator over symbols (\ref Symbol).
  ///
  /// This iterator returns symbols in name order. If two Symbols have the same
  /// name, their order is unspecified.
  using const_symbol_name_iterator =
      boost::indirect_iterator<SymbolSet::index<by_name>::type::const_iterator,
                               const Symbol>;
  /// \brief Constant range of symbols (\ref Symbol).
  ///
  /// This range returns symbols in name order. If two Symbols have the same
  /// name, their order is unspecified.
  using const_symbol_name_range =
      boost::iterator_range<const_symbol_name_iterator>;

  /// \brief Iterator over symbols (\ref Symbol).
  ///
  /// This iterator returns symbols in address order. If two Symbols have the
  /// same address, their order is unspecified.
  using symbol_addr_iterator =
      boost::indirect_iterator<SymbolSet::index<by_address>::type::iterator>;
  /// \brief Range of symbols (\ref Symbol).
  ///
  /// This range returns symbols in address order. If two Symbols have the same
  /// address, their order is unspecified.
  using symbol_addr_range = boost::iterator_range<symbol_addr_iterator>;
  /// \brief Constant iterator over symbols (\ref Symbol).
  ///
  /// This iterator returns symbols in address order. If two Symbols have the
  /// same address, their order is unspecified.
  using const_symbol_addr_iterator = boost::indirect_iterator<
      SymbolSet::index<by_address>::type::const_iterator, const Symbol>;
  /// \brief Constant range of symbols (\ref Symbol).
  ///
  /// This range returns symbols in address order. If two Symbols have the same
  /// address, their order is unspecified.
  using const_symbol_addr_range =
      boost::iterator_range<const_symbol_addr_iterator>;

  /// \brief Iterator over symbols (\ref Symbol).
  ///
  /// The order in which this iterator returns symbols is not specified.
  using symbol_ref_iterator =
      boost::indirect_iterator<SymbolSet::index<by_referent>::type::iterator>;
  /// \brief Range of symbols (\ref Symbol).
  ///
  /// The order of the symbols in this range is not specified.
  using symbol_ref_range = boost::iterator_range<symbol_ref_iterator>;
  /// \brief Constant iterator over symbols (\ref Symbol).
  ///
  /// The order in which this iterator returns symbols is not specified.
  using const_symbol_ref_iterator = boost::indirect_iterator<
      SymbolSet::index<by_referent>::type::const_iterator, const Symbol>;
  /// \brief Constant range of symbols (\ref Symbol).
  ///
  /// The order of the symbols in this range is not specified.
  using const_symbol_ref_range =
      boost::iterator_range<const_symbol_ref_iterator>;

  /// \brief Return an iterator to the first Symbol.
  symbol_iterator symbols_begin() {
    return symbol_iterator(Symbols.get<by_pointer>().begin());
  }
  /// \brief Return a constant iterator to the first Symbol.
  const_symbol_iterator symbols_begin() const {
    return const_symbol_iterator(Symbols.get<by_pointer>().begin());
  }
  /// \brief Return an iterator to the element following the last Symbol.
  symbol_iterator symbols_end() {
    return symbol_iterator(Symbols.get<by_pointer>().end());
  }
  /// \brief Return a constant iterator to the element following the last
  /// Symbol.
  const_symbol_iterator symbols_end() const {
    return const_symbol_iterator(Symbols.get<by_pointer>().end());
  }
  /// \brief Return a range of the symbols (\ref Symbol).
  symbol_range symbols() {
    return boost::make_iterator_range(symbols_begin(), symbols_end());
  }
  /// \brief Return a constant range of the symbols (\ref Symbol).
  const_symbol_range symbols() const {
    return boost::make_iterator_range(symbols_begin(), symbols_end());
  }

  /// \brief Return an iterator to the first Symbol, ordered by name.
  symbol_name_iterator symbols_by_name_begin() {
    return symbol_name_iterator(Symbols.get<by_name>().begin());
  }
  /// \brief Return a constant iterator to the first Symbol, ordered by name.
  const_symbol_name_iterator symbols_by_name_begin() const {
    return const_symbol_name_iterator(Symbols.get<by_name>().begin());
  }
  /// \brief Return an iterator to the element following the last Symbol,
  /// ordered by name.
  symbol_name_iterator symbols_by_name_end() {
    return symbol_name_iterator(Symbols.get<by_name>().end());
  }
  /// \brief Return a constant iterator to the element following the last
  /// Symbol, ordered by name.
  const_symbol_name_iterator symbols_by_name_end() const {
    return const_symbol_name_iterator(Symbols.get<by_name>().end());
  }
  /// \brief Return a range of the symbols (\ref Symbol), ordered by name.
  symbol_name_range symbols_by_name() {
    return boost::make_iterator_range(symbols_by_name_begin(),
                                      symbols_by_name_end());
  }
  /// \brief Return a constant range of the symbols (\ref Symbol), ordered by
  /// name.
  const_symbol_name_range symbols_by_name() const {
    return boost::make_iterator_range(symbols_by_name_begin(),
                                      symbols_by_name_end());
  }

  /// \brief Return an iterator to the first Symbol, ordered by address.
  symbol_addr_iterator symbols_by_addr_begin() {
    return symbol_addr_iterator(Symbols.get<by_address>().begin());
  }
  /// \brief Return a constant iterator to the first Symbol, ordered by address.
  const_symbol_addr_iterator symbols_by_addr_begin() const {
    return const_symbol_addr_iterator(Symbols.get<by_address>().begin());
  }
  /// \brief Return an iterator to the element following the last Symbol,
  /// ordered by address.
  symbol_addr_iterator symbols_by_addr_end() {
    return symbol_addr_iterator(Symbols.get<by_address>().end());
  }
  /// \brief Return a constant iterator to the element following the last
  /// Symbol, ordered by address.
  const_symbol_addr_iterator symbols_by_addr_end() const {
    return const_symbol_addr_iterator(Symbols.get<by_address>().end());
  }
  /// \brief Return a range of the symbols (\ref Symbol), ordered by address.
  symbol_addr_range symbols_by_addr() {
    return boost::make_iterator_range(symbols_by_addr_begin(),
                                      symbols_by_addr_end());
  }
  /// \brief Return a constant range of the symbols (\ref Symbol), ordered by
  /// address.
  const_symbol_addr_range symbols_by_addr() const {
    return boost::make_iterator_range(symbols_by_addr_begin(),
                                      symbols_by_addr_end());
  }

  /// \brief Remove a \ref Symbol object located in this module.
  ///
  /// \param S The \ref Symbol object to remove.
  ///
  /// \return Whether or not the operation succeeded. This operation can
  /// fail if the node to remove is not actually part of this node to begin
  /// with.
  bool removeSymbol(Symbol* S) {
    auto& Index = Symbols.get<by_pointer>();
    if (auto Iter = Index.find(S); Iter != Index.end()) {
      Index.erase(Iter);
      S->setModule(nullptr);
      return true;
    }
    return false;
  }

  /// \brief Move a \ref Symbol object to be located in this module.
  ///
  /// \param S The \ref Symbol object to add.
  Symbol* addSymbol(Symbol* S) {
    if (S->getModule()) {
      S->getModule()->removeSymbol(S);
    }
    Symbols.emplace(S);
    S->setModule(this);
    return S;
  }

  /// \brief Find symbols by name
  ///
  /// \param N The name to look up.
  ///
  /// \return A possibly empty range of all the symbols with the
  /// given name.
  symbol_name_range findSymbols(const std::string& N) {
    auto Found = Symbols.get<by_name>().equal_range(N);
    return boost::make_iterator_range(Found.first, Found.second);
  }

  /// \brief Find symbols by name
  ///
  /// \param N The name to look up.
  ///
  /// \return A possibly empty constant range of all the symbols with the
  /// given name.
  const_symbol_name_range findSymbols(const std::string& N) const {
    auto Found = Symbols.get<by_name>().equal_range(N);
    return boost::make_iterator_range(Found.first, Found.second);
  }

  /// \brief Find symbols by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return A possibly empty range of all the symbols with a referent at the
  /// given address.
  symbol_addr_range findSymbols(Addr X) {
    auto Found = Symbols.get<by_address>().equal_range(X);
    return boost::make_iterator_range(Found.first, Found.second);
  }

  /// \brief Find symbols by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return A possibly empty constant range of all the symbols with a referent
  /// at the given address.
  const_symbol_addr_range findSymbols(Addr X) const {
    auto Found = Symbols.get<by_address>().equal_range(X);
    return boost::make_iterator_range(Found.first, Found.second);
  }

  /// \brief Find symbols by a range of addresses.
  ///
  /// \param Lower The lower-bounded address to look up.
  /// \param Upper The upper-bounded address to look up.
  ///
  /// \return A possibly empty range of all the symbols within the given
  /// address range. Searches the range [Lower, Upper).
  symbol_addr_range findSymbols(Addr Lower, Addr Upper) {
    auto& Index = Symbols.get<by_address>();
    return boost::make_iterator_range(Index.lower_bound(Lower),
                                      Index.lower_bound(Upper));
  }

  /// \brief Find symbols by a range of addresses.
  ///
  /// \param Lower The lower-bounded address to look up.
  /// \param Upper The upper-bounded address to look up.
  ///
  /// \return A possibly empty constant range of all the symbols within the
  /// given address range. Searches the range [Lower, Upper).
  const_symbol_addr_range findSymbols(Addr Lower, Addr Upper) const {
    auto& Index = Symbols.get<by_address>();
    return boost::make_iterator_range(Index.lower_bound(Lower),
                                      Index.lower_bound(Upper));
  }

  /// \brief Find symbols by their referent object.
  ///
  /// \param Referent The object the symbol refers to.
  ///
  /// \return A possibly empty range of all the symbols that refer to the given
  /// object.
  symbol_ref_range findSymbols(const Node& Referent) {
    return Symbols.get<by_referent>().equal_range(&Referent);
  }

  /// \brief Find symbols by their referent object.
  ///
  /// \param Referent The object the symbol refers to.
  ///
  /// \return A possibly empty range of all the symbols that refer to the given
  /// object.
  const_symbol_ref_range findSymbols(const Node& Referent) const {
    return Symbols.get<by_referent>().equal_range(&Referent);
  }

  /// @}
  // (end group of symbol-related type aliases and functions)

  /// \brief Get the module name.
  ///
  /// \return The name.
  const std::string& getName() const { return Name; }

  /// \brief Set the module name.
  void setName(const std::string& X) {
    this->mutateIndices([this, &X]() { Name = X; });
  }

  /// \brief Get the associated Control Flow Graph (\ref CFG).
  ///
  /// \return The associated CFG.
  const CFG& getCFG() const { return Cfg; }

  /// \brief Get a const reference to the associated Control Flow Graph
  /// (\ref CFG).
  ///
  /// \return The associated CFG.
  CFG& getCFG() { return Cfg; }

  /// \name Section-Related Public Types and Functions
  /// @{

  /// \brief Iterator over sections (\ref Section).
  using section_iterator = boost::indirect_iterator<SectionSet::iterator>;
  /// \brief Range of sections (\ref Section).
  using section_range = boost::iterator_range<section_iterator>;
  /// \brief Sub-range of sections overlapping an address (\ref Section).
  using section_subrange = boost::iterator_range<
      boost::indirect_iterator<SectionIntMap::codomain_type::iterator>>;
  /// \brief Iterator over sections (\ref Section).
  ///
  /// Sections are returned in name order. If two Sections have the same name,
  /// their order is not specified.
  using section_name_iterator =
      boost::indirect_iterator<SectionSet::index<by_name>::type::iterator>;
  /// \brief Range of sections (\ref Section).
  ///
  /// Sections are returned in name order. If two Sections have the same name,
  /// their order is not specified.
  using section_name_range = boost::iterator_range<section_name_iterator>;
  /// \brief Constant iterator over sections (\ref Section).
  ///
  /// Sections are returned in address order. If two Sections start at the
  /// same address, the smaller one is returned first. If two Sections have
  /// the same address and the same size, their order is not specified.
  using const_section_iterator =
      boost::indirect_iterator<SectionSet::const_iterator, const Section&>;
  /// \brief Constant range of sections (\ref Section).
  ///
  /// Sections are returned in address order. If two Sections start at the
  /// same address, the smaller one is returned first. If two Sections have
  /// the same address and the same size, their order is not specified.
  using const_section_range = boost::iterator_range<const_section_iterator>;
  /// \brief Sub-range of sections overlapping an address (\ref Section).
  using const_section_subrange = boost::iterator_range<boost::indirect_iterator<
      SectionIntMap::codomain_type::const_iterator, const Section&>>;
  /// \brief Constant iterator over sections (\ref Section).
  ///
  /// Sections are returned in name order. If two Sections have the same name,
  /// their order is not specified.
  using const_section_name_iterator =
      boost::indirect_iterator<SectionSet::index<by_name>::type::const_iterator,
                               const Section&>;
  /// \brief Constant range of sections (\ref Section).
  ///
  /// Sections are returned in name order. If two Sections have the same name,
  /// their order is not specified.
  using const_section_name_range =
      boost::iterator_range<const_section_name_iterator>;

  /// \brief Return an iterator to the first Section.
  section_iterator sections_begin() { return Sections.begin(); }
  /// \brief Return a constant iterator to the first Section.
  const_section_iterator sections_begin() const { return Sections.begin(); }
  /// \brief Return an iterator to the first Section.
  section_name_iterator sections_by_name_begin() {
    return Sections.get<by_name>().begin();
  }
  /// \brief Return a constant iterator to the first Section.
  const_section_name_iterator sections_by_name_begin() const {
    return Sections.get<by_name>().begin();
  }
  /// \brief Return an iterator to the element following the last Section.
  section_iterator sections_end() { return Sections.end(); }
  /// \brief Return a constant iterator to the element following the last
  /// Section.
  const_section_iterator sections_end() const { return Sections.end(); }
  /// \brief Return an iterator to the element following the last Section.
  section_name_iterator sections_by_name_end() {
    return Sections.get<by_name>().end();
  }
  /// \brief Return a constant iterator to the element following the last
  /// Section.
  const_section_name_iterator sections_by_name_end() const {
    return Sections.get<by_name>().end();
  }
  /// \brief Return a range of the sections (\ref Section).
  section_range sections() {
    return boost::make_iterator_range(sections_begin(), sections_end());
  }
  /// \brief Return a constant range of the sections (\ref Section).
  const_section_range sections() const {
    return boost::make_iterator_range(sections_begin(), sections_end());
  }

  /// \brief Remove a \ref Section object located in this module.
  ///
  /// \param S The \ref Section object to remove.
  ///
  /// \return Whether or not the operation succeeded. This operation can
  /// fail if the node to remove is not actually part of this node to begin
  /// with.
  bool removeSection(Section* S) {
    S->removeFromIndices();
    auto& Index = Sections.get<by_pointer>();
    if (auto Iter = Index.find(S); Iter != Index.end()) {
      Index.erase(Iter);
      S->setModule(nullptr);
      return true;
    }
    return false;
  }

  /// \brief Move a \ref Section object to be located in this module.
  ///
  /// \param S The \ref Section object to add.
  Section* addSection(Section* S) {
    if (S->getModule()) {
      S->getModule()->removeSection(S);
    }
    Sections.emplace(S);
    S->setModule(this);
    S->addToIndices();
    return S;
  }

  /// \brief Find a Section containing an address.
  ///
  /// \param X The address to look up.
  ///
  /// \return The range of Sections containing the address.
  section_subrange findSectionsIn(Addr X) {
    if (auto It = SectionAddrs.find(X); It != SectionAddrs.end()) {
      return boost::make_iterator_range(It->second.begin(), It->second.end());
    }
    return {};
  }

  /// \brief Find a Section containing an address.
  ///
  /// \param X The address to look up.
  ///
  /// \return The range of Sections containing the address.
  const_section_subrange findSectionsIn(Addr X) const {
    if (auto It = SectionAddrs.find(X); It != SectionAddrs.end()) {
      return boost::make_iterator_range(It->second.begin(), It->second.end());
    }
    return {};
  }

  /// \brief Find all the sections that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref Section objects that are at the address \p A.
  section_range findSectionsAt(Addr A) {
    auto Pair = Sections.get<by_address>().equal_range(A);
    return boost::make_iterator_range(section_iterator(Pair.first),
                                      section_iterator(Pair.second));
  }

  /// \brief Find all the sections that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref Section objects that are between the addresses.
  section_range findSectionsAt(Addr Low, Addr High) {
    auto& Index = Sections.get<by_address>();
    return boost::make_iterator_range(
        section_iterator(Index.lower_bound(Low)),
        section_iterator(Index.lower_bound(High)));
  }

  /// \brief Find all the sections that start at an address.
  ///
  /// \param A The address to look up.
  ///
  /// \return A range of \ref Section objects that are at the address \p A.
  const_section_range findSectionsAt(Addr A) const {
    auto Pair = Sections.get<by_address>().equal_range(A);
    return boost::make_iterator_range(const_section_iterator(Pair.first),
                                      const_section_iterator(Pair.second));
  }

  /// \brief Find all the sections that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref Section objects that are between the addresses.
  const_section_range findSectionsAt(Addr Low, Addr High) const {
    auto& Index = Sections.get<by_address>();
    return boost::make_iterator_range(
        const_section_iterator(Index.lower_bound(Low)),
        const_section_iterator(Index.lower_bound(High)));
  }

  /// \brief Find a Section by name.
  ///
  /// \param X The name to look up.
  ///
  /// \return An iterator to the first Section with the requested name or
  /// \ref section_by_name_end() if not found.
  section_name_iterator findSections(const std::string& X) {
    return Sections.get<by_name>().find(X);
  }

  /// \brief Find a Section by name.
  ///
  /// \param X The name to look up.
  ///
  /// \return An iterator to the first Section with the requested name or
  /// \ref section_by_name_end() if not found.
  const_section_name_iterator findSections(const std::string& X) const {
    return Sections.get<by_name>().find(X);
  }

  /// @}
  // (end group of Section-related types and functions)

  /// \name ByteInterval-Related Public Types and Functions
  /// @{

  /// \brief Iterator over \ref ByteInterval objects.
  using byte_interval_iterator =
      MergeSortedIterator<Section::byte_interval_iterator,
                          AddressLess<ByteInterval>>;
  /// \brief Range of \ref ByteInterval objects.
  using byte_interval_range = boost::iterator_range<byte_interval_iterator>;
  /// \brief Sub-range of \ref ByteInterval objects overlapping addresses.
  using byte_interval_subrange = boost::iterator_range<MergeSortedIterator<
      Section::byte_interval_subrange::iterator, AddressLess<ByteInterval>>>;
  /// \brief Const iterator over \ref ByteInterval objects.
  using const_byte_interval_iterator =
      MergeSortedIterator<Section::const_byte_interval_iterator,
                          AddressLess<ByteInterval>>;
  /// \brief Const range of \ref ByteInterval objects.
  using const_byte_interval_range =
      boost::iterator_range<const_byte_interval_iterator>;
  /// \brief Sub-range of \ref ByteInterval objects overlapping addresses.
  using const_byte_interval_subrange = boost::iterator_range<
      MergeSortedIterator<Section::const_byte_interval_subrange::iterator,
                          AddressLess<ByteInterval>>>;

  /// \brief Return an iterator to the first \ref ByteInterval.
  byte_interval_iterator byte_intervals_begin() {
    return byte_interval_iterator(
        boost::make_transform_iterator(this->sections_begin(),
                                       NodeToByteIntervalRange<Section>()),
        boost::make_transform_iterator(this->sections_end(),
                                       NodeToByteIntervalRange<Section>()));
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
        boost::make_transform_iterator(
            this->sections_begin(), NodeToByteIntervalRange<const Section>()),
        boost::make_transform_iterator(
            this->sections_end(), NodeToByteIntervalRange<const Section>()));
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindByteIntervalsIn<Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindByteIntervalsIn<Section>(A))),
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
                this->sections_begin(), FindByteIntervalsIn<const Section>(A)),
            boost::make_transform_iterator(
                this->sections_end(), FindByteIntervalsIn<const Section>(A))),
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindByteIntervalsAt<Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindByteIntervalsAt<Section>(A))),
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
                this->sections_begin(),
                FindByteIntervalsBetween<Section>(Low, High)),
            boost::make_transform_iterator(
                this->sections_end(),
                FindByteIntervalsBetween<Section>(Low, High))),
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
                this->sections_begin(), FindByteIntervalsAt<const Section>(A)),
            boost::make_transform_iterator(
                this->sections_end(), FindByteIntervalsAt<const Section>(A))),
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
                this->sections_begin(),
                FindByteIntervalsBetween<const Section>(Low, High)),
            boost::make_transform_iterator(
                this->sections_end(),
                FindByteIntervalsBetween<const Section>(Low, High))),
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
      MergeSortedIterator<Section::block_iterator, BlockAddressLess>;
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
      MergeSortedIterator<Section::block_subrange::iterator, BlockAddressLess>>;
  /// \brief Iterator over blocks.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_block_iterator =
      MergeSortedIterator<Section::const_block_iterator, BlockAddressLess>;
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
      Section::const_block_subrange::iterator, BlockAddressLess>>;

  /// \brief Return an iterator to the first block.
  block_iterator blocks_begin() {
    return block_iterator(
        boost::make_transform_iterator(this->sections_begin(),
                                       NodeToBlockRange<Section>()),
        boost::make_transform_iterator(this->sections_end(),
                                       NodeToBlockRange<Section>()));
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
        boost::make_transform_iterator(this->sections_begin(),
                                       NodeToBlockRange<const Section>()),
        boost::make_transform_iterator(this->sections_end(),
                                       NodeToBlockRange<const Section>()));
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindBlocksIn<Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindBlocksIn<Section>(A))),
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindBlocksIn<const Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindBlocksIn<const Section>(A))),
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindBlocksAt<Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindBlocksAt<Section>(A))),
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
                this->sections_begin(), FindBlocksBetween<Section>(Low, High)),
            boost::make_transform_iterator(
                this->sections_end(), FindBlocksBetween<Section>(Low, High))),
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindBlocksAt<const Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindBlocksAt<const Section>(A))),
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
                this->sections_begin(),
                FindBlocksBetween<const Section>(Low, High)),
            boost::make_transform_iterator(
                this->sections_end(),
                FindBlocksBetween<const Section>(Low, High))),
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
      MergeSortedIterator<Section::code_block_iterator, AddressLess<CodeBlock>>;
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
      Section::code_block_subrange::iterator, AddressLess<CodeBlock>>>;
  /// \brief Iterator over \ref CodeBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_code_block_iterator =
      MergeSortedIterator<Section::const_code_block_iterator,
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
      Section::const_code_block_subrange::iterator, AddressLess<CodeBlock>>>;

  /// \brief Return an iterator to the first \ref CodeBlock.
  code_block_iterator code_blocks_begin() {
    return code_block_iterator(
        boost::make_transform_iterator(this->sections_begin(),
                                       NodeToCodeBlockRange<Section>()),
        boost::make_transform_iterator(this->sections_end(),
                                       NodeToCodeBlockRange<Section>()));
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
        boost::make_transform_iterator(this->sections_begin(),
                                       NodeToCodeBlockRange<const Section>()),
        boost::make_transform_iterator(this->sections_end(),
                                       NodeToCodeBlockRange<const Section>()));
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindCodeBlocksIn<Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindCodeBlocksIn<Section>(A))),
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindCodeBlocksIn<const Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindCodeBlocksIn<const Section>(A))),
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindCodeBlocksAt<Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindCodeBlocksAt<Section>(A))),
        code_block_range::iterator());
  }

  /// \brief Find all the code blocks that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref CodeBlock objects that are between the addresses.
  code_block_range findCodeBlocksAt(Addr Low, Addr High) {
    return code_block_range(code_block_range::iterator(
                                boost::make_transform_iterator(
                                    this->sections_begin(),
                                    FindCodeBlocksBetween<Section>(Low, High)),
                                boost::make_transform_iterator(
                                    this->sections_end(),
                                    FindCodeBlocksBetween<Section>(Low, High))),
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindCodeBlocksAt<const Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindCodeBlocksAt<const Section>(A))),
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
                this->sections_begin(),
                FindCodeBlocksBetween<const Section>(Low, High)),
            boost::make_transform_iterator(
                this->sections_end(),
                FindCodeBlocksBetween<const Section>(Low, High))),
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
      MergeSortedIterator<Section::data_block_iterator, AddressLess<DataBlock>>;
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
      Section::data_block_subrange::iterator, AddressLess<DataBlock>>>;
  /// \brief Iterator over \ref DataBlock objects.
  ///
  /// Blocks are yielded in address order, ascending. If two blocks have the
  /// same address, thier order is not specified.
  using const_data_block_iterator =
      MergeSortedIterator<Section::const_data_block_iterator,
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
      Section::const_data_block_subrange::iterator, AddressLess<DataBlock>>>;

  /// \brief Return an iterator to the first \ref DataBlock.
  data_block_iterator data_blocks_begin() {
    return data_block_iterator(
        boost::make_transform_iterator(this->sections_begin(),
                                       NodeToDataBlockRange<Section>()),
        boost::make_transform_iterator(this->sections_end(),
                                       NodeToDataBlockRange<Section>()));
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
        boost::make_transform_iterator(this->sections_begin(),
                                       NodeToDataBlockRange<const Section>()),
        boost::make_transform_iterator(this->sections_end(),
                                       NodeToDataBlockRange<const Section>()));
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindDataBlocksIn<Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindDataBlocksIn<Section>(A))),
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindDataBlocksIn<const Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindDataBlocksIn<const Section>(A))),
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindDataBlocksAt<Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindDataBlocksAt<Section>(A))),
        data_block_range::iterator());
  }

  /// \brief Find all the data blocks that start between a range of addresses.
  ///
  /// \param Low  The low address, inclusive.
  /// \param High The high address, exclusive.
  ///
  /// \return A range of \ref DataBlock objects that are between the addresses.
  data_block_range findDataBlocksAt(Addr Low, Addr High) {
    return data_block_range(data_block_range::iterator(
                                boost::make_transform_iterator(
                                    this->sections_begin(),
                                    FindDataBlocksBetween<Section>(Low, High)),
                                boost::make_transform_iterator(
                                    this->sections_end(),
                                    FindDataBlocksBetween<Section>(Low, High))),
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindDataBlocksAt<const Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindDataBlocksAt<const Section>(A))),
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
                this->sections_begin(),
                FindDataBlocksBetween<const Section>(Low, High)),
            boost::make_transform_iterator(
                this->sections_end(),
                FindDataBlocksBetween<const Section>(Low, High))),
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
      MergeSortedIterator<Section::symbolic_expression_iterator,
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
      Section::const_symbolic_expression_iterator,
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
            this->sections_begin(), NodeToSymbolicExpressionRange<Section>()),
        boost::make_transform_iterator(
            this->sections_end(), NodeToSymbolicExpressionRange<Section>()));
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
            this->sections_begin(),
            NodeToSymbolicExpressionRange<const Section>()),
        boost::make_transform_iterator(
            this->sections_end(),
            NodeToSymbolicExpressionRange<const Section>()));
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindSymExprsAt<Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindSymExprsAt<Section>(A))),
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
                this->sections_begin(),
                FindSymExprsBetween<Section>(Low, High)),
            boost::make_transform_iterator(
                this->sections_end(), FindSymExprsBetween<Section>(Low, High))),
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
            boost::make_transform_iterator(this->sections_begin(),
                                           FindSymExprsAt<const Section>(A)),
            boost::make_transform_iterator(this->sections_end(),
                                           FindSymExprsAt<const Section>(A))),
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
                this->sections_begin(),
                FindSymExprsBetween<const Section>(Low, High)),
            boost::make_transform_iterator(
                this->sections_end(),
                FindSymExprsBetween<const Section>(Low, High))),
        const_symbolic_expression_range::iterator());
  }
  /// @}
  // (end group of SymbolicExpression-related types and functions)

  /// \brief Return the address of this module, if known.
  ///
  /// The address is calculated from the \ref Section objects in this
  /// module. More specifically, if the address of all sections in this
  /// module are fixed, then it will return the address of the section lowest
  /// in memory. If any one section does not have an address, then this function
  /// will return \ref std::nullopt, as the address is not calculable in that
  /// case. Note that a module with no sections in it has no address or size,
  /// so it will return \ref std::nullopt in that case.
  ///
  /// Note that this returns the current address of this module, which can be
  /// distinct from the value returned by \ref getPreferredAddr if the module
  /// has been moved (see \ref isRelocated for details).
  std::optional<Addr> getAddress() const {
    if (Sections.empty()) {
      return std::nullopt;
    }

    Addr result{std::numeric_limits<Addr::value_type>::max()};
    for (const auto* Section : Sections) {
      if (auto A = Section->getAddress()) {
        result = std::min(result, *A);
      } else {
        return std::nullopt;
      }
    }
    return result;
  }

  /// \brief Return the size of this module, if known.
  ///
  /// The size is calculated from the \ref Section objects in this module.
  /// More specifically, if the address of all sections in this module
  /// are fixed, then it will return the difference between the lowest and
  /// highest address among the sections. If any one section does not have an
  /// address, then this function will return \ref std::nullopt, as the size is
  /// not calculable in that case. Note that a module with no sections in it
  /// has no address or size, so it will return \ref std::nullopt in that case.
  std::optional<uint64_t> getSize() const {
    if (Sections.empty()) {
      return std::nullopt;
    }

    Addr LowAddr{std::numeric_limits<Addr::value_type>::max()};
    Addr HighAddr{0};

    for (const auto* Section : Sections) {
      auto A = Section->getAddress();
      auto S = Section->getSize();
      if (A && S) {
        LowAddr = std::min(LowAddr, *A);
        HighAddr = std::max(HighAddr, *A + *S);
      } else {
        return std::nullopt;
      }
    }

    return static_cast<uint64_t>(HighAddr - LowAddr);
  }

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing Module.
  using MessageType = proto::Module;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a Module from a protobuf message.
  ///
  /// \param C   The Context in which the deserialized Module will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized Module object, or null on failure.
  static Module* fromProtobuf(Context& C, IR* Parent,
                              const MessageType& Message);

  static bool classof(const Node* N) { return N->getKind() == Kind::Module; }
  /// @endcond

private:
  void setIR(IR* I) { Parent = I; }

  IR* Parent{nullptr};
  std::string BinaryPath;
  Addr PreferredAddr;
  int64_t RebaseDelta{0};
  gtirb::FileFormat FileFormat{FileFormat::Undefined};
  gtirb::ISA Isa{ISA::Undefined};
  std::string Name;
  CodeBlock* EntryPoint{nullptr};
  CFG Cfg;
  ProxyBlockSet ProxyBlocks;
  SectionSet Sections;
  SectionIntMap SectionAddrs;
  SymbolSet Symbols;

  friend class Context; // Allow Context to construct new Modules.
  friend class IR;      // Allow IRs to call setIR.
  friend class Node;    // Allow Node::mutateIndices, etc. to set indices.
};

} // namespace gtirb

#endif // GTIRB_MODULE_H
