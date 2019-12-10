//===- Module.hpp -----------------------------------------------*- C++ -*-===//
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
enum class ISAID : uint8_t {
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
  struct by_name {};
  struct by_pointer {};
  struct by_referent {};

  // Helper template for implementing address-based ordering of
  // multi-containers.

  template <typename T> struct addr_order {
    bool operator()(const T* t1, const T* t2) const {
      return *t1->getAddress() < *t2->getAddress();
    }
  };

  template <typename T> struct block_addr_order {
    using pair = std::pair<ByteInterval*, T*>;
    bool operator()(const pair& p1, const pair& p2) const {
      return *p1.second->getAddress() < *p2.second->getAddress();
    }
  };

  // Helper templates for implementing transform iterators from maps.

  template <typename A, typename B> struct pair_to_node {
    B& operator()(const std::pair<A, B*> pair) const { return *pair.second; }
  };

  // Helper function for extracting the referent of a Symbol.

  static const Node* get_symbol_referent(const Symbol& s) {
    if (std::optional<const Node*> res =
            s.visit([](const Node* n) { return n; }))
      return *res;
    return nullptr;
  }

  using ProxyBlockSet = std::unordered_set<ProxyBlock*>;

  using SectionSet = boost::multi_index::multi_index_container<
      Section*, boost::multi_index::indexed_by<
                    boost::multi_index::ordered_non_unique<
                        boost::multi_index::tag<by_name>,
                        boost::multi_index::const_mem_fun<
                            Section, const std::string&, &Section::getName>>,
                    boost::multi_index::hashed_unique<
                        boost::multi_index::tag<by_pointer>,
                        boost::multi_index::identity<Section*>>>>;
  using SectionIntMap =
      boost::icl::interval_map<Addr, std::set<Section*, addr_order<Section>>>;

  using SymbolSet = boost::multi_index::multi_index_container<
      Symbol*, boost::multi_index::indexed_by<
                   boost::multi_index::ordered_non_unique<
                       boost::multi_index::tag<by_name>,
                       boost::multi_index::const_mem_fun<
                           Symbol, const std::string&, &Symbol::getName>>,
                   boost::multi_index::hashed_unique<
                       boost::multi_index::tag<by_pointer>,
                       boost::multi_index::identity<Symbol*>>,
                   boost::multi_index::hashed_non_unique<
                       boost::multi_index::tag<by_referent>,
                       boost::multi_index::global_fun<
                           const Symbol&, const Node*, &get_symbol_referent>>>>;
  using SymbolIntMap = std::multimap<Addr, Symbol*>;

  using ByteIntervalSet = std::unordered_set<ByteInterval*>;
  using ByteIntervalIntMap = boost::icl::interval_map<
      Addr, std::set<ByteInterval*, addr_order<ByteInterval>>>;

  using CodeBlockSet = std::unordered_set<CodeBlock*>;
  using CodeBlockIntMap =
      boost::icl::interval_map<Addr,
                               std::set<std::pair<ByteInterval*, CodeBlock*>,
                                        block_addr_order<CodeBlock>>>;

  using DataBlockSet = std::unordered_set<DataBlock*>;
  using DataBlockIntMap =
      boost::icl::interval_map<Addr,
                               std::set<std::pair<ByteInterval*, DataBlock*>,
                                        block_addr_order<DataBlock>>>;

  using SymbolicExpressionSet = std::unordered_set<
      std::pair<ByteInterval*, SymbolicExpression*>,
      boost::hash<std::pair<ByteInterval*, SymbolicExpression*>>>;
  using SymbolicExpressionIntMap = std::map<Addr, SymbolicExpression*>;

  Module(Context& C) : AuxDataContainer(C, Kind::Module) {}
  Module(Context& C, IR* P) : AuxDataContainer(C, Kind::Module), Parent(P) {}

  template <size_t I> struct ExtractNth {
    template <typename ParamTy> auto& operator()(ParamTy& V) const {
      return std::get<I>(V);
    }
  };

public:
  /// \brief Create a Module object in its default state.
  ///
  /// \param C      The Context in which this object will be held.
  /// \param Parent The \ref IR this module belongs to.
  ///
  /// \return The newly created object.
  static Module* Create(Context& C, IR* Parent) {
    return C.Create<Module>(C, Parent);
  }

  /// \brief Get the \ref IR this module belongs to.
  IR* getIR() const { return Parent; }

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
  void setISAID(gtirb::ISAID X) { IsaID = X; }

  /// \brief Get the ISA of the instructions in this Module.
  ///
  /// \return The ISA ID.
  gtirb::ISAID getISAID() const { return IsaID; }

  /// \brief Get the entry point of this module, or null if not present.
  CodeBlock* getEntryPoint() const { return EntryPoint; }

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
  void removeProxyBlock(ProxyBlock* B) {
    ProxyBlocks.erase(B);
    B->setModule(nullptr);
  }

  /// \brief Move a \ref ProxyBlock object to be located in this module.
  ///
  /// \param S The \ref ProxyBlock object to add.
  void moveProxyBlock(ProxyBlock* B) {
    if (B->getModule()) {
      B->getModule()->removeProxyBlock(B);
    }
    ProxyBlocks.insert(B);
    B->setModule(this);
  }

  /// \brief Creates a new \ref ProxyBlock in this module.
  ///
  /// \tparam Args  The arguments to construct a \ref ProxyBlock.
  template <typename... Args> ProxyBlock* addProxyBlock(Context& C, Args... A) {
    auto B = ProxyBlock::Create(C, this, A...);
    ProxyBlocks.insert(B);
    return B;
  }

  /// @}
  // (end of ProxyBlock-Related Public Types and Functions)

  /// \name Symbol-Related Public Types and Functions
  /// @{

  /// \brief Iterator over symbols (\ref Symbol).
  ///
  /// This iterator returns symbols in name order. If two Symbols have the same
  /// name, their order is unspecified.
  using symbol_iterator =
      boost::indirect_iterator<SymbolSet::index<by_name>::type::iterator>;
  /// \brief Range of symbols (\ref Symbol).
  ///
  /// This range returns symbols in name order. If two Symbols have the same
  /// name, their order is unspecified.
  using symbol_range = boost::iterator_range<symbol_iterator>;
  /// \brief Constant iterator over symbols (\ref Symbol).
  ///
  /// This iterator returns symbols in name order. If two Symbols have the same
  /// name, their order is unspecified.
  using const_symbol_iterator =
      boost::indirect_iterator<SymbolSet::index<by_name>::type::const_iterator,
                               const Symbol>;
  /// \brief Constant range of symbols (\ref Symbol).
  ///
  /// This range returns symbols in name order. If two Symbols have the same
  /// name, their order is unspecified.
  using const_symbol_range = boost::iterator_range<const_symbol_iterator>;

  /// \brief Iterator over symbols (\ref Symbol).
  ///
  /// This iterator returns symbols in address order. If two Symbols have the
  /// same address, their order is unspecified.
  using symbol_addr_iterator =
      boost::transform_iterator<pair_to_node<Addr, Symbol>,
                                SymbolIntMap::iterator>;
  /// \brief Range of symbols (\ref Symbol).
  ///
  /// This range returns symbols in address order. If two Symbols have the same
  /// address, their order is unspecified.
  using symbol_addr_range = boost::iterator_range<symbol_addr_iterator>;
  /// \brief Constant iterator over symbols (\ref Symbol).
  ///
  /// This iterator returns symbols in address order. If two Symbols have the
  /// same address, their order is unspecified.
  using const_symbol_addr_iterator =
      boost::transform_iterator<pair_to_node<Addr, const Symbol>,
                                SymbolIntMap::const_iterator, const Symbol&,
                                const Symbol>;
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
  symbol_iterator symbol_begin() {
    return symbol_iterator(Symbols.get<by_name>().begin());
  }
  /// \brief Return a constant iterator to the first Symbol.
  const_symbol_iterator symbol_begin() const {
    return const_symbol_iterator(Symbols.get<by_name>().begin());
  }
  /// \brief Return an iterator to the element following the last Symbol.
  symbol_iterator symbol_end() {
    return symbol_iterator(Symbols.get<by_name>().end());
  }
  /// \brief Return a constant iterator to the element following the last
  /// Symbol.
  const_symbol_iterator symbol_end() const {
    return const_symbol_iterator(Symbols.get<by_name>().end());
  }
  /// \brief Return a range of the symbols (\ref Symbol).
  symbol_range symbols() {
    return boost::make_iterator_range(symbol_begin(), symbol_end());
  }
  /// \brief Return a constant range of the symbols (\ref Symbol).
  const_symbol_range symbols() const {
    return boost::make_iterator_range(symbol_begin(), symbol_end());
  }

  /// \brief Remove a \ref Symbol object located in this module.
  ///
  /// \param S The \ref Symbol object to remove.
  void removeSymbol(Symbol* S) {
    auto& index = Symbols.get<by_pointer>();
    index.erase(index.find(S));
    S->setModule(nullptr);
  }

  /// \brief Move a \ref Symbol object to be located in this module.
  ///
  /// \param S The \ref Symbol object to add.
  void moveSymbol(Symbol* S) {
    if (S->getModule()) {
      S->getModule()->removeSymbol(S);
    }
    Symbols.emplace(S);
    S->setModule(this);
  }

  /// \brief Creates a new \ref Symbol in this module.
  ///
  /// \tparam Args  The arguments to construct a \ref Symbol.
  template <typename... Args> Symbol* addSymbol(Context& C, Args... A) {
    auto N = Symbol::Create(C, this, A...);
    Symbols.emplace(N);
    return N;
  }

  /// \brief Find symbols by name
  ///
  /// \param N The name to look up.
  ///
  /// \return A possibly empty range of all the symbols with the
  /// given name.
  symbol_range findSymbols(const std::string& N) {
    auto Found = Symbols.get<by_name>().equal_range(N);
    return boost::make_iterator_range(Found.first, Found.second);
  }

  /// \brief Find symbols by name
  ///
  /// \param N The name to look up.
  ///
  /// \return A possibly empty constant range of all the symbols with the
  /// given name.
  const_symbol_range findSymbols(const std::string& N) const {
    auto Found = Symbols.get<by_name>().equal_range(N);
    return boost::make_iterator_range(Found.first, Found.second);
  }

  /// \brief Find symbols by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return A possibly empty range of all the symbols containing the given
  /// address.
  symbol_addr_range findSymbols(Addr X) {
    auto Found = SymbolAddrs.equal_range(X);
    return boost::make_iterator_range(
        symbol_addr_iterator(Found.first, pair_to_node<Addr, Symbol>()),
        symbol_addr_iterator(Found.second, pair_to_node<Addr, Symbol>()));
  }

  /// \brief Find symbols by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return A possibly empty constant range of all the symbols containing the
  /// given address.
  const_symbol_addr_range findSymbols(Addr X) const {
    auto Found = SymbolAddrs.equal_range(X);
    return boost::make_iterator_range(
        const_symbol_addr_iterator(Found.first,
                                   pair_to_node<Addr, const Symbol>()),
        const_symbol_addr_iterator(Found.second,
                                   pair_to_node<Addr, const Symbol>()));
  }

  /// \brief Find symbols by a range of addresses.
  ///
  /// \param Lower The lower-bounded address to look up.
  /// \param Upper The upper-bounded address to look up.
  ///
  /// \return A possibly empty range of all the symbols within the given
  /// address range. Searches the range [Lower, Upper).
  symbol_addr_range findSymbols(Addr Lower, Addr Upper) {
    return boost::make_iterator_range(
        symbol_addr_iterator(SymbolAddrs.lower_bound(Lower),
                             pair_to_node<Addr, Symbol>()),
        symbol_addr_iterator(SymbolAddrs.lower_bound(Upper),
                             pair_to_node<Addr, Symbol>()));
  }

  /// \brief Find symbols by a range of addresses.
  ///
  /// \param Lower The lower-bounded address to look up.
  /// \param Upper The upper-bounded address to look up.
  ///
  /// \return A possibly empty constant range of all the symbols within the
  /// given address range. Searches the range [Lower, Upper).
  const_symbol_addr_range findSymbols(Addr Lower, Addr Upper) const {
    return boost::make_iterator_range(
        const_symbol_addr_iterator(SymbolAddrs.lower_bound(Lower),
                                   pair_to_node<Addr, const Symbol>()),
        const_symbol_addr_iterator(SymbolAddrs.lower_bound(Upper),
                                   pair_to_node<Addr, const Symbol>()));
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

  /// \brief Get the associated Control Flow Graph (\ref CFG).
  ///
  /// \return The associated CFG.
  const CFG& getCFG() const { return Cfg; }

  /// \brief Get a const reference to the associated Control Flow Graph
  /// (\ref CFG).
  ///
  /// \return The associated CFG.
  CFG& getCFG() { return Cfg; }

  /// \name CodeBlock-Related Public Types and Functions
  /// @{

  /// \brief Iterator over code_blocks (\ref CodeBlock).
  using code_block_iterator = boost::indirect_iterator<CodeBlockSet::iterator>;
  /// \brief Range of code_blocks (\ref CodeBlock).
  using code_block_range = boost::iterator_range<code_block_iterator>;
  /// \brief Sub-range of code blocks overlapping an address (\ref CodeBlock).
  using code_block_subrange = boost::iterator_range<
      boost::transform_iterator<pair_to_node<ByteInterval*, CodeBlock>,
                                CodeBlockIntMap::codomain_type::iterator>>;

  /// \brief Constant iterator over code_blocks (\ref CodeBlock).
  using const_code_block_iterator =
      boost::indirect_iterator<CodeBlockSet::const_iterator, const CodeBlock&>;
  /// \brief Constant range of code_blocks (\ref CodeBlock).
  using const_code_block_range =
      boost::iterator_range<const_code_block_iterator>;
  /// \brief Sub-range of code blocks overlapping an address (\ref CodeBlock).
  using const_code_block_subrange =
      boost::iterator_range<boost::transform_iterator<
          pair_to_node<ByteInterval*, const CodeBlock>,
          CodeBlockIntMap::codomain_type::const_iterator>>;

  /// \brief Return an iterator to the first CodeBlock.
  code_block_iterator code_blocks_begin() {
    return code_block_iterator(CodeBlocks.begin());
  }
  /// \brief Return a constant iterator to the first CodeBlock.
  const_code_block_iterator code_blocks_begin() const {
    return const_code_block_iterator(CodeBlocks.begin());
  }
  /// \brief Return an iterator to the element following the last CodeBlock.
  code_block_iterator code_blocks_end() {
    return code_block_iterator(CodeBlocks.end());
  }
  /// \brief Return a constant iterator to the element following the last
  /// CodeBlock.
  const_code_block_iterator code_blocks_end() const {
    return const_code_block_iterator(CodeBlocks.end());
  }
  /// \brief Return a range of the code_blocks (\ref CodeBlock).
  code_block_range code_blocks() {
    return boost::make_iterator_range(code_blocks_begin(), code_blocks_end());
  }
  /// \brief Return a constant range of the code_blocks (\ref CodeBlock).
  const_code_block_range code_blocks() const {
    return boost::make_iterator_range(code_blocks_begin(), code_blocks_end());
  }

  /// \brief Find a \ref CodeBlock containing an address.
  ///
  /// \param X The address to look up.
  ///
  /// \return The range of CodeBlocks containing the address.
  code_block_subrange findCodeBlock(Addr X) {
    auto it = CodeBlockAddrs.find(X);
    if (it == CodeBlockAddrs.end())
      return {};
    return boost::make_iterator_range(
        code_block_subrange::iterator(it->second.begin(),
                                      pair_to_node<ByteInterval*, CodeBlock>()),
        code_block_subrange::iterator(
            it->second.end(), pair_to_node<ByteInterval*, CodeBlock>()));
  }

  /// \brief Find a \ref CodeBlock containing an address.
  ///
  /// \param X The address to look up.
  ///
  /// \return The range of CodeBlocks containing the address.
  const_code_block_subrange findCodeBlock(Addr X) const {
    auto it = CodeBlockAddrs.find(X);
    if (it == CodeBlockAddrs.end())
      return {};
    return boost::make_iterator_range(
        const_code_block_subrange::iterator(
            it->second.begin(), pair_to_node<ByteInterval*, const CodeBlock>()),
        const_code_block_subrange::iterator(
            it->second.end(), pair_to_node<ByteInterval*, const CodeBlock>()));
  }

  /// @}

  /// \name DataBlock-Related Public Types and Functions
  /// @{

  /// \brief Iterator over data_blocks (\ref DataBlock).
  using data_block_iterator = boost::indirect_iterator<DataBlockSet::iterator>;
  /// \brief Range of data_blocks (\ref DataBlock).
  using data_block_range = boost::iterator_range<data_block_iterator>;
  /// \brief Sub-range of data blocks overlapping an address (\ref DataBlock).
  using data_block_subrange = boost::iterator_range<
      boost::transform_iterator<pair_to_node<ByteInterval*, DataBlock>,
                                DataBlockIntMap::codomain_type::iterator>>;

  /// \brief Constant iterator over data_blocks (\ref DataBlock).
  using const_data_block_iterator =
      boost::indirect_iterator<DataBlockSet::const_iterator, const DataBlock&>;
  /// \brief Constant range of data_blocks (\ref DataBlock).
  using const_data_block_range =
      boost::iterator_range<const_data_block_iterator>;
  /// \brief Sub-range of data blocks overlapping an address (\ref DataBlock).
  using const_data_block_subrange =
      boost::iterator_range<boost::transform_iterator<
          pair_to_node<ByteInterval*, const DataBlock>,
          DataBlockIntMap::codomain_type::const_iterator>>;

  /// \brief Return an iterator to the first DataBlock.
  data_block_iterator data_blocks_begin() {
    return data_block_iterator(DataBlocks.begin());
  }
  /// \brief Return a constant iterator to the first DataBlock.
  const_data_block_iterator data_blocks_begin() const {
    return const_data_block_iterator(DataBlocks.begin());
  }
  /// \brief Return an iterator to the element following the last DataBlock.
  data_block_iterator data_blocks_end() {
    return data_block_iterator(DataBlocks.end());
  }
  /// \brief Return a constant iterator to the element following the last
  /// DataBlock.
  const_data_block_iterator data_blocks_end() const {
    return const_data_block_iterator(DataBlocks.end());
  }
  /// \brief Return a range of the data_blocks (\ref DataBlock).
  data_block_range data_blocks() {
    return boost::make_iterator_range(data_blocks_begin(), data_blocks_end());
  }
  /// \brief Return a constant range of the data_blocks (\ref DataBlock).
  const_data_block_range data_blocks() const {
    return boost::make_iterator_range(data_blocks_begin(), data_blocks_end());
  }

  /// \brief Find a \ref DataBlock containing an address.
  ///
  /// \param X The address to look up.
  ///
  /// \return The range of DataBlocks containing the address.
  data_block_subrange findDataBlock(Addr X) {
    auto it = DataBlockAddrs.find(X);
    if (it == DataBlockAddrs.end())
      return {};
    return boost::make_iterator_range(
        data_block_subrange::iterator(it->second.begin(),
                                      pair_to_node<ByteInterval*, DataBlock>()),
        data_block_subrange::iterator(
            it->second.end(), pair_to_node<ByteInterval*, DataBlock>()));
  }

  /// \brief Find a \ref DataBlock containing an address.
  ///
  /// \param X The address to look up.
  ///
  /// \return The range of DataBlocks containing the address.
  const_data_block_subrange findDataBlock(Addr X) const {
    auto it = DataBlockAddrs.find(X);
    if (it == DataBlockAddrs.end())
      return {};
    return boost::make_iterator_range(
        const_data_block_subrange::iterator(
            it->second.begin(), pair_to_node<ByteInterval*, const DataBlock>()),
        const_data_block_subrange::iterator(
            it->second.end(), pair_to_node<ByteInterval*, const DataBlock>()));
  }

  /// @}

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
  section_iterator section_begin() { return Sections.begin(); }
  /// \brief Return a constant iterator to the first Section.
  const_section_iterator section_begin() const { return Sections.begin(); }
  /// \brief Return an iterator to the first Section.
  section_name_iterator section_by_name_begin() {
    return Sections.get<by_name>().begin();
  }
  /// \brief Return a constant iterator to the first Section.
  const_section_name_iterator section_by_name_begin() const {
    return Sections.get<by_name>().begin();
  }
  /// \brief Return an iterator to the element following the last Section.
  section_iterator section_end() { return Sections.end(); }
  /// \brief Return a constant iterator to the element following the last
  /// Section.
  const_section_iterator section_end() const { return Sections.end(); }
  /// \brief Return an iterator to the element following the last Section.
  section_name_iterator section_by_name_end() {
    return Sections.get<by_name>().end();
  }
  /// \brief Return a constant iterator to the element following the last
  /// Section.
  const_section_name_iterator section_by_name_end() const {
    return Sections.get<by_name>().end();
  }
  /// \brief Return a range of the sections (\ref Section).
  section_range sections() {
    return boost::make_iterator_range(section_begin(), section_end());
  }
  /// \brief Return a constant range of the sections (\ref Section).
  const_section_range sections() const {
    return boost::make_iterator_range(section_begin(), section_end());
  }

  /// \brief Remove a \ref Section object located in this module.
  ///
  /// \param S The \ref Section object to remove.
  void removeSection(Section* S) {
    auto& index = Sections.get<by_pointer>();
    index.erase(index.find(S));
    S->setModule(nullptr);
  }

  /// \brief Move a \ref Section object to be located in this module.
  ///
  /// \param S The \ref Section object to add.
  void moveSection(Section* S) {
    if (S->getModule()) {
      S->getModule()->removeSection(S);
    }
    Sections.emplace(S);
    S->setModule(this);
  }

  /// \brief Creates a new \ref Section in this module.
  ///
  /// \tparam Args  The arguments to construct a \ref Section.
  template <typename... Args> Section* addSection(Context& C, Args... A) {
    auto N = Section::Create(C, this, A...);
    Sections.emplace(N);
    return N;
  }

  /// \brief Find a Section containing an address.
  ///
  /// \param X The address to look up.
  ///
  /// \return The range of Sections containing the address.
  section_subrange findSection(Addr X) {
    auto it = SectionAddrs.find(X);
    if (it == SectionAddrs.end())
      return {};
    return boost::make_iterator_range(it->second.begin(), it->second.end());
  }

  /// \brief Find a Section containing an address.
  ///
  /// \param X The address to look up.
  ///
  /// \return The range of Sections containing the address.
  const_section_subrange findSection(Addr X) const {
    auto it = SectionAddrs.find(X);
    if (it == SectionAddrs.end())
      return {};
    return boost::make_iterator_range(it->second.begin(), it->second.end());
  }

  /// \brief Find a Section by name.
  ///
  /// \param X The name to look up.
  ///
  /// \return An iterator to the first Section with the requested name or
  /// \ref section_by_name_end() if not found.
  section_name_iterator findSection(const std::string& X) {
    return Sections.get<by_name>().find(X);
  }

  /// \brief Find a Section by name.
  ///
  /// \param X The name to look up.
  ///
  /// \return An iterator to the first Section with the requested name or
  /// \ref section_by_name_end() if not found.
  const_section_name_iterator findSection(const std::string& X) const {
    return Sections.get<by_name>().find(X);
  }

  /// @}
  // (end group of Section-related types and functions)

  /// \name SymbolicExpression-Related Public Types and Functions
  /// @{
  /// \brief Iterator over symbolic expressions (\ref SymbolicExpression).
  using const_symbolic_expression_iterator = boost::transform_iterator<
      pair_to_node<ByteInterval*, const SymbolicExpression>,
      SymbolicExpressionSet::const_iterator>;
  /// \brief Range over symbolic expressions (\ref SymbolicExpression).
  using const_symbolic_expression_range =
      boost::iterator_range<const_symbolic_expression_iterator>;

  /// \brief Sub-range of \ref SymbolicExpression objects spanning addresses.
  using symbolic_expression_subrange = boost::iterator_range<
      boost::transform_iterator<pair_to_node<Addr, SymbolicExpression>,
                                SymbolicExpressionIntMap::iterator>>;
  /// \brief Sub-range of \ref SymbolicExpression objects spanning addresses.
  using const_symbolic_expression_subrange = boost::iterator_range<
      boost::transform_iterator<pair_to_node<Addr, const SymbolicExpression>,
                                SymbolicExpressionIntMap::const_iterator>>;

  const_symbolic_expression_iterator symbolic_expressions_begin() {
    return boost::make_transform_iterator(
        SymbolicExpressions.begin(),
        pair_to_node<ByteInterval*, const SymbolicExpression>());
  }

  const_symbolic_expression_iterator symbolic_expressions_end() {
    return boost::make_transform_iterator(
        SymbolicExpressions.end(),
        pair_to_node<ByteInterval*, const SymbolicExpression>());
  }

  const_symbolic_expression_range symbolic_expressions() {
    return const_symbolic_expression_range(symbolic_expressions_begin(),
                                           symbolic_expressions_end());
  }

  SymbolicExpression* findSymbolicExpression(Addr X) {
    auto it = SymbolicExpressionAddrs.find(X);
    if (it == SymbolicExpressionAddrs.end()) {
      return nullptr;
    } else {
      return it->second;
    }
  }

  const SymbolicExpression* findSymbolicExpression(Addr X) const {
    auto it = SymbolicExpressionAddrs.find(X);
    if (it == SymbolicExpressionAddrs.end()) {
      return nullptr;
    } else {
      return it->second;
    }
  }

  symbolic_expression_subrange findSymbolicExpression(Addr Lower, Addr Upper) {
    return boost::make_iterator_range(
        symbolic_expression_subrange::iterator(
            SymbolicExpressionAddrs.lower_bound(Lower),
            pair_to_node<Addr, SymbolicExpression>()),
        symbolic_expression_subrange::iterator(
            SymbolicExpressionAddrs.lower_bound(Upper),
            pair_to_node<Addr, SymbolicExpression>()));
  }

  const_symbolic_expression_subrange findSymbolicExpression(Addr Lower,
                                                            Addr Upper) const {
    return boost::make_iterator_range(
        const_symbolic_expression_subrange::iterator(
            SymbolicExpressionAddrs.lower_bound(Lower),
            pair_to_node<Addr, const SymbolicExpression>()),
        const_symbolic_expression_subrange::iterator(
            SymbolicExpressionAddrs.lower_bound(Upper),
            pair_to_node<Addr, const SymbolicExpression>()));
  }

  template <typename ExprType, typename... Args>
  SymbolicExpression& addSymbolicExpression(ByteInterval* BI, uint64_t O,
                                            Args... A) {
    SymbolicExpression& SE = BI->addSymbolicExpression<ExprType>(O, A...);
    SymbolicExpressions.insert(std::make_pair(BI, &SE));
    auto addr = BI->getAddress();
    if (addr.has_value()) {
      SymbolicExpressionAddrs[*addr] = &SE;
    }
    return SE;
  }

  /// @}
  // (end group of SymbolicExpression-related type aliases and methods)

  /// \name ByteInterval-Related Public Types and Functions
  /// @{

  /// @}
  // (end group of ByteInterval-related type aliases and methods)

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

  IR* Parent;
  std::string BinaryPath{};
  Addr PreferredAddr;
  int64_t RebaseDelta{0};
  gtirb::FileFormat FileFormat{};
  gtirb::ISAID IsaID{};
  std::string Name{};
  CodeBlock* EntryPoint{nullptr};
  CFG Cfg;
  ProxyBlockSet ProxyBlocks;
  SectionSet Sections;
  SectionIntMap SectionAddrs;
  SymbolSet Symbols;
  SymbolIntMap SymbolAddrs;
  ByteIntervalSet ByteIntervals;
  ByteIntervalIntMap ByteIntervalAddrs;
  CodeBlockSet CodeBlocks;
  CodeBlockIntMap CodeBlockAddrs;
  DataBlockSet DataBlocks;
  DataBlockIntMap DataBlockAddrs;
  SymbolicExpressionSet SymbolicExpressions;
  SymbolicExpressionIntMap SymbolicExpressionAddrs;

  friend class Context; // Allow Context to construct new Modules.
  friend class IR;      // Allow IRs to call setIR.

  // Allow changing the module's name.
  friend void setModuleName(IR& Ir, Module& M, const std::string& X);

  // Allow these methods to update Symbols.
  friend void renameSymbol(Module& M, Symbol& S, const std::string& N);
  friend void setSymbolAddress(Module& M, Symbol& S, Addr A);
  template <typename NodeTy>
  friend std::enable_if_t<Symbol::is_supported_type<NodeTy>()>
  setReferent(Module& M, Symbol& S, NodeTy* N);
};

/// \relates Addr
/// \brief Check: does the specified Module have the specified
/// preferred address?
///
/// \param M  The Module object to test.
/// \param X  The address to test.
///
/// \return \c true if \p M has preferred address \p X, \c false
/// otherwise.
///
/// Can be used in algorithms iterating over Module objects.
inline bool hasPreferredAddr(const Module& M, Addr X) {
  return M.getPreferredAddr() == X;
}

/// \relates Module
/// \relates Symbol
/// \brief Change the name of a symbol and update the module with the new symbol
/// name.
///
/// The module is notified of the changes so that future calls to findSymbol
/// with the new name will find the symbol.
///
/// \param M  The module containing the symbol.
/// \param S  The symbol to rename.
/// \param N  The new name to assign.
inline void renameSymbol(Module& M, Symbol& S, const std::string& N) {
  auto& Index = M.Symbols.get<Module::by_pointer>();
  Index.modify(Index.find(&S), [&N, &S](Symbol*) { S.Name = N; });
}

/// \relates Module
/// \relates Symbol
/// \brief Set the referent of a symbol and update the module with the new
/// symbol address.
///
/// The module is notified of the changes so that future calls to findSymbol
/// with the new address will find the symbol.
///
/// \tparam NodeTy  A Node type of a supported referent; should be automatically
/// deduced.
///
/// \param M  The module containing the symbol.
/// \param S  The symbol to modify.
/// \param N  The node to reference.
template <typename NodeTy>
std::enable_if_t<Symbol::is_supported_type<NodeTy>()>
setReferent(Module& M, Symbol& S, NodeTy* N) {
  auto& Index = M.Symbols.get<Module::by_pointer>();
  Index.modify(Index.find(&S), [&N, &S](Symbol*) { S.Payload = N; });
}

/// \relates Module
/// \relates Symbol
/// \brief Set the address of a symbol and update the module with the new
/// address.
///
/// The module is notified of the changes so that future calls to findSymbol
/// with the old address will no longer find the symbol.
///
/// \param M  The module containing the symbol.
/// \param S  The symbol to modify.
/// \param A  The new address to assign.
inline void setSymbolAddress(Module& M, Symbol& S, Addr A) {
  auto& Index = M.Symbols.get<Module::by_pointer>();
  Index.modify(Index.find(&S), [&A, &S](Symbol*) { S.Payload = A; });
}
} // namespace gtirb

#endif // GTIRB_MODULE_H
