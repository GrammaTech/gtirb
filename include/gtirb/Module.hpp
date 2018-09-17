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
#include <gtirb/CFG.hpp>
#include <gtirb/DataObject.hpp>
#include <gtirb/Export.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <proto/Module.pb.h>
#include <boost/iterator/indirect_iterator.hpp>
#include <boost/iterator/iterator_traits.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/range/iterator_range.hpp>
#include <cstdint>
#include <string>

/// \file Module.hpp
/// \brief Class gtirb::Module and related functions and types.

namespace gtirb {
/// \enum FileFormat
///
/// \brief DOCFIXME
enum class FileFormat : uint8_t {
  Undefined = proto::Format_Undefined, ///< Default value to indicates an
                                       ///< uninitialized state.
  COFF = proto::COFF,                  ///< Common Object File Format (COFF)
  ELF = proto::ELF, ///< Executable and Linkable Format (ELF, formerly named
                    ///< Extensible Linking Format)
  PE = proto::PE,   ///< Microsoft Portable Executable (PE) format.
  IdaProDb32 = proto::IdaProDb32, ///< IDA Pro database file
  IdaProDb64 = proto::IdaProDb64, ///< IDA Pro database file
  XCOFF = proto::XCOFF, ///< Non-COFF (files start with ANON_OBJECT_HEADER*)
  MACHO = proto::MACHO  ///< Mach object file format
};

/// \enum ISAID
///
/// \brief ISA ID DOCFIXME[need more info?]
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
/// \brief DOCFIXME
class GTIRB_EXPORT_API Module : public Node {
  using SymbolSet = std::map<std::string, Symbol*>;
  using SymbolAddrMap = std::multimap<Addr, Symbol*>;
  using SymbolicExpressionSet = std::map<Addr, SymbolicExpression>;
  using DataSet = std::map<Addr, DataObject*>;
  using SectionSet = std::map<Addr, Section*>;

  Module(Context& C);

  template <typename Iter> struct SymSetTransform {
    using ParamTy = decltype((*std::declval<Iter>()));
    using RetTy = decltype((*std::declval<ParamTy>().second));
    RetTy operator()(ParamTy V) const { return *V.second; }
  };

  template <typename Iter> struct SymExprSetTransform {
    using ParamTy = decltype((*std::declval<Iter>()));
    using RetTy = decltype((std::declval<ParamTy>().second));
    RetTy operator()(ParamTy V) const { return V.second; }
  };

public:
  /// \brief Create a Module object in its default state.
  ///
  /// \param C  The Context in which this object will be held.
  ///
  /// \return The newly created object.
  static Module* Create(Context& C) { return new (C) Module(C); }

  /// \brief Set the location of the corresponding binary on disk.
  ///
  /// \param  X   A path to the corresponding binary on disk. DOCFIXME[any path
  /// requirements?]
  void setBinaryPath(const std::string& X) { BinaryPath = X; }

  /// \brief Get the location of the corresponding binary on disk.
  ///
  /// \return   The path to the corresponding binary on disk. DOCFIXME[any path
  /// properties to note?]
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

  /// \brief DOCFIXME
  ///
  /// \param X DOCFIXME
  ///
  /// \return void
  void setRebaseDelta(int64_t X) { RebaseDelta = X; }

  /// \brief DOCFIXME
  ///
  /// \return DOCFIXME
  int64_t getRebaseDelta() const { return RebaseDelta; }

  /// DOCFIXME[check all]
  ///
  /// \brief Set the Preferred Effective Address.
  ///
  /// \param X The effective address to use.
  ///
  /// \return void
  void setPreferredAddr(gtirb::Addr X) { PreferredAddr = X; }

  /// DOCFIXME[check all]
  /// \brief Set the ISA ID. DOCFIXME[needs more detail]
  ///
  /// \param X The ISA ID to set.
  void setISAID(gtirb::ISAID X) { IsaID = X; }

  /// DOCFIXME[check all]
  /// \brief Get the ISA ID. DOCFIXME[needs more detail]
  ///
  /// \return The ISA ID.
  gtirb::ISAID getISAID() const { return IsaID; }

  /// DOCFIXME[check all]
  ///
  /// \brief Get the Preferred Effective Address.
  ///
  /// \return The Preferred Effective Address.
  gtirb::Addr getPreferredAddr() const { return PreferredAddr; }

  /// DOCFIXME[check all]
  /// \brief Get the associated ImageByteMap.
  ///
  /// \return The ImageByteMap.
  ///
  /// A Module can have exactly one ImageByteMap child.
  gtirb::ImageByteMap& getImageByteMap();

  /// DOCFIXME[check all]
  /// \brief Get the associated ImageByteMap. DOCFIXME[difference to previous]
  ///
  /// \return The ImageByteMap.
  ///
  /// A Module can have exactly one ImageByteMap child.
  const gtirb::ImageByteMap& getImageByteMap() const;

  /// \brief DOCFIXME
  using symbol_iterator =
      boost::transform_iterator<SymSetTransform<SymbolSet::iterator>,
                                SymbolSet::iterator, Symbol&>;
  /// \brief DOCFIXME
  using symbol_range = boost::iterator_range<symbol_iterator>;
  /// \brief DOCFIXME
  using const_symbol_iterator =
      boost::transform_iterator<SymSetTransform<SymbolSet::const_iterator>,
                                SymbolSet::const_iterator, const Symbol&>;
  /// \brief DOCFIXME
  using const_symbol_range = boost::iterator_range<const_symbol_iterator>;

  /// \brief DOCFIXME
  using symbol_addr_iterator =
      boost::transform_iterator<SymSetTransform<SymbolAddrMap::iterator>,
                                SymbolAddrMap::iterator, Symbol&>;

  /// \brief DOCFIXME
  using symbol_addr_range = boost::iterator_range<symbol_addr_iterator>;

  /// \brief DOCFIXME
  using const_symbol_addr_iterator =
      boost::transform_iterator<SymSetTransform<SymbolAddrMap::const_iterator>,
                                SymbolAddrMap::const_iterator, const Symbol&>;

  /// \brief DOCFIXME
  using const_symbol_addr_range =
      boost::iterator_range<const_symbol_addr_iterator>;

  /// \brief DOCFIXME
  symbol_iterator symbol_begin() { return symbol_iterator(Symbols.begin()); }
  /// \brief DOCFIXME
  symbol_iterator symbol_end() { return symbol_iterator(Symbols.end()); }
  /// \brief DOCFIXME
  const_symbol_iterator symbol_begin() const {
    return const_symbol_iterator(Symbols.begin());
  }
  /// \brief DOCFIXME
  const_symbol_iterator symbol_end() const {
    return const_symbol_iterator(Symbols.end());
  }
  /// \brief DOCFIXME
  symbol_range symbols() {
    return boost::make_iterator_range(symbol_begin(), symbol_end());
  }
  /// \brief DOCFIXME
  const_symbol_range symbols() const {
    return boost::make_iterator_range(symbol_begin(), symbol_end());
  }

  /// \brief Adds a single symbol to the module.
  ///
  /// \param S The Symbol object to add.
  ///
  /// \return void
  void addSymbol(Symbol* S) { addSymbol({S}); }

  /// \brief Adds one or more symbols to the module.
  ///
  /// \param Ss The list of Symbol objects to add.
  ///
  /// \return void
  void addSymbol(std::initializer_list<Symbol*> Ss) {
    for (auto* S : Ss) {
      Symbols.emplace(S->getName(), S);
      SymbolsByAddr.emplace(S->getAddress(), S);
    }
  }

  /// \brief Finds symbols by name
  ///
  /// \param X The address to look up.
  ///
  /// \return An iterator to the found object, or \ref symbol_end() if not
  /// found.
  symbol_iterator findSymbol(const std::string& N) {
    return symbol_iterator(Symbols.find(N));
  }

  /// \brief Finds symbols by name
  ///
  /// \param X The address to look up.
  ///
  /// \return An iterator to the found object, or \ref symbol_end() if not
  /// found.
  const_symbol_iterator findSymbol(const std::string& N) const {
    return const_symbol_iterator(Symbols.find(N));
  }

  /// \brief Finds symbols by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return A possibly empty range of all the symbols containing the given
  /// address.
  symbol_addr_range findSymbols(Addr X) {
    auto Found = SymbolsByAddr.equal_range(X);
    return boost::make_iterator_range(symbol_addr_iterator(Found.first),
                                      symbol_addr_iterator(Found.second));
  }

  /// \brief Finds symbols by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return A possibly empty constant range of all the symbols containing the
  /// given address.
  const_symbol_addr_range findSymbols(Addr X) const {
    auto Found = SymbolsByAddr.equal_range(X);
    return boost::make_iterator_range(const_symbol_addr_iterator(Found.first),
                                      const_symbol_addr_iterator(Found.second));
  }

  /// DOCFIXME[check all]
  ///
  /// \brief Set the module name.
  ///
  /// \param X The name to use.
  ///
  /// \return void
  void setName(const std::string& X) { Name = X; }

  /// DOCFIXME[check all]
  ///
  /// \brief Get the module name.
  ///
  /// \return The name.
  const std::string& getName() const { return Name; }

  /// DOCFIXME[check all]
  ///
  /// \brief Get the associated Control Flow Graph (\ref CFG).
  ///
  /// \return The associated CFG.
  const CFG& getCFG() const { return Cfg; }

  /// DOCFIXME[check all]
  ///
  /// \brief Get the associated Control Flow Graph (\ref CFG).
  /// DOCFIXME[difference to previous]
  ///
  /// \return The associated CFG.
  CFG& getCFG() { return Cfg; }

  /// \brief DOCFIXME
  using data_object_iterator =
      boost::transform_iterator<SymSetTransform<DataSet::iterator>,
                                DataSet::iterator, DataObject&>;
  /// \brief DOCFIXME
  using data_object_range = boost::iterator_range<data_object_iterator>;
  /// \brief DOCFIXME
  using const_data_object_iterator =
      boost::transform_iterator<SymSetTransform<DataSet::const_iterator>,
                                DataSet::const_iterator, const DataObject&>;
  /// \brief DOCFIXME
  using const_data_object_range =
      boost::iterator_range<const_data_object_iterator>;

  /// \brief DOCFIXME
  data_object_iterator data_begin() {
    return data_object_iterator(Data.begin());
  }
  /// \brief DOCFIXME
  const_data_object_iterator data_begin() const {
    return const_data_object_iterator(Data.begin());
  }
  /// \brief DOCFIXME
  data_object_iterator data_end() { return data_object_iterator(Data.end()); }
  /// \brief DOCFIXME
  const_data_object_iterator data_end() const {
    return const_data_object_iterator(Data.end());
  }
  /// \brief DOCFIXME
  data_object_range data() {
    return boost::make_iterator_range(data_begin(), data_end());
  }
  /// \brief DOCFIXME
  const_data_object_range data() const {
    return boost::make_iterator_range(data_begin(), data_end());
  }

  /// \brief Adds a single data object to the module.
  ///
  /// \param DO The DataObject object to add.
  ///
  /// \return void
  void addData(DataObject* DO) { addData({DO}); }

  /// \brief Adds one or more data objects to the module.
  ///
  /// \param Ds The list of DataObject objects to add.
  ///
  /// \return void
  void addData(std::initializer_list<DataObject*> Ds) {
    for (auto* D : Ds)
      Data.emplace(D->getAddress(), D);
  }

  /// \brief Finds DataObjects by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return An iterator to the found object, or \ref data_end() if not found.
  data_object_iterator findData(Addr X) {
    return data_object_iterator(Data.find(X));
  }

  /// \brief Finds DataObjects by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return An iterator to the found object, or \ref data_end() if not found.
  const_data_object_iterator findData(Addr X) const {
    return const_data_object_iterator(Data.find(X));
  }

  /// \brief DOCFIXME
  using section_iterator =
      boost::transform_iterator<SymSetTransform<SectionSet::iterator>,
                                SectionSet::iterator, Section&>;
  /// \brief DOCFIXME
  using section_range = boost::iterator_range<section_iterator>;
  /// \brief DOCFIXME
  using const_section_iterator =
      boost::transform_iterator<SymSetTransform<SectionSet::const_iterator>,
                                SectionSet::const_iterator, const Section&>;
  /// \brief DOCFIXME
  using const_section_range = boost::iterator_range<const_section_iterator>;

  /// \brief DOCFIXME
  section_iterator section_begin() {
    return section_iterator(Sections.begin());
  }
  /// \brief DOCFIXME
  const_section_iterator section_begin() const {
    return const_section_iterator(Sections.begin());
  }
  /// \brief DOCFIXME
  section_iterator section_end() { return section_iterator(Sections.end()); }
  /// \brief DOCFIXME
  const_section_iterator section_end() const {
    return const_section_iterator(Sections.end());
  }
  /// \brief DOCFIXME
  section_range sections() {
    return boost::make_iterator_range(section_begin(), section_end());
  }
  /// \brief DOCFIXME
  const_section_range sections() const {
    return boost::make_iterator_range(section_begin(), section_end());
  }

  /// \brief Adds a single section object to the module.
  ///
  /// \param S The Section object to add.
  ///
  /// \return void
  void addSection(Section* S) { addSection({S}); }

  /// \brief Adds one or more section objects to the module.
  ///
  /// \param Ss The list of Section objects to add.
  ///
  /// \return void
  void addSection(std::initializer_list<Section*> Ss) {
    for (auto* S : Ss)
      Sections.emplace(S->getAddress(), S);
  }

  /// \brief Finds Sections by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return An iterator to the found object, or \ref section_end() if not
  /// found.
  section_iterator findSection(Addr X) {
    return section_iterator(Sections.find(X));
  }

  /// \brief Finds Sections by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return An iterator to the found object, or \ref section_end() if not
  /// found.
  const_section_iterator findSection(Addr X) const {
    return const_section_iterator(Sections.find(X));
  }

  /// \brief DOCFIXME
  using symbolic_expr_iterator = boost::transform_iterator<
      SymExprSetTransform<SymbolicExpressionSet::iterator>,
      SymbolicExpressionSet::iterator, SymbolicExpression&>;
  /// \brief DOCFIXME
  using symbolic_expr_range = boost::iterator_range<symbolic_expr_iterator>;
  /// \brief DOCFIXME
  using const_symbolic_expr_iterator = boost::transform_iterator<
      SymExprSetTransform<SymbolicExpressionSet::const_iterator>,
      SymbolicExpressionSet::const_iterator, const SymbolicExpression&>;
  /// \brief DOCFIXME
  using const_symbolic_expr_range =
      boost::iterator_range<const_symbolic_expr_iterator>;

  /// \brief DOCFIXME
  symbolic_expr_iterator symbolic_expr_begin() {
    return symbolic_expr_iterator(SymbolicOperands.begin());
  }
  /// \brief DOCFIXME
  symbolic_expr_iterator symbolic_expr_end() {
    return symbolic_expr_iterator(SymbolicOperands.end());
  }
  /// \brief DOCFIXME
  const_symbolic_expr_iterator symbolic_expr_begin() const {
    return const_symbolic_expr_iterator(SymbolicOperands.begin());
  }
  /// \brief DOCFIXME
  const_symbolic_expr_iterator symbolic_expr_end() const {
    return const_symbolic_expr_iterator(SymbolicOperands.end());
  }
  /// \brief DOCFIXME
  symbolic_expr_range symbolic_exprs() {
    return boost::make_iterator_range(symbolic_expr_begin(),
                                      symbolic_expr_end());
  }
  /// \brief DOCFIXME
  const_symbolic_expr_range symbolic_exprs() const {
    return boost::make_iterator_range(symbolic_expr_begin(),
                                      symbolic_expr_end());
  }

  /// \brief Finds symbolic expressions by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return an iterator representing the first symbolic expression found. The
  /// end of the iterator range can be obtained by calling symbolic_expr_end().
  symbolic_expr_iterator findSymbolicExpression(Addr X) {
    return symbolic_expr_iterator(SymbolicOperands.find(X));
  }

  /// \brief Finds symbolic expressions by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return a constant iterator representing the first symbolic expression
  /// found. The end of the iterator range can be obtained by calling
  /// symbolic_expr_end().
  const_symbolic_expr_iterator findSymbolicExpression(Addr X) const {
    return const_symbolic_expr_iterator(SymbolicOperands.find(X));
  }

  /// \brief Adds a symbolic expression to the module.
  ///
  /// \param X  The address of the symbolic expression.
  /// \param SE The SymbolicExpression object to add.
  ///
  /// \return void
  void addSymbolicExpression(Addr X, const SymbolicExpression& SE) {
    SymbolicOperands.emplace(X, SE);
  }

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
  static Module* fromProtobuf(Context& C, const MessageType& Message);

  /// \cond INTERNAL
  static bool classof(const Node* N) { return N->getKind() == Kind::Module; }
  /// \endcond

private:
  std::string BinaryPath{};
  Addr PreferredAddr;
  int64_t RebaseDelta{0};
  gtirb::FileFormat FileFormat{};
  gtirb::ISAID IsaID{};
  std::string Name{};
  CFG Cfg;
  DataSet Data;
  ImageByteMap* ImageBytes;
  SectionSet Sections;
  SymbolSet Symbols;
  SymbolAddrMap SymbolsByAddr;
  SymbolicExpressionSet SymbolicOperands;
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

/// \relates Addr
/// \brief Check: does the specified Module contain the specified
/// address?
///
/// \param M  The Module object to test.
/// \param X  The address to test.
///
/// \return \c true if \p M contains address \p X, \c false otherwise.
///
/// Can be used in algorithms iterating over Module objects.
inline bool containsAddr(const Module& M, Addr X) {
  const std::pair<Addr, Addr>& MinMax = M.getImageByteMap().getAddrMinMax();
  return X >= MinMax.first && X < MinMax.second;
}
} // namespace gtirb

#endif // GTIRB_MODULE_H
