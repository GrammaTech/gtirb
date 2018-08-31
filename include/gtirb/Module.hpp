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

  /// \brief Set the ISA of the instructions in this Module.
  ///
  /// \param X The ISA ID to set.
  void setISAID(gtirb::ISAID X) { IsaID = X; }

  /// \brief Get the ISA of the instructions in this Module.
  ///
  /// \return The ISA ID.
  gtirb::ISAID getISAID() const { return IsaID; }

  /// \brief Get the associated ImageByteMap.
  ///
  /// \return The ImageByteMap.
  gtirb::ImageByteMap& getImageByteMap();

  /// \brief Get a const reference to the associated ImageByteMap.
  ///
  /// \return The ImageByteMap.
  ///
  /// A Module can have exactly one ImageByteMap child.
  const gtirb::ImageByteMap& getImageByteMap() const;

  /// \name Symbol-Related Public Types and Functions
  /// @{

  /// \brief Iterator over symbols (\ref Symbol).
  using symbol_iterator =
      boost::transform_iterator<SymSetTransform<SymbolSet::iterator>,
                                SymbolSet::iterator, Symbol&>;
  /// \brief Range of symbols (\ref Symbol).
  using symbol_range = boost::iterator_range<symbol_iterator>;
  /// \brief Constant iterator over symbols (\ref Symbol).
  using const_symbol_iterator =
      boost::transform_iterator<SymSetTransform<SymbolSet::const_iterator>,
                                SymbolSet::const_iterator, const Symbol&>;
  /// \brief Constant range of symbols (\ref Symbol).
  using const_symbol_range = boost::iterator_range<const_symbol_iterator>;

  /// \brief Iterator over symbols (\ref Symbol).
  using symbol_addr_iterator =
      boost::transform_iterator<SymSetTransform<SymbolAddrMap::iterator>,
                                SymbolAddrMap::iterator, Symbol&>;
  /// \brief Range of symbols (\ref Symbol).
  using symbol_addr_range = boost::iterator_range<symbol_addr_iterator>;
  /// \brief Constant iterator over symbols (\ref Symbol).
  using const_symbol_addr_iterator =
      boost::transform_iterator<SymSetTransform<SymbolAddrMap::const_iterator>,
                                SymbolAddrMap::const_iterator, const Symbol&>;
  /// \brief Constant range of symbols (\ref Symbol).
  using const_symbol_addr_range =
      boost::iterator_range<const_symbol_addr_iterator>;

  /// \brief Return an iterator to the first Symbol.
  symbol_iterator symbol_begin() { return symbol_iterator(Symbols.begin()); }
  /// \brief Return a constant iterator to the first Symbol.
  const_symbol_iterator symbol_begin() const {
    return const_symbol_iterator(Symbols.begin());
  }
  /// \brief Return an iterator to the element following the last Symbol.
  symbol_iterator symbol_end() { return symbol_iterator(Symbols.end()); }
  /// \brief Return a constant iterator to the element following the last
  /// Symbol.
  const_symbol_iterator symbol_end() const {
    return const_symbol_iterator(Symbols.end());
  }
  /// \brief Return a range of the symbols (\ref Symbol).
  symbol_range symbols() {
    return boost::make_iterator_range(symbol_begin(), symbol_end());
  }
  /// \brief Return a constant range of the symbols (\ref Symbol).
  const_symbol_range symbols() const {
    return boost::make_iterator_range(symbol_begin(), symbol_end());
  }

  /// \brief Add a single symbol to the module.
  ///
  /// \param S The Symbol object to add.
  ///
  /// \return void
  void addSymbol(Symbol* S) { addSymbol({S}); }

  /// \brief Add one or more symbols to the module.
  ///
  /// \param Ss The list of Symbol objects to add.
  ///
  /// \return void
  void addSymbol(std::initializer_list<Symbol*> Ss) {
    for (auto* S : Ss) {
      Symbols.emplace(S->getName(), S);
      if (const auto& A = S->getAddress()) {
        SymbolsByAddr.emplace(A.value(), S);
      }
    }
  }

  /// \brief Find symbols by name
  ///
  /// \param N The address to look up.
  ///
  /// \return An iterator to the found object, or \ref symbol_end() if not
  /// found.
  symbol_iterator findSymbol(const std::string& N) {
    return symbol_iterator(Symbols.find(N));
  }

  /// \brief Find symbols by name
  ///
  /// \param N The address to look up.
  ///
  /// \return An iterator to the found object, or \ref symbol_end() if not
  /// found.
  const_symbol_iterator findSymbol(const std::string& N) const {
    return const_symbol_iterator(Symbols.find(N));
  }

  /// \brief Find symbols by address.
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

  /// \brief Find symbols by address.
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
  /// @}
  // (end group of symbol-related type aliases and functions)

  /// \brief Set the module name.
  ///
  /// \param X The name to use.
  ///
  /// \return void
  void setName(const std::string& X) { Name = X; }

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

  /// \name DataObject-Related Public Types and Functions
  /// @{

  /// \brief Iterator over data objects (\ref DataObject).
  using data_object_iterator =
      boost::transform_iterator<SymSetTransform<DataSet::iterator>,
                                DataSet::iterator, DataObject&>;
  /// \brief Range of data objects (\ref DataObject).
  using data_object_range = boost::iterator_range<data_object_iterator>;
  /// \brief Constant iterator over data objects (\ref DataObject).
  using const_data_object_iterator =
      boost::transform_iterator<SymSetTransform<DataSet::const_iterator>,
                                DataSet::const_iterator, const DataObject&>;
  /// \brief Constant range of data objects (\ref DataObject).
  using const_data_object_range =
      boost::iterator_range<const_data_object_iterator>;

  /// \brief Return an iterator to the first DataObject.
  data_object_iterator data_begin() {
    return data_object_iterator(Data.begin());
  }
  /// \brief Return a constant iterator to the first DataObject.
  const_data_object_iterator data_begin() const {
    return const_data_object_iterator(Data.begin());
  }
  /// \brief Return an iterator to the element following the last DataObject.
  data_object_iterator data_end() { return data_object_iterator(Data.end()); }
  /// \brief Return a constant iterator to the element following the last
  /// DataObject.
  const_data_object_iterator data_end() const {
    return const_data_object_iterator(Data.end());
  }
  /// \brief Return a range of the data objects (\ref DataObject).
  data_object_range data() {
    return boost::make_iterator_range(data_begin(), data_end());
  }
  /// \brief Return a constant range of the data objects (\ref DataObject).
  const_data_object_range data() const {
    return boost::make_iterator_range(data_begin(), data_end());
  }

  /// \brief Add a single data object to the module.
  ///
  /// \param DO The DataObject object to add.
  ///
  /// \return void
  void addData(DataObject* DO) { addData({DO}); }

  /// \brief Add one or more data objects to the module.
  ///
  /// \param Ds The list of DataObject objects to add.
  ///
  /// \return void
  void addData(std::initializer_list<DataObject*> Ds) {
    for (auto* D : Ds)
      Data.emplace(D->getAddress(), D);
  }

  /// \brief Find a DataObject by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return An iterator to the found object, or \ref data_end() if not found.
  data_object_iterator findData(Addr X) {
    return data_object_iterator(Data.find(X));
  }

  /// \brief Find a DataObject by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return An iterator to the found object, or \ref data_end() if not found.
  const_data_object_iterator findData(Addr X) const {
    return const_data_object_iterator(Data.find(X));
  }
  /// @}
  // (end group of DataObject-related types and functions)

  /// \name Section-Related Public Types and Functions
  /// @{

  /// \brief Iterator over sections (\ref Section).
  using section_iterator =
      boost::transform_iterator<SymSetTransform<SectionSet::iterator>,
                                SectionSet::iterator, Section&>;
  /// \brief Range of sections (\ref Section).
  using section_range = boost::iterator_range<section_iterator>;
  /// \brief Constant iterator over sections (\ref Section).
  using const_section_iterator =
      boost::transform_iterator<SymSetTransform<SectionSet::const_iterator>,
                                SectionSet::const_iterator, const Section&>;
  /// \brief Constant range of sections (\ref Section).
  using const_section_range = boost::iterator_range<const_section_iterator>;

  /// \brief Return an iterator to the first Section.
  section_iterator section_begin() {
    return section_iterator(Sections.begin());
  }
  /// \brief Return a constant iterator to the first Section.
  const_section_iterator section_begin() const {
    return const_section_iterator(Sections.begin());
  }
  /// \brief Return an iterator to the element following the last Section.
  section_iterator section_end() { return section_iterator(Sections.end()); }
  /// \brief Return a constant iterator to the element following the last
  /// Section.
  const_section_iterator section_end() const {
    return const_section_iterator(Sections.end());
  }
  /// \brief Return a range of the sections (\ref Section).
  section_range sections() {
    return boost::make_iterator_range(section_begin(), section_end());
  }
  /// \brief Return a constant range of the sections (\ref Section).
  const_section_range sections() const {
    return boost::make_iterator_range(section_begin(), section_end());
  }

  /// \brief Add a single Section object to the module.
  ///
  /// \param S The Section object to add.
  ///
  /// \return void
  void addSection(Section* S) { addSection({S}); }

  /// \brief Add one or more section objects to the module.
  ///
  /// \param Ss The list of Section objects to add.
  ///
  /// \return void
  void addSection(std::initializer_list<Section*> Ss) {
    for (auto* S : Ss)
      Sections.emplace(S->getAddress(), S);
  }

  /// \brief Find a Section by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return An iterator to the found object, or \ref section_end() if not
  /// found.
  section_iterator findSection(Addr X) {
    return section_iterator(Sections.find(X));
  }

  /// \brief Find a Section by address.
  ///
  /// \param X The address to look up.
  ///
  /// \return An iterator to the found object, or \ref section_end() if not
  /// found.
  const_section_iterator findSection(Addr X) const {
    return const_section_iterator(Sections.find(X));
  }
  /// @}
  // (end group of Section-related types and functions)

  /// \name SymbolicExpression-Related Public Types and Functions
  /// @{

  /// \brief Iterator over symbolic expressions (\ref SymbolicExpression).
  using symbolic_expr_iterator = boost::transform_iterator<
      SymExprSetTransform<SymbolicExpressionSet::iterator>,
      SymbolicExpressionSet::iterator, SymbolicExpression&>;
  /// \brief Range of symbolic expressions (\ref SymbolicExpression).
  using symbolic_expr_range = boost::iterator_range<symbolic_expr_iterator>;
  /// \brief Constant iterator over symbolic expressions
  /// (\ref SymbolicExpression).
  using const_symbolic_expr_iterator = boost::transform_iterator<
      SymExprSetTransform<SymbolicExpressionSet::const_iterator>,
      SymbolicExpressionSet::const_iterator, const SymbolicExpression&>;
  /// \brief Constant range of symbolic expressions (\ref SymbolicExpression).
  using const_symbolic_expr_range =
      boost::iterator_range<const_symbolic_expr_iterator>;

  /// \brief Return an iterator to the first \ref SymbolicExpression.
  symbolic_expr_iterator symbolic_expr_begin() {
    return symbolic_expr_iterator(SymbolicOperands.begin());
  }
  /// \brief Return a constant iterator to the first \ref SymbolicExpression.
  const_symbolic_expr_iterator symbolic_expr_begin() const {
    return const_symbolic_expr_iterator(SymbolicOperands.begin());
  }
  /// \brief Return an iterator to the element following the last
  /// \ref SymbolicExpression.
  symbolic_expr_iterator symbolic_expr_end() {
    return symbolic_expr_iterator(SymbolicOperands.end());
  }
  /// \brief Return a constant iterator to the element following the last
  /// \ref SymbolicExpression.
  const_symbolic_expr_iterator symbolic_expr_end() const {
    return const_symbolic_expr_iterator(SymbolicOperands.end());
  }
  /// \brief Return a range of the symbolic expressions
  /// (\ref SymbolicExpression).
  symbolic_expr_range symbolic_exprs() {
    return boost::make_iterator_range(symbolic_expr_begin(),
                                      symbolic_expr_end());
  }
  /// \brief Return a constant range of the symbolic expressions
  /// (\ref SymbolicExpression).
  const_symbolic_expr_range symbolic_exprs() const {
    return boost::make_iterator_range(symbolic_expr_begin(),
                                      symbolic_expr_end());
  }

  /// \brief Find symbolic expressions (\ref SymbolicExpression) by
  /// address.
  ///
  /// \param X The address to look up.
  ///
  /// \return an iterator representing the first symbolic expression found. The
  /// end of the iterator range can be obtained by calling symbolic_expr_end().
  symbolic_expr_iterator findSymbolicExpression(Addr X) {
    return symbolic_expr_iterator(SymbolicOperands.find(X));
  }

  /// \brief Find symbolic expressions (\ref SymbolicExpression) by
  /// address.
  ///
  /// \param X The address to look up.
  ///
  /// \return a constant iterator representing the first symbolic expression
  /// found. The end of the iterator range can be obtained by calling
  /// symbolic_expr_end().
  const_symbolic_expr_iterator findSymbolicExpression(Addr X) const {
    return const_symbolic_expr_iterator(SymbolicOperands.find(X));
  }

  /// \brief Add a symbolic expression (\ref SymbolicExpression) to
  /// the module.
  ///
  /// \param X  The address of the symbolic expression.
  /// \param SE The SymbolicExpression object to add.
  ///
  /// \return void
  void addSymbolicExpression(Addr X, const SymbolicExpression& SE) {
    SymbolicOperands.emplace(X, SE);
  }
  /// @}
  // (end group of SymbolicExpression-related type aliases and methods)

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
