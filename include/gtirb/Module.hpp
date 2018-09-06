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
#include <boost/iterator/indirect_iterator.hpp>
#include <boost/iterator/iterator_traits.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/range/iterator_range.hpp>
#include <proto/Module.pb.h>
#include <cstdint>
#include <string>

namespace gtirb {
class ImageByteMap;

class DataObject;
/// \brief DOCFIXME
///
using DataSet = std::vector<DataObject *>;

class Section;
/// \brief DOCFIXME
///
using SectionSet = std::vector<Section *>;


/// \enum FileFormat
///
/// \brief DOCFIXME
///
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
///
enum class ISAID : uint8_t {
  Undefined = proto::ISA_Undefined, ///< Default value to indicates an
                                    ///< uninitialized state.
  IA32 = proto::IA32,   ///< Intel Architecture, 32-bit. Also known as i386.
  PPC32 = proto::PPC32, ///< Performance Optimization With Enhanced RISC –
                        ///< Performance Computing, 32-bit.
  X64 =
      proto::X64, ///< The generic name for the 64-bit extensions to both Intel's
                  ///< and AMD's 32-bit x86 instruction set architecture (ISA).
  ARM = proto::ARM, ///< Advanced RISC Machine. also known as Acorn RISC Machine.
  ValidButUnsupported = proto::ValidButUnsupported
};


/// \class Module
///
/// \brief DOCFIXME
///
class GTIRB_EXPORT_API Module : public Node {
  using SymbolSet = std::multimap<Addr, Symbol *>;
  using SymbolicExpressionSet = std::map<Addr, SymbolicExpression>;

  ///
  /// Default constructor.
  ///
  Module(Context &C);

  template <typename Iter>
  struct SymSetTransform {
    using ParamTy = decltype((*std::declval<Iter>()));
    using RetTy = decltype((*std::declval<ParamTy>().second));
    RetTy operator()(ParamTy V) const {
      return *V.second;
    }
  };

  template <typename Iter>
  struct SymExprSetTransform {
    using ParamTy = decltype((*std::declval<Iter>()));
    using RetTy = decltype((std::declval<ParamTy>().second));
    RetTy operator()(ParamTy V) const {
      return V.second;
    }
  };
public:

  static Module *Create(Context &C) { return new (C) Module(C); }


  /// \brief Set the location of the corresponding binary on disk.
  ///
  /// \param  X   A path to the corresponding binary on disk. DOCFIXME[any path requirements?]
  ///
  void setBinaryPath(const std::string& X) { BinaryPath = X; }


  /// \brief Get the location of the corresponding binary on disk.
  ///
  /// \return   The path to the corresponding binary on disk. DOCFIXME[any path properties to note?]
  ///
  const std::string& getBinaryPath() const { return BinaryPath; }


  /// \brief Set the format of the binary pointed to by getBinaryPath().
  ///
  /// \param X   The format of the binary associated with \c this, as a
  ///            gtirb::FileFormat enumerator.
  ///
  void setFileFormat(gtirb::FileFormat X) { this->FileFormat = X; }


  /// \brief Get the format of the binary pointed to by getBinaryPath().
  ///
  /// \return   The format of the binary associated with \c this, as a
  ///           gtirb::FileFormat enumerator.
  ///
  gtirb::FileFormat getFileFormat() const { return this->FileFormat; }


  /// \brief DOCFIXME
  ///
  /// \param X DOCFIXME
  ///
  /// \return void
  ///
  void setRebaseDelta(int64_t X) { RebaseDelta = X; }


  /// \brief DOCFIXME
  ///
  /// \return DOCFIXME
  ///
  int64_t getRebaseDelta() const { return RebaseDelta; }


  /// DOCFIXME[check all]
  ///
  /// \brief Set the Preferred Effective Address.
  ///
  /// \param X The effective address to use.
  ///
  /// \return void
  ///
  void setPreferredAddr(gtirb::Addr X) { PreferredAddr = X; }


  /// DOCFIXME[check all]
  /// \brief Set the ISA ID. DOCFIXME[needs more detail]
  ///
  /// \param X The ISA ID to set.
  ///
  void setISAID(gtirb::ISAID X) { IsaID = X; }


  /// DOCFIXME[check all]
  /// \brief Get the ISA ID. DOCFIXME[needs more detail]
  ///
  /// \return The ISA ID.
  ///
  gtirb::ISAID getISAID() const { return IsaID; }


  /// DOCFIXME[check all]
  ///
  /// \brief Get the Preferred Effective Address.
  ///
  /// \return The Preferred Effective Address.
  ///
  gtirb::Addr getPreferredAddr() const { return PreferredAddr; }


  ///
  /// A Module can have exactly one ImageByteMap child.
  ///
  gtirb::ImageByteMap& getImageByteMap();


  /// DOCFIXME[check all]
  /// \brief Get the associated ImageByteMap. DOCFIXME[difference to previous]   
  ///
  /// \return The ImageByteMap.
  ///
  /// A Module can have exactly one ImageByteMap child.
  ///
  const gtirb::ImageByteMap& getImageByteMap() const;


  using symbol_iterator =
      boost::transform_iterator<SymSetTransform<SymbolSet::iterator>,
                                SymbolSet::iterator, Symbol&>;
  using symbol_range = boost::iterator_range<symbol_iterator>;
  using const_symbol_iterator =
      boost::transform_iterator<SymSetTransform<SymbolSet::const_iterator>,
                                SymbolSet::const_iterator, const Symbol&>;
  using const_symbol_range = boost::iterator_range<const_symbol_iterator>;

  symbol_iterator symbol_begin() {
    return symbol_iterator(Symbols.begin());
  }
  symbol_iterator symbol_end() {
    return symbol_iterator(Symbols.end());
  }
  const_symbol_iterator symbol_begin() const {
    return const_symbol_iterator(Symbols.begin());
  }
  const_symbol_iterator symbol_end() const {
    return const_symbol_iterator(Symbols.end());
  }
  symbol_range symbols() {
    return boost::make_iterator_range(symbol_begin(), symbol_end());
  }
  const_symbol_range symbols() const {
    return boost::make_iterator_range(symbol_begin(), symbol_end());
  }

  void addSymbol(Symbol* S) { addSymbol({S}); }
  void addSymbol(std::initializer_list<Symbol*> Ss) {
    for (auto* S : Ss)
      Symbols.emplace(S->getAddress(), S);
  }

  symbol_range findSymbols(Addr X) {
    std::pair<SymbolSet::iterator, SymbolSet::iterator> Found =
        Symbols.equal_range(X);
    return boost::make_iterator_range(symbol_iterator(Found.first),
                                      symbol_iterator(Found.second));
  }
  const_symbol_range findSymbols(Addr X) const {
    std::pair<SymbolSet::const_iterator, SymbolSet::const_iterator> Found =
        Symbols.equal_range(X);
    return boost::make_iterator_range(const_symbol_iterator(Found.first),
                                      const_symbol_iterator(Found.second));
  }


  /// DOCFIXME[check all]
  ///
  /// \brief Set the module name.
  ///
  /// \param X The name to use.
  ///
  /// \return void
  ///
  void setName(const std::string& X) { Name = X; }


  /// DOCFIXME[check all]
  ///
  /// \brief Get the module name.
  ///
  /// \return The name.
  ///
  const std::string& getName() const { return Name; }


  /// DOCFIXME[check all]
  ///
  /// \brief Get the associated Control Flow Graph (\ref CFG).
  ///
  /// \return The associated CFG.
  ///
  const CFG& getCFG() const { return Cfg; }


  /// DOCFIXME[check all]
  ///
  /// \brief Get the associated Control Flow Graph (\ref CFG). DOCFIXME[difference to previous]
  ///
  /// \return The associated CFG.
  ///
  CFG& getCFG() { return Cfg; }

  using data_object_iterator = boost::indirect_iterator<DataSet::iterator>;
  using data_object_range = boost::iterator_range<data_object_iterator>;
  using const_data_object_iterator =
      boost::indirect_iterator<DataSet::const_iterator>;
  using const_data_object_range =
      boost::iterator_range<const_data_object_iterator>;

  data_object_iterator data_begin() { return Data.begin(); }
  const_data_object_iterator data_begin() const { return Data.begin(); }
  data_object_iterator data_end() { return Data.end(); }
  const_data_object_iterator data_end() const { return Data.end(); }
  data_object_range data() {
    return boost::make_iterator_range(data_begin(), data_end());
  }
  const_data_object_range data() const {
    return boost::make_iterator_range(data_begin(), data_end());
  }

  void addData(DataObject* DO) { addData({DO}); }
  void addData(std::initializer_list<DataObject*> Ds) {
    Data.insert(Data.end(), Ds);
  }

  using section_iterator = boost::indirect_iterator<SectionSet::iterator>;
  using section_range = boost::iterator_range<section_iterator>;
  using const_section_iterator =
      boost::indirect_iterator<SectionSet::const_iterator>;
  using const_section_range = boost::iterator_range<const_section_iterator>;

  section_iterator section_begin() { return Sections.begin(); }
  const_section_iterator section_begin() const { return Sections.begin(); }
  section_iterator section_end() { return Sections.end(); }
  const_section_iterator section_end() const { return Sections.end(); }
  section_range sections() {
    return boost::make_iterator_range(section_begin(), section_end());
  }
  const_section_range sections() const {
    return boost::make_iterator_range(section_begin(), section_end());
  }

  void addSection(Section* S) { addSection({S}); }
  void addSection(std::initializer_list<Section*> Ss) {
    Sections.insert(Sections.end(), Ss);
  }

  using symbol_expr_iterator = boost::transform_iterator<
      SymExprSetTransform<SymbolicExpressionSet::iterator>,
      SymbolicExpressionSet::iterator, SymbolicExpression&>;
  using symbol_expr_range = boost::iterator_range<symbol_expr_iterator>;
  using const_symbol_expr_iterator = boost::transform_iterator<
      SymExprSetTransform<SymbolicExpressionSet::const_iterator>,
      SymbolicExpressionSet::const_iterator, const SymbolicExpression&>;
  using const_symbol_expr_range =
      boost::iterator_range<const_symbol_expr_iterator>;

  symbol_expr_iterator symbol_expr_begin() {
    return symbol_expr_iterator(SymbolicOperands.begin());
  }
  symbol_expr_iterator symbol_expr_end() {
    return symbol_expr_iterator(SymbolicOperands.end());
  }
  const_symbol_expr_iterator symbol_expr_begin() const {
    return const_symbol_expr_iterator(SymbolicOperands.begin());
  }
  const_symbol_expr_iterator symbol_expr_end() const {
    return const_symbol_expr_iterator(SymbolicOperands.end());
  }
  symbol_expr_range symbol_exprs() {
    return boost::make_iterator_range(symbol_expr_begin(), symbol_expr_end());
  }
  const_symbol_expr_range symbol_exprs() const {
    return boost::make_iterator_range(symbol_expr_begin(), symbol_expr_end());
  }

  symbol_expr_iterator findSymbolicExpression(Addr X) {
    return symbol_expr_iterator(SymbolicOperands.find(X));
  }

  const_symbol_expr_iterator findSymbolicExpression(Addr X) const {
    return const_symbol_expr_iterator(SymbolicOperands.find(X));
  }

  void addSymbolicExpression(Addr X, const SymbolicExpression& SE) {
    SymbolicOperands.emplace(X, SE);
  }


  /// \brief DOCFIXME
  using MessageType = proto::Module;


  /// \brief DOCFIXME
  ///
  /// \param message DOCFIXME
  ///
  /// \return void
  ///
  void toProtobuf(MessageType* message) const;


  /// \brief DOCFIXME
  ///
  /// \param C DOCFIXME
  ///
  /// \param message DOCFIXME
  ///
  /// \return DOCFIXME
  ///
  static Module *fromProtobuf(Context &C, const MessageType& message);

  static bool classof(const Node *N) { return N->getKind() == Kind::Module; }

private:
  std::string BinaryPath{};
  Addr PreferredAddr;
  int64_t RebaseDelta{0};
  gtirb::FileFormat FileFormat{};
  gtirb::ISAID IsaID{};
  std::string Name{};
  CFG Cfg;
  DataSet Data;
  ImageByteMap *ImageBytes;
  SectionSet Sections;
  SymbolSet Symbols;
  SymbolicExpressionSet SymbolicOperands;
};

inline bool hasPreferredAddr(const Module &M, Addr X) {
  return M.getPreferredAddr() == X;
}

inline bool containsAddr(const Module &M, Addr X) {
  const std::pair<Addr, Addr> &MinMax = M.getImageByteMap().getAddrMinMax();
  return X >= MinMax.first && X < MinMax.second;
}
} // namespace gtirb

#endif // GTIRB_MODULE_H
