#ifndef GTIRB_MODULE_H
#define GTIRB_MODULE_H

#include <gtirb/Addr.hpp>
#include <gtirb/CFG.hpp>
#include <gtirb/Export.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/SymbolSet.hpp>
#include <gtirb/SymbolicExpressionSet.hpp>
#include <proto/Module.pb.h>
#include <cstdint>
#include <memory>
#include <string>

namespace gtirb {
class DataObject;
class ImageByteMap;
class DataObject;
using DataSet = std::vector<DataObject>;
class Section;
using SectionSet = std::vector<Section>;


/// \enum FileFormat
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
/// ISA ID
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
public:

  /// \brief Default constructor.
  ///
  Module();


  /// \brief Modules can not be copied due to unique_ptrs.
  ///
  Module(const Module&) = delete;


  /// \brief Move constructor.
  ///
  Module(Module&&);


  /// \brief Move assignment.
  ///
  Module& operator=(Module&&) = default;


  /// \brief Trivial virtual destructor.
  ///
  ~Module() override;


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


  /// DOCFIXME[check all]
  /// \brief Get the associated ImageByteMap.
  ///
  /// \return The ImageByteMap.
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


  /// DOCFIXME[check all]
  ///
  /// \brief Get the associated symbols (\ref Symbol).
  ///
  /// \return The associated symbols, as a \ref SymbolSet.
  ///
  gtirb::SymbolSet& getSymbols();


  /// DOCFIXME[check all]
  ///
  /// \brief Get the associated symbols (\ref Symbol). DOCFIXME[difference to previous]
  ///
  /// \return The associated symbols, as a \ref SymbolSet.
  ///
  const gtirb::SymbolSet& getSymbols() const;


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
  const CFG& getCFG() const;


  /// DOCFIXME[check all]
  ///
  /// \brief Get the associated Control Flow Graph (\ref CFG). DOCFIXME[difference to previous]
  ///
  /// \return The associated CFG.
  ///
  CFG& getCFG();


  /// \brief DOCFIXME
  ///
  /// \return DOCFIXME
  ///
  const std::vector<DataObject>& getData() const;


  /// \brief DOCFIXME
  ///
  /// \return DOCFIXME
  ///  
  std::vector<DataObject>& getData();


  /// \brief DOCFIXME
  ///
  /// \return DOCFIXME
  ///
  std::vector<Section>& getSections();


  /// \brief DOCFIXME
  ///
  /// \return DOCFIXME
  ///
  const std::vector<Section>& getSections() const;


  /// \brief DOCFIXME
  ///
  /// \return DOCFIXME
  ///
  SymbolicExpressionSet& getSymbolicExpressions();


  /// \brief DOCFIXME
  ///
  /// \return DOCFIXME
  ///
  const SymbolicExpressionSet& getSymbolicExpressions() const;


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
  /// \param message DOCFIXME
  ///
  /// \return void
  ///
  void fromProtobuf(const MessageType& message);

private:
  std::string BinaryPath{};
  Addr PreferredAddr;
  int64_t RebaseDelta{0};
  gtirb::FileFormat FileFormat{};
  gtirb::ISAID IsaID{};
  std::string Name{};
  std::unique_ptr<CFG> Cfg;
  std::unique_ptr<DataSet> Data;
  std::unique_ptr<ImageByteMap> ImageByteMap_;
  std::unique_ptr<SectionSet> Sections;
  std::unique_ptr<SymbolSet> Symbols;
  std::unique_ptr<SymbolicExpressionSet> SymbolicOperands;
};
} // namespace gtirb

#endif // GTIRB_MODULE_H
