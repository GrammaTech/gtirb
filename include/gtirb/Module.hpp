#pragma once

#include <gtirb/CFG.hpp>
#include <gtirb/EA.hpp>
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
using DataSet = std::vector<DataObject *>;
class Section;
using SectionSet = std::vector<Section *>;

///
/// \enum FileFormat
///
enum class FileFormat : uint8_t {
  Undefined = proto::Format_Undefined, /// Default value to indicates an
                                       /// uninitialized state.
  COFF = proto::COFF,                  /// Common Object File Format (COFF)
  ELF = proto::ELF, /// Executable and Linkable Format (ELF, formerly named
                    /// Extensible Linking Format)
  PE = proto::PE,   /// Microsoft Portable Executable (PE) format.
  IdaProDb32 = proto::IdaProDb32, /// IDA Pro database file
  IdaProDb64 = proto::IdaProDb64, /// IDA Pro database file
  XCOFF = proto::XCOFF, /// Non-COFF (files start with ANON_OBJECT_HEADER*)
  MACHO = proto::MACHO  /// Mach object file format
};

///
/// \enum ISAID
///
/// ISA ID
///
enum class ISAID : uint8_t {
  Undefined = proto::ISA_Undefined, /// Default value to indicates an
                                    /// uninitialized state.
  IA32 = proto::IA32,   /// Intel Architecture, 32-bit. Also known as i386.
  PPC32 = proto::PPC32, /// Performance Optimization With Enhanced RISC –
                        /// Performance Computing, 32-bit.
  X64 =
      proto::X64, /// The generic name for the 64-bit extensions to both Intel's
                  /// and AMD's 32-bit x86 instruction set architecture (ISA).
  ARM = proto::ARM, /// Advanced RISC Machine. also known as Acorn RISC Machine.
  ValidButUnsupported = proto::ValidButUnsupported
};

///
/// \class Module
///
class GTIRB_EXPORT_API Module : public Node {
  ///
  /// Default constructor.
  ///
  Module(Context &C);

public:

  static Module *Create(Context &C) { return new (C) Module(C); }

  ///
  /// Set the location of the corresponding binary on disk.
  ///
  /// \param  X   A path to the corresponding binary on disk.
  ///
  void setBinaryPath(std::string X);

  ///
  /// Get the location of the corresponding binary on disk.
  ///
  /// \return   The path to the corresponding binary on disk.
  ///
  std::string getBinaryPath() const;

  ///
  /// Sets the format of the binary pointed to by getBinaryPath().
  ///
  /// \param  X   The gtirb::FileFormat enumeration corresponding to the binary
  /// associated with this Module.
  ///
  void setFileFormat(gtirb::FileFormat X);

  ///
  /// Gets the format of the binary pointed to by getBinaryPath().
  ///
  /// \return     The gtirb::FileFormat enumeration corresponding to the binary
  /// associated with this Module.
  ///
  gtirb::FileFormat getFileFormat() const;

  ///
  ///
  ///
  void setRebaseDelta(int64_t X);

  ///
  ///
  ///
  int64_t getRebaseDelta() const;

  ///
  ///
  ///
  void setPreferredEA(gtirb::EA X);

  ///
  ///
  ///
  void setISAID(gtirb::ISAID X);

  ///
  ///
  ///
  gtirb::ISAID getISAID() const;

  ///
  ///
  ///
  gtirb::EA getPreferredEA() const;

  ///
  /// A Module can have exactly one ImageByteMap child.
  ///
  gtirb::ImageByteMap& getImageByteMap();
  const gtirb::ImageByteMap& getImageByteMap() const;

  gtirb::SymbolSet& getSymbols();
  const gtirb::SymbolSet& getSymbols() const;

  void setName(std::string X);
  std::string getName() const;

  const CFG& getCFG() const;
  CFG& getCFG();
  const DataSet& getData() const;
  DataSet& getData();
  SectionSet& getSections();
  const SectionSet& getSections() const;
  SymbolicExpressionSet& getSymbolicExpressions();
  const SymbolicExpressionSet& getSymbolicExpressions() const;

  using MessageType = proto::Module;
  void toProtobuf(MessageType* message) const;
  static Module *fromProtobuf(Context &C, const MessageType& message);

private:
  std::string BinaryPath{};
  gtirb::EA PreferredEA{};
  int64_t RebaseDelta{0};
  gtirb::FileFormat FileFormat{};
  gtirb::ISAID IsaID{};
  std::string Name{};
  std::unique_ptr<CFG> Cfg;
  std::unique_ptr<DataSet> Data;
  ImageByteMap *ImageBytes;
  std::unique_ptr<SectionSet> Sections;
  std::unique_ptr<SymbolSet> Symbols;
  std::unique_ptr<SymbolicExpressionSet> SymbolicOperands;
};
} // namespace gtirb
