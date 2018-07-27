#pragma once

#include <boost/filesystem.hpp>
#include <gtirb/CFG.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Enums.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/NodeReference.hpp>
#include <gtirb/SymbolSet.hpp>
#include <gtirb/SymbolicOperandSet.hpp>

namespace proto {
class Module;
}

namespace gtirb {
class AddrRanges;
class Data;
class ImageByteMap;
class Data;
using DataSet = std::vector<Data>;
struct Relocation;
using RelocationSet = std::vector<Relocation>;
class Section;
using SectionSet = std::vector<Section>;

///
/// \class Module
///
/// \todo Replace boost::filesystem with std::filesystem.
///
class GTIRB_EXPORT_API Module : public Node {
public:
  ///
  /// Default constructor.
  ///
  Module();

  ///
  /// Modules can not be copied due to unique_ptrs.
  ///
  Module(const Module&) = delete;

  ///
  /// Move constructor
  ///
  Module(Module&&);

  ///
  /// Move assignment
  ///
  Module& operator=(Module&&) = default;

  ///
  /// Trivial virtual destructor.
  ///
  ~Module() override;

  ///
  /// Set the location of the corresponding binary on disk.
  ///
  /// \param  x   A path to the corresponding binary on disk.
  ///
  void setBinaryPath(boost::filesystem::path x);

  ///
  /// Get the location of the corresponding binary on disk.
  ///
  /// \return   The path to the corresponding binary on disk.
  ///
  boost::filesystem::path getBinaryPath() const;

  ///
  /// Sets the format of the binary pointed to by getBinaryPath().
  ///
  /// \param  x   The gtirb::FileFormat enumeration corresponding to the binary associated
  /// with this Module.
  ///
  void setFileFormat(gtirb::FileFormat x);

  ///
  /// Gets the format of the binary pointed to by getBinaryPath().
  ///
  /// \return     The gtirb::FileFormat enumeration corresponding to the binary associated
  /// with this Module.
  ///
  gtirb::FileFormat getFileFormat() const;

  ///
  ///
  ///
  void setRebaseDelta(int64_t x);

  ///
  ///
  ///
  int64_t getRebaseDelta() const;

  ///
  ///
  ///
  void setPreferredEA(gtirb::EA x);

  ///
  ///
  ///
  void setISAID(gtirb::ISAID x);

  ///
  ///
  ///
  gtirb::ISAID getISAID() const;

  ///
  ///
  ///
  gtirb::EA getPreferredEA() const;

  ///
  /// A Module can have exactly one AddrRanges child.
  ///
  gtirb::AddrRanges& getAddrRanges();
  const gtirb::AddrRanges& getAddrRanges() const;

  ///
  /// A Module can have exactly one ImageByteMap child.
  ///
  gtirb::ImageByteMap& getImageByteMap();
  const gtirb::ImageByteMap& getImageByteMap() const;

  gtirb::SymbolSet& getSymbols();
  const gtirb::SymbolSet& getSymbols() const;

  void setName(std::string x);
  std::string getName() const;

  void setDecodeMode(uint64_t x);
  uint64_t getDecodeMode() const;

  const CFG& getCFG() const;
  CFG& getCFG();
  const std::vector<Relocation>& getRelocations() const;
  std::vector<Relocation>& getRelocations();
  const std::vector<Data>& getData() const;
  std::vector<Data>& getData();
  std::vector<Section>& getSections();
  const std::vector<Section>& getSections() const;
  SymbolicOperandSet& getSymbolicOperands();
  const SymbolicOperandSet& getSymbolicOperands() const;

  using MessageType = proto::Module;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);

private:
  boost::filesystem::path binaryPath{};
  gtirb::EA preferredEA{};
  int64_t rebaseDelta{0};
  gtirb::FileFormat fileFormat{};
  gtirb::ISAID isaID{};
  std::string name{};
  uint64_t decodeMode{0};
  std::unique_ptr<AddrRanges> addrRanges;
  std::unique_ptr<CFG> cfg;
  std::unique_ptr<DataSet> data;
  std::unique_ptr<ImageByteMap> imageByteMap;
  std::unique_ptr<RelocationSet> relocations;
  std::unique_ptr<SectionSet> sections;
  std::unique_ptr<SymbolSet> symbols;
  std::unique_ptr<SymbolicOperandSet> symbolicOperands;
};
} // namespace gtirb
