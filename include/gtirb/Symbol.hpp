#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/NodeRef.hpp>
#include <proto/Symbol.pb.h>

namespace gtirb {
class DataObject;
class Block;

///
/// \class Symbol
///
class GTIRB_EXPORT_API Symbol : public Node {
public:
  ///
  ///
  /// \enum StorageKind
  ///
  enum class StorageKind : uint8_t {
    Undefined = proto::Storage_Undefined,
    Normal = proto::Storage_Normal,
    Static = proto::Storage_Static,
    Extern = proto::Storage_Extern,
    Local = proto::Storage_Local
  };

  ///
  /// Default constructor.
  ///
  Symbol() = default;

  ///
  /// This constructor sets the Effective Address on construction.
  ///
  Symbol(EA x);
  Symbol(EA x, std::string name, StorageKind storageKind = StorageKind::Extern);
  Symbol(EA x, std::string name, const DataObject& referent,
         StorageKind storageKind = StorageKind::Extern);
  Symbol(EA x, std::string name, const Block& referent,
         StorageKind storageKind = StorageKind::Extern);

  ///
  /// Copy constructor. Assigns a new UUID to the copy.
  ///
  explicit Symbol(const Symbol& other) = default;

  ///
  /// Move constructor
  ///
  Symbol(Symbol&&) = default;

  ///
  /// Move assignment
  ///
  Symbol& operator=(Symbol&&) = default;

  ///
  /// Defaulted trivial destructor.
  ///
  ~Symbol() override = default;

  void setEA(gtirb::EA x);
  gtirb::EA getEA() const;

  void setName(std::string x);
  std::string getName() const;

  ///
  /// Set the DataObject to which this symbol refers.
  ///
  void setReferent(const DataObject& data);

  ///
  /// Set the Block to which this symbol refers.
  ///
  void setReferent(const Block& instruction);

  ///
  /// Get the DataObject to which this symbol refers.
  ///
  NodeRef<DataObject> getDataReferent() const;

  ///
  /// Get the Block object to which this symbol refers.
  ///
  NodeRef<Block> getCodeReferent() const;

  void setStorageKind(Symbol::StorageKind x);
  gtirb::Symbol::StorageKind getStorageKind() const;

  using MessageType = proto::Symbol;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);

private:
  gtirb::EA ea{};
  std::string name;
  gtirb::Symbol::StorageKind storageKind{StorageKind::Extern};
  NodeRef<DataObject> dataReferent;
  NodeRef<Block> codeReferent;
};
} // namespace gtirb
