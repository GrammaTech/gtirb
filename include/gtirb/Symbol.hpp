#ifndef GTIRB_SYMBOL_H
#define GTIRB_SYMBOL_H

#include <gtirb/Addr.hpp>
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
  Symbol(Addr X);
  Symbol(Addr X, std::string Name,
         StorageKind StorageKind = StorageKind::Extern);
  Symbol(Addr X, std::string Name, const DataObject& Referent,
         StorageKind StorageKind = StorageKind::Extern);
  Symbol(Addr X, std::string Name, const Block& Referent,
         StorageKind StorageKind = StorageKind::Extern);

  ///
  /// Copy constructor. Assigns a new UUID to the copy.
  ///
  explicit Symbol(const Symbol&) = default;

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

  void setAddress(gtirb::Addr X) { Address = X; }
  gtirb::Addr getAddress() const { return Address; }

  void setName(const std::string& X) { Name = X; }
  const std::string& getName() const { return Name; }

  ///
  /// Set the DataObject to which this symbol refers.
  ///
  void setReferent(const DataObject& Data);

  ///
  /// Set the Block to which this symbol refers.
  ///
  void setReferent(const Block& Instruction);

  ///
  /// Get the DataObject to which this symbol refers.
  ///
  NodeRef<DataObject> getDataReferent() const { return DataReferent; }

  ///
  /// Get the Block object to which this symbol refers.
  ///
  NodeRef<Block> getCodeReferent() const { return CodeReferent; }

  void setStorageKind(Symbol::StorageKind X) { Storage = X; }
  gtirb::Symbol::StorageKind getStorageKind() const { return Storage; }

  using MessageType = proto::Symbol;
  void toProtobuf(MessageType* Message) const;
  void fromProtobuf(const MessageType& Message);

private:
  Addr Address;
  std::string Name;
  gtirb::Symbol::StorageKind Storage{StorageKind::Extern};
  NodeRef<DataObject> DataReferent;
  NodeRef<Block> CodeReferent;
};
} // namespace gtirb

#endif // GTIRB_SYMBOL_H
