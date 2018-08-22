#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/NodeRef.hpp>
#include <proto/Symbol.pb.h>
#include <variant>

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

  static Symbol* Create(Context& C) { return new (C) Symbol; }
  static Symbol* Create(Context& C, EA X) { return new (C) Symbol(X); }
  static Symbol* Create(Context& C, EA X, const std::string& Name,
                        StorageKind Kind = StorageKind::Extern) {
    return new (C) Symbol(X, Name, Kind);
  }
  static Symbol* Create(Context& C, EA X, const std::string& Name,
                        const DataObject& Referent,
                        StorageKind Kind = StorageKind::Extern) {
    return new (C) Symbol(X, Name, Referent, Kind);
  }
  static Symbol* Create(Context& C, EA X, const std::string& Name,
                        const Block& Referent,
                        StorageKind Kind = StorageKind::Extern) {
    return new (C) Symbol(X, Name, Referent, Kind);
  }

  void setEA(gtirb::EA X);
  gtirb::EA getEA() const;

  void setName(std::string X);
  std::string getName() const;

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
  NodeRef<DataObject> getDataReferent() const;

  ///
  /// Get the Block object to which this symbol refers.
  ///
  NodeRef<Block> getCodeReferent() const;

  void setStorageKind(Symbol::StorageKind X);
  Symbol::StorageKind getStorageKind() const;

  using MessageType = proto::Symbol;
  void toProtobuf(MessageType* Message) const;
  static Symbol *fromProtobuf(Context &C, const MessageType& Message);

  static bool classof(const Node *N) { return N->getKind() == Kind::Symbol; }

private:
  ///
  /// Default constructor.
  ///
  Symbol() : Node(Kind::Symbol) {}

  ///
  /// This constructor sets the Effective Address on construction.
  ///
  Symbol(EA X) : Node(Kind::Symbol), Ea(X) {}
  Symbol(EA X, std::string N, StorageKind SK = StorageKind::Extern)
      : Node(Kind::Symbol), Ea(X), Name(N), Storage(SK) {}
  Symbol(EA X, std::string Name, const DataObject& Referent,
         StorageKind Kind = StorageKind::Extern);
  Symbol(EA X, std::string Name, const Block& Referent,
    StorageKind Kind = StorageKind::Extern);

  EA Ea{};
  std::string Name;
  Symbol::StorageKind Storage{StorageKind::Extern};
  std::variant<NodeRef<DataObject>, NodeRef<Block>> Referent;
};
} // namespace gtirb
