#ifndef GTIRB_SYMBOL_H
#define GTIRB_SYMBOL_H

#include <gtirb/Addr.hpp>
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
/// \brief DOCFIXME
///
class GTIRB_EXPORT_API Symbol : public Node {
public:

  ///
  /// \enum StorageKind
  ///
  /// \brief DOCFIXME
  ///
  enum class StorageKind : uint8_t {
    Undefined = proto::Storage_Undefined, ///< DOCFIXME
    Normal = proto::Storage_Normal,       ///< DOCFIXME
    Static = proto::Storage_Static,       ///< DOCFIXME
    Extern = proto::Storage_Extern,       ///< DOCFIXME
    Local = proto::Storage_Local          ///< DOCFIXME
  };

  static Symbol* Create(Context& C) { return new (C) Symbol; }
  static Symbol* Create(Context& C, Addr X) { return new (C) Symbol(X); }
  static Symbol* Create(Context& C, Addr X, const std::string& Name,
                        StorageKind Kind = StorageKind::Extern) {
    return new (C) Symbol(X, Name, Kind);
  }
  static Symbol* Create(Context& C, Addr X, const std::string& Name,
                        const DataObject& Referent,
                        StorageKind Kind = StorageKind::Extern) {
    return new (C) Symbol(X, Name, Referent, Kind);
  }
  static Symbol* Create(Context& C, Addr X, const std::string& Name,
                        const Block& Referent,
                        StorageKind Kind = StorageKind::Extern) {
    return new (C) Symbol(X, Name, Referent, Kind);
  }


  /// \brief Set the effective address.
  ///
  /// \param X The effective address to use.
  ///
  /// \return void
  ///
  void setAddress(gtirb::Addr X) { Address = X; }


  /// \brief Get the effective address.
  ///
  /// \return The effective address.
  ///
  gtirb::Addr getAddress() const { return Address; }


  /// \brief Set the name.
  ///
  /// \param X The name to use.
  ///
  /// \return void
  ///
  void setName(const std::string& X) { Name = X; }


  /// \brief Get the name.
  ///
  /// \return The name.
  ///
  const std::string& getName() const { return Name; }


  /// \brief Set the DataObject to which this symbol refers.
  ///
  /// \param Data The DataObject to use.
  ///
  /// \return void
  ///
  void setReferent(const DataObject& Data);


  /// \brief Set the Block to which this symbol refers.
  ///
  /// \param Instruction The Block to use.
  ///
  /// \return void
  ///
  void setReferent(const Block& Instruction);


  /// \brief Get the data to which this symbol refers.
  ///
  /// \return The data, as a NodeRef<DataObject>.
  ///
  /// DOCFIXME[what if the referent is a Block?]
  NodeRef<DataObject> getDataReferent() const { return DataReferent; }

  ///
  /// \brief Get the code to which this symbol refers.
  ///
  /// \return The code, as a NodeRef<Block>.
  ///
  /// DOCFIXME[what if the referent is a DataObject?]
  NodeRef<Block> getCodeReferent() const { return CodeReferent; }


  /// \brief Set the storage kind.
  ///
  /// \param X The storage kind to use.
  ///
  /// \return void
  ///
  void setStorageKind(Symbol::StorageKind X) { Storage = X; }


  /// \brief Get the storage kind.
  ///
  /// \return The storage kind.
  ///
  Symbol::StorageKind getStorageKind() const { return Storage; }


  /// \brief DOCFIXME
  using MessageType = proto::Symbol;


  /// \brief DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return DOCFIXME
  ///
  void toProtobuf(MessageType* Message) const;


  /// \brief DOCFIXME
  ///
  /// \param C       DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return DOCFIXME
  ///
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
  Symbol(Addr X) : Node(Kind::Symbol), Ea(X) {}
  Symbol(Addr X, std::string N, StorageKind SK = StorageKind::Extern)
      : Node(Kind::Symbol), Ea(X), Name(N), Storage(SK) {}
  Symbol(Addr X, std::string Name, const DataObject& Referent,
         StorageKind Kind = StorageKind::Extern);
  Symbol(Addr X, std::string Name, const Block& Referent,
    StorageKind Kind = StorageKind::Extern);

  Addr Ea;
  std::string Name;
  Symbol::StorageKind Storage{StorageKind::Extern};
  std::variant<NodeRef<DataObject>, NodeRef<Block>> Referent;
};
} // namespace gtirb

#endif // GTIRB_SYMBOL_H
