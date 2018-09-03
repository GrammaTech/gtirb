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

  ///
  /// \brief Default constructor.
  ///
  Symbol() = default;


  /// \brief Constructor.
  ///
  /// \param X The Effective Address for the new Symbol.
  ///
  Symbol(Addr X);


  /// \brief Constructor.
  ///
  /// \param X           The symbol's Effective Address.
  ///
  /// \param Name        The symbol name. DOCFIXME[what happens on conflict?]
  ///
  /// \param StorageKind The storage kind for the symbol. DOCFIXME[parameter name could cause confusion]
  ///
  Symbol(Addr X, std::string Name,
         StorageKind StorageKind = StorageKind::Extern);


  /// \brief Constructor.
  ///
  /// \param X The symbol's Effective Address.
  ///
  /// \param Name        The symbol name. DOCFIXME[what happens on conflict?]
  ///
  /// \param Referent    DOCFIXME
  ///
  /// \param StorageKind The storage kind for the symbol. DOCFIXME[parameter name could cause confusion]
  ///
  Symbol(Addr X, std::string Name, const DataObject& Referent,
         StorageKind StorageKind = StorageKind::Extern);


  /// \brief Constructor.
  ///
  /// \param X The symbol's Effective Address.
  ///
  /// \param Name        The symbol name. DOCFIXME[what happens on conflict?]
  ///
  /// \param Referent    DOCFIXME
  ///
  /// \param StorageKind The storage kind for the symbol. DOCFIXME[parameter name could cause confusion]
  ///
  Symbol(Addr X, std::string Name, const Block& Referent,
         StorageKind StorageKind = StorageKind::Extern);


  /// \brief Copy constructor.
  /// Assigns a new UUID to the copy.
  ///
  explicit Symbol(const Symbol&) = default;


  /// \brief Move constructor.
  ///
  Symbol(Symbol&&) = default;


  /// \brief Move assignment.
  ///
  Symbol& operator=(Symbol&&) = default;


  /// Defaulted trivial destructor.
  ///
  ~Symbol() override = default;


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
  gtirb::Symbol::StorageKind getStorageKind() const { return Storage; }


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
  /// \param Message DOCFIXME
  ///
  /// \return DOCFIXME
  ///
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
