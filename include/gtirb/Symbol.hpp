//===- Symbol.hpp -----------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_SYMBOL_H
#define GTIRB_SYMBOL_H

#include <gtirb/Addr.hpp>
#include <gtirb/Block.hpp>
#include <gtirb/DataObject.hpp>
#include <gtirb/Node.hpp>
#include <proto/Symbol.pb.h>
#include <type_traits>

namespace gtirb {
template <typename NodeTy> struct NodeTraits {
  static constexpr bool IsReferent = false;
};

template <> struct NodeTraits<DataObject> {
  static constexpr bool IsReferent = true;
};

template <> struct NodeTraits<Block> {
  static constexpr bool IsReferent = true;
};

/// \class Symbol
///
/// \brief DOCFIXME
class GTIRB_EXPORT_API Symbol : public Node {
public:
  /// \enum StorageKind
  ///
  /// \brief DOCFIXME
  enum class StorageKind : uint8_t {
    Undefined = proto::Storage_Undefined, ///< DOCFIXME
    Normal = proto::Storage_Normal,       ///< DOCFIXME
    Static = proto::Storage_Static,       ///< DOCFIXME
    Extern = proto::Storage_Extern,       ///< DOCFIXME
    Local = proto::Storage_Local          ///< DOCFIXME
  };

  /// \brief Create a Symbol object in its default state.
  ///
  /// \param C  The Context in which this object will be held.
  ///
  /// \return The newly created object.
  static Symbol* Create(Context& C) { return new (C) Symbol(C); }

  /// \brief Create a Symbol object.
  ///
  /// \param C  The Context in which this object will be held.
  /// \param X  The address of the symbol.
  ///
  /// \return The newly created object.
  static Symbol* Create(Context& C, Addr X) { return new (C) Symbol(C, X); }

  /// \brief Create a Symbol object.
  ///
  /// \param C  The Context in which this object will be held.
  /// \param X  The address of the symbol.
  /// \param Name The name of the symbol.
  /// \param Kind The storage kind the symbol has; defaults to
  /// StorageKind::Extern
  ///
  /// \return The newly created object.
  static Symbol* Create(Context& C, Addr X, const std::string& Name,
                        StorageKind Kind = StorageKind::Extern) {
    return new (C) Symbol(C, X, Name, Kind);
  }

  /// \brief Create a Symbol object.
  ///
  /// \param C  The Context in which this object will be held.
  /// \param X  The address of the symbol.
  /// \param Name The name of the symbol.
  /// \param Referent The DataObject this symbol refers to.
  /// \param Kind The storage kind the symbol has; defaults to
  /// StorageKind::Extern
  ///
  /// \return The newly created object.
  static Symbol* Create(Context& C, Addr X, const std::string& Name,
                        DataObject* Referent,
                        StorageKind Kind = StorageKind::Extern) {
    return new (C) Symbol(C, X, Name, Referent, Kind);
  }

  /// \brief Create a Symbol object.
  ///
  /// \param C  The Context in which this object will be held.
  /// \param X  The address of the symbol.
  /// \param Name The name of the symbol.
  /// \param Referent The Block this symbol refers to.
  /// \param Kind The storage kind the symbol has; defaults to
  /// StorageKind::Extern
  ///
  /// \return The newly created object.
  static Symbol* Create(Context& C, Addr X, const std::string& Name,
                        Block* Referent,
                        StorageKind Kind = StorageKind::Extern) {
    return new (C) Symbol(C, X, Name, Referent, Kind);
  }

  /// \brief Set the effective address.
  ///
  /// \param X The effective address to use.
  ///
  /// \return void
  void setAddress(gtirb::Addr X) { Address = X; }

  /// \brief Get the effective address.
  ///
  /// \return The effective address.
  gtirb::Addr getAddress() const { return Address; }

  /// \brief Set the name.
  ///
  /// \param X The name to use.
  ///
  /// \return void
  void setName(const std::string& X) { Name = X; }

  /// \brief Get the name.
  ///
  /// \return The name.
  const std::string& getName() const { return Name; }

  /// \brief Set the DataObject to which this symbol refers.
  ///
  /// \tparam NodeTy  A Node type of a supported referent; should be
  /// automatically deduced.
  ///
  /// \param N The Node to refer to.
  ///
  /// \return void
  template <typename NodeTy> void
  setReferent(NodeTy* N,
              std::enable_if_t<NodeTraits<NodeTy>::IsReferent>* = nullptr) {
    Referent = N;
  }

  /// \brief Deleted overload used to prevent setting a referent of an
  /// unsupported type.
  ///
  /// \tparam NodeTy  An arbitrary type; should be automatically deduced.
  ///
  /// \return void
  template <typename NodeTy>
  void setReferent(
      NodeTy*,
      std::enable_if_t<!NodeTraits<NodeTy>::IsReferent>* = nullptr) = delete;

  /// \brief Get the referent to which this symbol refers.
  ///
  /// \tparam NodeTy A Node type of a supported referent.
  ///
  /// \return The data, dynamically typed as the given NodeTy, or null if there
  /// is no referent of that type.
  ///
  /// DOCFIXME
  template <typename NodeTy> NodeTy* getReferent() {
    return dyn_cast_or_null<NodeTy>(Referent);
  }

  /// \brief Get the referent to which this symbol refers.
  ///
  /// \tparam NodeTy A Node type of a supported referent.
  ///
  /// \return The data, dynamically typed as the given NodeTy, or null if there
  /// is no referent of that type.
  ///
  /// DOCFIXME
  template <typename NodeTy> const NodeTy* getReferent() const {
    return dyn_cast_or_null<NodeTy>(Referent);
  }

  /// \brief Set the storage kind.
  ///
  /// \param X The storage kind to use.
  ///
  /// \return void
  void setStorageKind(Symbol::StorageKind X) { Storage = X; }

  /// \brief Get the storage kind.
  ///
  /// \return The storage kind.
  Symbol::StorageKind getStorageKind() const { return Storage; }

  /// \brief DOCFIXME
  using MessageType = proto::Symbol;

  /// \brief DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief DOCFIXME
  ///
  /// \param C       DOCFIXME
  /// \param Message DOCFIXME
  ///
  /// \return The deserialized Symbol object, or null on failure.
  static Symbol* fromProtobuf(Context& C, const MessageType& Message);

  static bool classof(const Node* N) { return N->getKind() == Kind::Symbol; }

private:
  Symbol(Context& C) : Node(C, Kind::Symbol) {}
  Symbol(Context& C, Addr X) : Node(C, Kind::Symbol), Address(X) {}
  Symbol(Context& C, Addr X, const std::string& N,
         StorageKind SK = StorageKind::Extern)
      : Node(C, Kind::Symbol), Address(X), Name(N), Storage(SK) {}
  Symbol(Context& C, Addr X, const std::string& N, Node* R,
         StorageKind SK = StorageKind::Extern)
      : Node(C, Kind::Symbol), Address(X), Name(N), Storage(SK), Referent(R) {}

  Addr Address;
  std::string Name;
  Symbol::StorageKind Storage{StorageKind::Extern};
  Node* Referent;
};
} // namespace gtirb

#endif // GTIRB_SYMBOL_H
