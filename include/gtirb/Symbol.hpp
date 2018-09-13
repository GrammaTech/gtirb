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
#include <functional>
#include <optional>
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

namespace details {
template <typename... Ts> struct TypeList {};
}

/// \class Symbol
///
/// \brief DOCFIXME
class GTIRB_EXPORT_API Symbol : public Node {
  // SFINAE overload handling a Callable with a void return type.
  template <typename Callable, typename Ty>
  auto visit_impl_help(Callable&& Visitor) const -> std::enable_if_t<
      std::is_void_v<decltype(std::invoke(Visitor, (Ty*)0))>> {
    static_assert(std::is_invocable_v<Callable, Ty*>,
                  "Visitor must contain an overloaded function call operator "
                  "for each of the types in supported_types");
    if (Ty* Obj = dyn_cast_or_null<Ty>(this->Referent)) {
      std::invoke(Visitor, Obj);
    }
  }

  // SFINAE overload handling a Callable with a non-void return type.
  template <typename Callable, typename Ty>
  auto visit_impl_help(Callable&& Visitor) const -> std::enable_if_t<
      !std::is_void_v<decltype(std::invoke(Visitor, (Ty*)0))>,
      std::optional<decltype(std::invoke(Visitor, (Ty*)0))>> {
    static_assert(std::is_invocable_v<Callable, Ty*>,
                  "Visitor must contain an overloaded function call operator "
                  "for each of the types in supported_types");
    if (Ty* Obj = dyn_cast_or_null<Ty>(this->Referent)) {
      return std::invoke(Visitor, Obj);
    }
    return std::nullopt;
  }

  // Helper type traits used to determine whether all of the Callable object's
  // return types agree. Note that the return types do not need to be the same,
  // but do need to be implicitly convertible to the same common type. e.g.,
  // this class is a valid Callable.
  //   struct Visitor {
  //     int operator()(Block*);
  //     long operator()(DataObject*);
  //  };
  template <typename AlwaysVoid, typename Callable,
            template <typename...> typename TypeList, typename... Types>
  struct has_common_type_impl : std::false_type {};

  template <typename Callable, template <typename...> typename TypeList,
            typename... Types>
  struct has_common_type_impl<std::void_t<std::common_type_t<
                                  std::invoke_result_t<Callable, Types*>...>>,
                              Callable, TypeList, Types...> : std::true_type {};

  template <typename Callable, template <typename...> typename TypeList,
            typename... Types>
  static constexpr bool has_common_type_v =
      has_common_type_impl<void, Callable, TypeList, Types...>::value;

  // Helper function to unpack all of the types in the type list and attempt to
  // visit Callable once per type. Verifies that the Callable objects all share
  // a compatible return type.
  template <typename Callable, template <typename...> typename TypeList,
            typename... Types>
  auto visit_impl(Callable&& Visitor, TypeList<Types...>) const {
    // If this assertion fails, the return values from the Callable object are
    // not compatible enough. This can happen if they return incompatible types
    // or if there is not an overload for each referent type.
    static_assert(has_common_type_v<Callable, TypeList, Types...>,
                  "incompatible return types for the Callable object");
    // Instantiate a call to the Visitor once for each of the listed types, but
    // only issue the call at runtime if the Referent can be dynamically cast to
    // the given type. In this way, the Visitor needs to be able to handle any
    // of the supported types, but will only be called once for the concrete
    // type of the Referent.
    //
    // If you get an error about there being no matching overloaded function for
    // the call to visit_impl_help, that is most likely because there are one
    // or more overloads missing for each referent type.
    return (...,
            visit_impl_help<Callable, Types>(std::forward<Callable>(Visitor)));
  }

public:
  /// \brief The list of supported referent types.
  using supported_referent_types = details::TypeList<Block, DataObject>;

  /// \brief Visits the symbol's referent, if one is present, by concrete
  /// referent type. For example:
  ///
  /// \code
  /// struct Visitor {
  ///   int operator()(Block*) { return 0; }
  ///   long operator()(DataObject*) { return 1; }
  /// };
  ///
  /// Context Ctx;
  /// Symbol* SymB = Symbol::Create(Ctx, Addr(0), "", Block::Create(Ctx));
  /// Symbol* SymD = Symbol::Create(Ctx, Addr(0), "", DataObject::Create(Ctx));
  /// Symbol* SymN = Symbol::Create(Ctx);
  ///
  /// SymB->visit(Visitor{}); // Will call Visitor::operator()(Block*);
  /// SymD->visit(Visitor{}); // Will call Visitor::operator()(DatObject*);
  /// SymN->visit(Visitor{}); // Will not call either overload
  /// \endcode
  ///
  /// \tparam Callable  A callable function type. This type must be able to be
  /// called with a pointer to all of the types listed in \ref
  /// supported_referent_types. All overloaded functions must have a common,
  /// compatible return type.
  ///
  /// \param Visitor  A callable object that will be called with a nonnull
  /// symbol referent.
  template <typename Callable> auto visit(Callable&& Visitor) const {
    return visit_impl(std::forward<Callable>(Visitor),
                      std::decay_t<supported_referent_types>{});
  }

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
  Node* Referent{nullptr};
};
} // namespace gtirb

#endif // GTIRB_SYMBOL_H
