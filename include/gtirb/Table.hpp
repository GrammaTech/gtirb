//===- Table.hpp ------------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_TABLE_H
#define GTIRB_TABLE_H

#include <gtirb/Addr.hpp>
#include <gtirb/Block.hpp>
#include <gtirb/Node.hpp>
#include <boost/endian/conversion.hpp>
#include <list>
#include <map>
#include <string>
#include <type_traits>
#include <typeinfo>
#include <variant>
#include <vector>

/// \file Table.hpp
/// \brief  Definitions and functions for \ref TABLE_GROUP.

namespace proto {
class Table;
} // namespace proto

namespace gtirb {
class Context;

template <class T> struct is_sequence : std::false_type {};
template <class T> struct is_sequence<std::vector<T>> : std::true_type {};
template <class T> struct is_sequence<std::list<T>> : std::true_type {};
/// @{

/// \brief DOCFIXME

template <class T> struct is_mapping : std::false_type {};
template <class T, class U>
struct is_mapping<std::map<T, U>> : std::true_type {};
// Explicitly disable multimap since its semantics are different.
template <class T, class U>
struct is_mapping<std::multimap<T, U>> : std::false_type {};

template <class T> struct is_tuple : std::false_type {};
template <class... Args>
struct is_tuple<std::tuple<Args...>> : std::true_type {};

using to_iterator = std::back_insert_iterator<std::string>;
using from_iterator = std::string::const_iterator;

// Serialize and deserialize by copying the object representation directly.
template <class T> struct default_serialization {
  static void toBytes(const T& object, to_iterator it) {
    // Store as little-endian.
    T reversed = boost::endian::conditional_reverse<
        boost::endian::order::little, boost::endian::order::native>(object);
    auto srcBytes = as_bytes(gsl::make_span(&reversed, 1));
    std::transform(srcBytes.begin(), srcBytes.end(), it,
                   [](auto b) { return char(b); });
  }

  static from_iterator fromBytes(T& object, from_iterator it) {
    auto dest = as_writeable_bytes(gsl::make_span(&object, 1));
    std::for_each(dest.begin(), dest.end(), [&it](auto& b) {
      b = std::byte(*it);
      ++it;
    });
    // Data stored as little-endian.
    boost::endian::conditional_reverse_inplace<boost::endian::order::little,
                                               boost::endian::order::native>(
        object);
    return it;
  }
};

template <class T, class Enable = void> struct table_traits {
  static void toBytes(const T& Object, to_iterator it) = delete;
  static from_iterator fromBytes(T& Object, from_iterator it) = delete;
};
template <class... Ts> struct TypeId {};

template <> struct table_traits<std::byte> : default_serialization<std::byte> {
  static std::string type_id() { return "byte"; }
};

template <> struct table_traits<Addr> : default_serialization<Addr> {
  static std::string type_id() { return "Addr"; }
};

template <> struct table_traits<UUID> : default_serialization<UUID> {
  static std::string type_id() { return "UUID"; }
};

template <class T>
struct table_traits<T, typename std::enable_if_t<std::is_integral<T>::value &&
                                                 std::is_signed<T>::value>>
    : default_serialization<T> {
  static std::string type_id() {
    return "int" + std::to_string(8 * sizeof(T)) + "_t";
  }
};

template <class T>
struct table_traits<T, typename std::enable_if_t<std::is_integral<T>::value &&
                                                 std::is_unsigned<T>::value>>
    : default_serialization<T> {
  static std::string type_id() {
    return "uint" + std::to_string(8 * sizeof(T)) + "_t";
  }
};

template <> struct table_traits<std::string> {
  static std::string type_id() { return "string"; }

  static void toBytes(const std::string& Object, to_iterator It) {
    table_traits<uint64_t>::toBytes(Object.size(), It);
    std::copy(Object.begin(), Object.end(), It);
  }

  static from_iterator fromBytes(std::string& Object, from_iterator It) {
    size_t Count;
    It = table_traits<uint64_t>::fromBytes(Count, It);

    Object.resize(Count);
    std::for_each(Object.begin(), Object.end(), [&It](auto& elt) {
      It = table_traits<char>::fromBytes(elt, It);
    });

    return It;
  }
};

template <> struct table_traits<InstructionRef> {
  static std::string type_id() { return "InstructionRef"; }

  static void toBytes(const InstructionRef& Object, to_iterator It) {
    table_traits<UUID>::toBytes(Object.BlockRef.getUUID(), It);
    table_traits<uint64_t>::toBytes(Object.Offset, It);
  }

  static from_iterator fromBytes(InstructionRef& Object, from_iterator It) {
    UUID Id;
    It = table_traits<UUID>::fromBytes(Id, It);
    Object.BlockRef = NodeRef<Block>(Id);

    It = table_traits<uint64_t>::fromBytes(Object.Offset, It);

    return It;
  }
};

template <class T>
struct table_traits<T, typename std::enable_if_t<is_sequence<T>::value>> {
  static std::string type_id() {
    return "sequence<" + TypeId<typename T::value_type>::value() + ">";
  }

  static void toBytes(const T& Object, to_iterator It) {
    table_traits<uint64_t>::toBytes(Object.size(), It);
    std::for_each(Object.begin(), Object.end(), [&It](const auto& Elt) {
      table_traits<typename T::value_type>::toBytes(Elt, It);
    });
  }

  static from_iterator fromBytes(T& Object, from_iterator It) {
    size_t Count;
    It = table_traits<uint64_t>::fromBytes(Count, It);

    Object.resize(Count);
    std::for_each(Object.begin(), Object.end(), [&It](auto& Elt) {
      It = table_traits<typename T::value_type>::fromBytes(Elt, It);
    });

    return It;
  }
};

template <class T>
struct table_traits<T, typename std::enable_if_t<is_mapping<T>::value>> {
  static std::string type_id() {
    return "mapping<" +
           TypeId<typename T::key_type, typename T::mapped_type>::value() + ">";
  }

  static void toBytes(const T& Object, to_iterator It) {
    table_traits<uint64_t>::toBytes(Object.size(), It);
    std::for_each(Object.begin(), Object.end(), [&It](const auto& Elt) {
      table_traits<typename T::key_type>::toBytes(Elt.first, It);
      table_traits<typename T::mapped_type>::toBytes(Elt.second, It);
    });
  }

  static from_iterator fromBytes(T& Object, from_iterator It) {
    size_t Count;
    It = table_traits<uint64_t>::fromBytes(Count, It);

    for (size_t i = 0; i < Count; i++) {
      typename T::key_type K;
      It = table_traits<decltype(K)>::fromBytes(K, It);
      typename T::mapped_type V;
      It = table_traits<decltype(V)>::fromBytes(V, It);
      Object.emplace(std::move(K), std::move(V));
    }
    return It;
  }
};

template <class T> struct tuple_traits {};
template <class... Ts> struct tuple_traits<std::tuple<Ts...>> {
  using Tuple = std::tuple<Ts...>;

  static std::string type_id() {
    return "tuple<" + TypeId<Ts...>::value() + ">";
  }
};

template <class Func, size_t... Is>
constexpr void static_for(Func&& f, std::integer_sequence<size_t, Is...>) {
  (f(std::integral_constant<size_t, Is>{}), ...);
}

template <class T>
struct table_traits<T, typename std::enable_if_t<is_tuple<T>::value>>
    : tuple_traits<T> {
  static void toBytes(const T& Object, to_iterator It) {
    static_for(
        [&It, &Object](auto i) {
          const auto& F = std::get<i>(Object);
          table_traits<std::remove_cv_t<std::remove_reference_t<decltype(F)>>>::
              toBytes(F, It);
        },
        std::make_index_sequence<std::tuple_size<T>::value>{});
  }

  static from_iterator fromBytes(T& Object, from_iterator It) {
    static_for(
        [&It, &Object](auto i) {
          auto& F = std::get<i>(Object);
          It = table_traits<std::remove_cv_t<
              std::remove_reference_t<decltype(F)>>>::fromBytes(F, It);
        },
        std::make_index_sequence<std::tuple_size<T>::value>{});

    return It;
  }
};

template <class T> struct TypeId<T> {
  static std::string value() { return table_traits<T>::type_id(); }
};

template <class T, class... Ts> struct TypeId<T, Ts...> {
  static std::string value() {
    return table_traits<T>::type_id() + "," + TypeId<Ts...>::value();
  }
};

class TableImpl {
public:
  virtual void toBytes(std::string& Bytes) const = 0;
  virtual void fromBytes(const std::string& Bytes) = 0;
  virtual const std::type_info& storedType() const = 0;
  virtual std::string typeName() const = 0;
  virtual void* get() = 0;
};

template <class T> class TableTemplate : public TableImpl {
public:
  TableTemplate() = default;
  TableTemplate(const T& Val) : Object(Val){};
  TableTemplate(T&& Val) : Object(std::move(Val)){};

  void toBytes(std::string& Bytes) const override {
    table_traits<T>::toBytes(Object, std::back_inserter(Bytes));
  }

  void fromBytes(const std::string& Bytes) override {
    table_traits<T>::fromBytes(Object, Bytes.begin());
  }

  const std::type_info& storedType() const override { return typeid(T); }

  std::string typeName() const override { return TypeId<T>::value(); }

  void* get() { return static_cast<void*>(&Object); }

  T Object;
};


/// \brief A generic \ref TABLE_GROUP "table" for storing additional,
/// client-specific data.
class Table {
public:
  Table() = default;

  template <typename T>
  Table(T&& Value)
      : Impl(std::make_unique<TableTemplate<std::remove_reference_t<T>>>(
            std::forward<T>(Value))) {}

  template <typename T> Table& operator=(T&& Value) {
    this->Impl = std::make_unique<TableTemplate<std::remove_reference_t<T>>>(
        std::forward<T>(Value));
    return *this;
  }

  template <typename T> friend T& get(Table& Table);
  template <typename T> friend std::add_pointer_t<T> get_if(Table* Table);

  /// \brief DOCFIXME
  ///
  /// \param <unnamed>  Not used.
  /// \param  Result    DOCFIXME
  /// \param  Message   DOCFIXME
  ///
  /// \return void
  GTIRB_EXPORT_API friend void fromProtobuf(Context&, Table& result,
                                            const proto::Table& Message);

  /// \brief DOCFIXME
  ///
  /// \param  Expr   DOCFIXME
  ///
  /// \return DOCFIXME
  GTIRB_EXPORT_API friend proto::Table toProtobuf(const Table&);

private:
  std::unique_ptr<TableImpl> Impl;
  std::string RawBytes;
  std::string TypeName;
};

template <typename T> T& get(Table& Foo) {
  if (!Foo.RawBytes.empty()) {
    // Reconstruct from deserialized data
    Foo.Impl = std::make_unique<TableTemplate<T>>();
    Expects(Foo.Impl->typeName() == Foo.TypeName);
    Foo.Impl->fromBytes(Foo.RawBytes);
    Foo.RawBytes.clear();
    Foo.TypeName.clear();
  } else {
    Expects(Foo.Impl != nullptr);
    Expects(typeid(T) == Foo.Impl->storedType());
  }

  return *static_cast<T*>(Foo.Impl->get());
}

template <typename T> std::add_pointer_t<T> get_if(Table* Foo) {
  if (Foo == nullptr) {
    return nullptr;
  }

  if (!Foo->RawBytes.empty()) {
    if (Foo->Impl->typeName() != Foo->TypeName) {
      return nullptr;
    }
  } else {
    if (Foo->Impl == nullptr || typeid(T) != Foo->Impl->storedType()) {
      return nullptr;
    }
  }

  return &get<T>(*Foo);
}
} // namespace gtirb

#endif // GTIRB_TABLE_H
