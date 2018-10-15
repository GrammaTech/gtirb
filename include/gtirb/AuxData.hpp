//===- AuxData.hpp ---------------------------------------------*- C++-*-===//
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
#ifndef GTIRB_AUXDATA_H
#define GTIRB_AUXDATA_H

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

/// \file AuxData.hpp
/// \ingroup AUXDATA_GROUP
/// \brief  Types and operations for auxiliar data.
/// \see AUXDATA_GROUP

namespace proto {
class AuxData;
} // namespace proto

namespace gtirb {
class Context;

/// \defgroup AUXDATA_GROUP AuxData
/// \brief \ref AuxData objects can be attached to the \ref IR to store
/// additional client-specific data in a portable way.
///
/// AuxData can store the following types:
///   - all integral types
///   - Addr
///   - InstructionRef
///   - \ref UUID
///   - sequential containers
///   - mapping containers
///   - std::tuple
///
/// ### Supporting Additional Types
///
/// Support for additional containers can be added by specializing \ref
/// is_sequence or \ref is_mapping. Once serialized, the data does not
/// depend on any specific container type, and its contents can be
/// deserialized into different containers of the same kind (e.g. \c std::list
/// to \c std::vector).
///
/// Support for other types can be added by specializing \ref auxdata_traits to
/// provide serialization functions. However, \ref AuxData containing these
/// types will not be accessible to other clients which are not compiled with
/// support for those types. It is preferable to store data using the basic
/// types whenever possible, in order to maximize interoperability.
///
/// ### Serialization Format
///
/// AuxData is serialized by packing the contents into a byte array, which
/// is stored in a protobuf message along with a string which identifies the
/// type in a portable fashion.
///
/// Fixed-size types such as integers, Addr, etc are packed by swapping their
/// bytes to little-endian order and writing them directly to the byte
/// array. Containers first write out the number of elements (as a uint64_t),
/// then write each element one after another. Tuples are similar but omit
/// the size, since it can be inferred from the type.

/// @{

/// \struct is_sequence
///
/// \brief Trait class that identifies whether T is a sequential
/// container type.
///
/// \see AUXDATA_GROUP
template <class T> struct is_sequence : std::false_type {};

/// @cond INTERNAL
template <class T> struct is_sequence<std::vector<T>> : std::true_type {};
template <class T> struct is_sequence<std::list<T>> : std::true_type {};
/// @endcond

/// \struct is_mapping
///
/// \brief Trait class that identifies whether T is a mapping container type.
///
/// \see AUXDATA_GROUP
template <class T> struct is_mapping : std::false_type {};
/// @cond INTERNAL
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
///@endcond

/// \struct auxdata_traits
///
/// \brief Provides type information and serialization functions
/// for types which can be stored in \ref AuxData.
///
/// \see AUXDATA_GROUP
template <class T, class Enable = void> struct auxdata_traits {
  /// \brief  Serialize an object to a sequence of bytes.
  ///
  /// \param Object  The object to serialize.
  /// \param It      Store byte sequence here.
  static void toBytes(const T& Object, to_iterator It) = delete;

  /// \brief  Deserialize an object from a sequence of bytes.
  ///
  /// \param Object  The object to deserialize.
  /// \param It      Read bytes from here.
  /// \return An iterator pointing to the first byte after the representation
  /// of \p Object.
  static from_iterator fromBytes(T& Object, from_iterator It) = delete;

  /// \brief String representation of the serialized type of T.
  ///
  /// This identifier is portable and independent of the specific container
  /// types. Integral types are represented with an exact size (e.g.
  /// "uint32_t"). Sequential containers are represented as "sequence<...>", and
  /// mapping containers are represented as "mapping<...>".
  static std::string type_id() = delete;
};

/// @cond INTERNAL
template <class... Ts> struct TypeId {};

// Serialize and deserialize by copying the object representation directly.
template <class T> struct default_serialization {
  static void toBytes(const T& object, to_iterator It) {
    // Store as little-endian.
    T reversed = boost::endian::conditional_reverse<
        boost::endian::order::little, boost::endian::order::native>(object);
    auto srcBytes = as_bytes(gsl::make_span(&reversed, 1));
    std::transform(srcBytes.begin(), srcBytes.end(), It,
                   [](auto b) { return char(b); });
  }

  static from_iterator fromBytes(T& object, from_iterator It) {
    auto dest = as_writeable_bytes(gsl::make_span(&object, 1));
    std::for_each(dest.begin(), dest.end(), [&It](auto& b) {
      b = std::byte(*It);
      ++It;
    });
    // Data stored as little-endian.
    boost::endian::conditional_reverse_inplace<boost::endian::order::little,
                                               boost::endian::order::native>(
        object);
    return It;
  }
};

template <>
struct auxdata_traits<std::byte> : default_serialization<std::byte> {
  static std::string type_id() { return "byte"; }
};

template <> struct auxdata_traits<Addr> : default_serialization<Addr> {
  static std::string type_id() { return "Addr"; }
};

template <> struct auxdata_traits<UUID> : default_serialization<UUID> {
  static std::string type_id() { return "UUID"; }
};

template <class T>
struct auxdata_traits<T, typename std::enable_if_t<std::is_integral<T>::value &&
                                                   std::is_signed<T>::value>>
    : default_serialization<T> {
  static std::string type_id() {
    return "int" + std::to_string(8 * sizeof(T)) + "_t";
  }
};

template <class T>
struct auxdata_traits<T, typename std::enable_if_t<std::is_integral<T>::value &&
                                                   std::is_unsigned<T>::value>>
    : default_serialization<T> {
  static std::string type_id() {
    return "uint" + std::to_string(8 * sizeof(T)) + "_t";
  }
};

template <> struct auxdata_traits<std::string> {
  static std::string type_id() { return "string"; }

  static void toBytes(const std::string& Object, to_iterator It) {
    auxdata_traits<uint64_t>::toBytes(Object.size(), It);
    std::copy(Object.begin(), Object.end(), It);
  }

  static from_iterator fromBytes(std::string& Object, from_iterator It) {
    size_t Count;
    It = auxdata_traits<uint64_t>::fromBytes(Count, It);

    Object.resize(Count);
    std::for_each(Object.begin(), Object.end(), [&It](auto& elt) {
      It = auxdata_traits<char>::fromBytes(elt, It);
    });

    return It;
  }
};

template <> struct auxdata_traits<InstructionRef> {
  static std::string type_id() { return "InstructionRef"; }

  static void toBytes(const InstructionRef& Object, to_iterator It) {
    auxdata_traits<UUID>::toBytes(Object.BlockId, It);
    auxdata_traits<uint64_t>::toBytes(Object.Offset, It);
  }

  static from_iterator fromBytes(InstructionRef& Object, from_iterator It) {
    It = auxdata_traits<UUID>::fromBytes(Object.BlockId, It);
    It = auxdata_traits<uint64_t>::fromBytes(Object.Offset, It);
    return It;
  }
};

template <class T>
struct auxdata_traits<T, typename std::enable_if_t<is_sequence<T>::value>> {
  static std::string type_id() {
    return "sequence<" + TypeId<typename T::value_type>::value() + ">";
  }

  static void toBytes(const T& Object, to_iterator It) {
    auxdata_traits<uint64_t>::toBytes(Object.size(), It);
    std::for_each(Object.begin(), Object.end(), [&It](const auto& Elt) {
      auxdata_traits<typename T::value_type>::toBytes(Elt, It);
    });
  }

  static from_iterator fromBytes(T& Object, from_iterator It) {
    size_t Count;
    It = auxdata_traits<uint64_t>::fromBytes(Count, It);

    Object.resize(Count);
    std::for_each(Object.begin(), Object.end(), [&It](auto& Elt) {
      It = auxdata_traits<typename T::value_type>::fromBytes(Elt, It);
    });

    return It;
  }
};

template <class T>
struct auxdata_traits<T, typename std::enable_if_t<is_mapping<T>::value>> {
  static std::string type_id() {
    return "mapping<" +
           TypeId<typename T::key_type, typename T::mapped_type>::value() + ">";
  }

  static void toBytes(const T& Object, to_iterator It) {
    auxdata_traits<uint64_t>::toBytes(Object.size(), It);
    std::for_each(Object.begin(), Object.end(), [&It](const auto& Elt) {
      auxdata_traits<typename T::key_type>::toBytes(Elt.first, It);
      auxdata_traits<typename T::mapped_type>::toBytes(Elt.second, It);
    });
  }

  static from_iterator fromBytes(T& Object, from_iterator It) {
    size_t Count;
    It = auxdata_traits<uint64_t>::fromBytes(Count, It);

    for (size_t i = 0; i < Count; i++) {
      typename T::key_type K;
      It = auxdata_traits<decltype(K)>::fromBytes(K, It);
      typename T::mapped_type V;
      It = auxdata_traits<decltype(V)>::fromBytes(V, It);
      Object.emplace(std::move(K), std::move(V));
    }
    return It;
  }
};
/// @endcond

/// @cond INTERNAL
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
struct auxdata_traits<T, typename std::enable_if_t<is_tuple<T>::value>>
    : tuple_traits<T> {
  static void toBytes(const T& Object, to_iterator It) {
    static_for(
        [&It, &Object](auto i) {
          const auto& F = std::get<i>(Object);
          auxdata_traits<std::remove_cv_t<
              std::remove_reference_t<decltype(F)>>>::toBytes(F, It);
        },
        std::make_index_sequence<std::tuple_size<T>::value>{});
  }

  static from_iterator fromBytes(T& Object, from_iterator It) {
    static_for(
        [&It, &Object](auto i) {
          auto& F = std::get<i>(Object);
          It = auxdata_traits<std::remove_cv_t<
              std::remove_reference_t<decltype(F)>>>::fromBytes(F, It);
        },
        std::make_index_sequence<std::tuple_size<T>::value>{});

    return It;
  }
};
/// @endcond

/// @cond INTERNAL
template <class T> struct TypeId<T> {
  static std::string value() { return auxdata_traits<T>::type_id(); }
};

template <class T, class... Ts> struct TypeId<T, Ts...> {
  static std::string value() {
    return auxdata_traits<T>::type_id() + "," + TypeId<Ts...>::value();
  }
};
/// @endcond

/// @cond INTERNAL
class AuxDataImpl {
public:
  virtual ~AuxDataImpl() = default;
  virtual void toBytes(std::string& Bytes) const = 0;
  virtual void fromBytes(const std::string& Bytes) = 0;
  virtual const std::type_info& storedType() const = 0;
  virtual std::string typeName() const = 0;
  virtual void* get() = 0;
};

template <class T> class AuxDataTemplate : public AuxDataImpl {
public:
  AuxDataTemplate() = default;
  AuxDataTemplate(const T& Val) : Object(Val){};
  AuxDataTemplate(T&& Val) : Object(std::move(Val)){};

  void toBytes(std::string& Bytes) const override {
    auxdata_traits<T>::toBytes(Object, std::back_inserter(Bytes));
  }

  void fromBytes(const std::string& Bytes) override {
    auxdata_traits<T>::fromBytes(Object, Bytes.begin());
  }

  const std::type_info& storedType() const override { return typeid(T); }

  std::string typeName() const override { return TypeId<T>::value(); }

  void* get() override { return static_cast<void*>(&Object); }

  T Object;
};
/// @endcond

/// \brief A generic object for storing additional client-specific data.
///
/// \see \ref AUXDATA_GROUP

class AuxData {
public:
  /// \brief Construct an empty table.
  AuxData() = default;

  /// \brief Construct an \ref AuxData containing a value.
  ///
  /// \param Value  The contents of the \ref AuxData.
  template <typename T>
  AuxData(T&& Value)
      : Impl(std::make_unique<AuxDataTemplate<std::remove_reference_t<T>>>(
            std::forward<T>(Value))) {}

  /// \brief Store a new value, destroying the previous contents.
  ///
  /// \param Value  The value to store.
  template <typename T> AuxData& operator=(T&& Value) {
    this->Impl = std::make_unique<AuxDataTemplate<std::remove_reference_t<T>>>(
        std::forward<T>(Value));
    return *this;
  }

  /// \brief Get the contents of the \ref AuxData.
  ///
  /// \tparam T  The expected type of the contents.
  ///
  /// \returns If the \ref AuxData contains an object of type T, return a
  /// pointer to it. Otherwise return nullptr.
  //
  template <typename T> T* get() {
    if (!this->RawBytes.empty()) {
      // Reconstruct from deserialized data
      this->Impl = std::make_unique<AuxDataTemplate<T>>();

      if (this->Impl->typeName() != this->TypeName) {
        return nullptr;
      }

      this->Impl->fromBytes(this->RawBytes);
      this->RawBytes.clear();
      this->TypeName.clear();
    } else if (this->Impl == nullptr || typeid(T) != this->Impl->storedType()) {
      return nullptr;
    }

    return static_cast<T*>(this->Impl->get());
  }

  /// \brief A string representation of the type of the stored data.
  ///
  /// \returns The type name, or an empty string if no value is stored.
  std::string typeName() const {
    if (this->Impl) {
      return this->Impl->typeName();
    } else {
      return this->TypeName;
    }
  }

  /// \brief Initialize an AuxData from a protobuf message.
  ///
  /// \param <unnamed>   Not used.
  /// \param Message     The protobuf message from which to deserialize.
  /// \param[out] Result  The AuxData to initialize.
  GTIRB_EXPORT_API friend void fromProtobuf(Context&, AuxData& Result,
                                            const proto::AuxData& Message);

  /// \brief Serialize into a protobuf message.
  ///
  /// \param <unnamed>     The AuxData to serialize.
  ///
  /// \return A protobuf message representing the AuxData.
  GTIRB_EXPORT_API friend proto::AuxData toProtobuf(const AuxData&);

private:
  std::unique_ptr<AuxDataImpl> Impl;
  std::string RawBytes;
  std::string TypeName;
};

/// @}
// (end \defgroup AUXDATA_GROUP)

} // namespace gtirb

#endif // GTIRB_AUXDATA_H
