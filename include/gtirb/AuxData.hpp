//===- AuxData.hpp -----------------------------------------------*- C++-*-===//
//
//  Copyright (C) 2020 GrammaTech, Inc.
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
#include <gtirb/Node.hpp>
#include <gtirb/Offset.hpp>
#include <gtirb/proto/AuxData.pb.h>
#include <boost/endian/conversion.hpp>
#include <deque>
#include <list>
#include <map>
#include <string>
#include <type_traits>
#include <typeinfo>
#include <variant>
#include <vector>

/// \file AuxData.hpp
/// \ingroup AUXDATA_GROUP
/// \brief  Types and operations for auxiliary data.
/// \see AUXDATA_GROUP

namespace gtirb {
namespace proto {
class AuxData;
} // namespace proto

class Context;

/// \defgroup AUXDATA_GROUP AuxData
/// \brief \ref AuxData objects can be attached to the \ref IR or individual
/// \ref Modules to store additional client-specific data in a portable way.
///
/// AuxData can store the following types:
///   - all integral types
///   - Addr
///   - Offset
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
///
/// ### 'Sanctioned' AuxData Tables
///
/// We specify a small number of standard AuxData table schemata to
/// support interoperability. For details, see \ref md_AuxData. C++
/// schemata for the sanctioned tables are present in \ref
/// AuxDataSchema.hpp.
///
/// ### Adding Custom AuxData Tables
///
/// Clients may add their own AuxData tables in the C++ API by
/// defining their own schemata. Schemata should be defined by
/// extending the gtirb::schema namespace. Each schema should be a
/// struct declaring public members for the type's name and its C++
/// type. One can follow the model provided by the schemata in \ref
/// AuxDataSchema.hpp.
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
template <class T> struct is_sequence<std::deque<T>> : std::true_type {};
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
template <class T, class U>
struct is_mapping<std::unordered_map<T, U>> : std::true_type {};
// Explicitly disable multimaps. Because they can contain multiple values for
// a given key, they can't be used interchangeably with maps.
template <class T, class U>
struct is_mapping<std::multimap<T, U>> : std::false_type {};
template <class T, class U>
struct is_mapping<std::unordered_multimap<T, U>> : std::false_type {};

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
  static std::string type_name() = delete;
};

/// @cond INTERNAL
template <class... Ts> struct TypeId {};

template <class T>
struct is_endian_type
    : std::integral_constant<bool, std::is_class_v<T> ||
                                       (std::is_integral_v<T> &&
                                        !std::is_same_v<T, bool>)> {};

template <typename T, typename Enable = void> struct default_serialization {};

// Serialize and deserialize by copying the object representation directly.
template <typename T>
struct default_serialization<
    T, typename std::enable_if_t<is_endian_type<T>::value>> {
  static void toBytes(const T& object, to_iterator It) {
    // Store as little-endian.
    T reversed = boost::endian::conditional_reverse<
        boost::endian::order::little, boost::endian::order::native>(object);
    auto srcBytes_begin = reinterpret_cast<std::byte*>(&reversed);
    auto srcBytes_end = reinterpret_cast<std::byte*>(&reversed + 1);
    std::transform(srcBytes_begin, srcBytes_end, It,
                   [](auto b) { return char(b); });
  }

  static from_iterator fromBytes(T& object, from_iterator It) {
    auto dest_begin = reinterpret_cast<std::byte*>(&object);
    auto dest_end = reinterpret_cast<std::byte*>(&object + 1);
    std::for_each(dest_begin, dest_end, [&It](auto& b) {
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
  static std::string type_name() { return "byte"; }

  static void toBytes(std::byte object, to_iterator It) {
    *It = static_cast<char>(object);
  }

  static from_iterator fromBytes(std::byte& object, from_iterator It) {
    object = std::byte(*It);
    ++It;
    return It;
  }
};

template <> struct auxdata_traits<Addr> : default_serialization<Addr> {
  static std::string type_name() { return "Addr"; }
};

template <> struct auxdata_traits<UUID> : default_serialization<UUID> {
  static std::string type_name() { return "UUID"; }
};

template <class T>
struct auxdata_traits<T, typename std::enable_if_t<std::is_integral<T>::value &&
                                                   std::is_signed<T>::value>>
    : default_serialization<T> {
  static std::string type_name() {
    return "int" + std::to_string(8 * sizeof(T)) + "_t";
  }
};

template <class T>
struct auxdata_traits<T, typename std::enable_if_t<std::is_integral<T>::value &&
                                                   std::is_unsigned<T>::value>>
    : default_serialization<T> {
  static std::string type_name() {
    return "uint" + std::to_string(8 * sizeof(T)) + "_t";
  }
};

template <> struct auxdata_traits<std::string> {
  static std::string type_name() { return "string"; }

  static void toBytes(const std::string& Object, to_iterator It) {
    auxdata_traits<uint64_t>::toBytes(Object.size(), It);
    std::copy(Object.begin(), Object.end(), It);
  }

  static from_iterator fromBytes(std::string& Object, from_iterator It) {
    uint64_t Count;
    It = auxdata_traits<uint64_t>::fromBytes(Count, It);

    Object.resize(Count);
    std::for_each(Object.begin(), Object.end(), [&It](auto& elt) {
      It = auxdata_traits<char>::fromBytes(elt, It);
    });

    return It;
  }
};

template <> struct auxdata_traits<Offset> {
  static std::string type_name() { return "Offset"; }

  static void toBytes(const Offset& Object, to_iterator It) {
    auxdata_traits<UUID>::toBytes(Object.ElementId, It);
    auxdata_traits<uint64_t>::toBytes(Object.Displacement, It);
  }

  static from_iterator fromBytes(Offset& Object, from_iterator It) {
    It = auxdata_traits<UUID>::fromBytes(Object.ElementId, It);
    It = auxdata_traits<uint64_t>::fromBytes(Object.Displacement, It);
    return It;
  }
};

template <class T>
struct auxdata_traits<T, typename std::enable_if_t<is_sequence<T>::value>> {
  static std::string type_name() {
    return "sequence<" + TypeId<typename T::value_type>::value() + ">";
  }

  static void toBytes(const T& Object, to_iterator It) {
    auxdata_traits<uint64_t>::toBytes(Object.size(), It);
    std::for_each(Object.begin(), Object.end(), [&It](const auto& Elt) {
      auxdata_traits<typename T::value_type>::toBytes(Elt, It);
    });
  }

  static from_iterator fromBytes(T& Object, from_iterator It) {
    uint64_t Count;
    It = auxdata_traits<uint64_t>::fromBytes(Count, It);

    Object.resize(Count);
    std::for_each(Object.begin(), Object.end(), [&It](auto& Elt) {
      It = auxdata_traits<typename T::value_type>::fromBytes(Elt, It);
    });

    return It;
  }
};

template <class... Args> struct auxdata_traits<std::set<Args...>> {
  using T = std::set<Args...>;

  static std::string type_name() {
    return "set<" + TypeId<typename T::value_type>::value() + ">";
  }

  static void toBytes(const T& Object, to_iterator It) {
    auxdata_traits<uint64_t>::toBytes(Object.size(), It);
    for (const auto& Elt : Object)
      auxdata_traits<typename T::value_type>::toBytes(Elt, It);
  }

  static from_iterator fromBytes(T& Object, from_iterator It) {
    uint64_t Count;
    It = auxdata_traits<uint64_t>::fromBytes(Count, It);

    for (uint64_t i = 0; i < Count; i++) {
      typename T::value_type V;
      It = auxdata_traits<decltype(V)>::fromBytes(V, It);
      Object.emplace(std::move(V));
    }

    return It;
  }
};

template <class T>
struct auxdata_traits<T, typename std::enable_if_t<is_mapping<T>::value>> {
  static std::string type_name() {
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
    uint64_t Count;
    It = auxdata_traits<uint64_t>::fromBytes(Count, It);

    for (uint64_t i = 0; i < Count; i++) {
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

  static std::string type_name() {
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
  static std::string value() { return auxdata_traits<T>::type_name(); }
};

template <class T, class... Ts> struct TypeId<T, Ts...> {
  static std::string value() {
    return auxdata_traits<T>::type_name() + "," + TypeId<Ts...>::value();
  }
};
/// @endcond

/// @cond INTERNAL
class GTIRB_EXPORT_API AuxData {
public:
  /// !brief Structure containing the serialized representation of an
  /// !AuxData.
  struct SerializedForm {
    std::string RawBytes;
    std::string ProtobufType;
  };

  /// \brief Construct an empty table.
  AuxData() = default;

  virtual ~AuxData() = default;

  /// \brief Returns the serialized representation of this AuxData.
  ///
  /// If the object is newly constructed (and wasn't unserialized from
  /// protobuf), the content present in the returned structure will be
  /// empty.
  ///
  /// If the object has been modified since being unserialized, the
  /// content returned here will not incorporate those modifications.
  ///
  /// This interface is provided primarily as a means for clients to
  /// inspect the raw data of AuxData objects whose types have not
  /// been registered.
  const SerializedForm& rawData() const { return this->SF; }

  /// !brief The degenerate api type id used for AuxData types that
  /// haven't been registered.
  static constexpr std::size_t UNREGISTERED_API_TYPE_ID = 0;

  /// !brief Retrieve a type-trait-specific Id number.
  virtual std::size_t getApiTypeId() const { return UNREGISTERED_API_TYPE_ID; }

  /// \brief The protobuf message type used for serializing AuxData.
  using MessageType = proto::AuxData;

  /// \brief Initialize an AuxData from a protobuf message.
  ///
  /// \param Message     The protobuf message from which to deserialize.
  /// \param[out] Result  The AuxData to initialize.
  static void fromProtobuf(AuxData& Result, const MessageType& Message);

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message  A protobuf message representing the AuxData.
  virtual void toProtobuf(MessageType* Message) const;

private:
  SerializedForm SF;
};

template <class Schema> class AuxDataImpl : public AuxData {
public:
  AuxDataImpl() = default;
  AuxDataImpl(typename Schema::Type&& Val) : Object(std::move(Val)){};

  /// !brief Register/retrieve a type-trait-specific Id number.
  static std::size_t staticGetApiTypeId() {
    return typeid(typename Schema::Type).hash_code();
  }

  /// \brief Retrieve a type-trait-specific Id number.
  virtual std::size_t getApiTypeId() const override {
    return staticGetApiTypeId();
  }

  const typename Schema::Type* get() const { return &Object; }

  static std::unique_ptr<AuxDataImpl<Schema>>
  fromProtobuf(const MessageType& Message) {
    // Check if the serialized type isn't compatible with the type
    // we're trying to deserialize to.
    if (Message.type_name() !=
        auxdata_traits<typename Schema::Type>::type_name()) {
      return nullptr;
    }

    auto TypedAuxData = std::make_unique<AuxDataImpl<Schema>>();
    AuxData::fromProtobuf(*TypedAuxData, Message);
    auxdata_traits<typename Schema::Type>::fromBytes(TypedAuxData->Object,
                                                     Message.data().begin());
    return TypedAuxData;
  }

  /// \brief Serialize into a protobuf message.
  ///
  /// \param Message     The Message to serialize into.
  virtual void toProtobuf(MessageType* Message) const override {
    Message->set_type_name(auxdata_traits<typename Schema::Type>::type_name());
    auxdata_traits<typename Schema::Type>::toBytes(
        this->Object, std::back_inserter(*Message->mutable_data()));
  }

private:
  typename Schema::Type Object;
};
/// @endcond

// (end \defgroup AUXDATA_GROUP)

} // namespace gtirb

#endif // GTIRB_AUXDATA_H
