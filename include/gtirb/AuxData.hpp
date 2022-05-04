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
#include <boost/endian/conversion.hpp>
#include <deque>
#include <list>
#include <map>
#include <string>
#include <type_traits>
#include <typeinfo>
#include <unordered_map>
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

template <class... Args>
struct is_tuple<std::pair<Args...>> : std::true_type {};

// Utility class for serializing AuxData.
class ToByteRange {
public:
  explicit ToByteRange(std::string& Bytes) : It(std::back_inserter(Bytes)) {}

  void write(std::byte Byte) { *It = static_cast<char>(Byte); }

private:
  std::back_insert_iterator<std::string> It;
};

// Utility class for deserializing AuxData.
class FromByteRange {
public:
  explicit FromByteRange(const std::string& Bytes)
      : Curr(Bytes.begin()), End(Bytes.end()) {}

  bool read(std::byte& Byte) {
    if (Curr == End)
      return false;

    Byte = std::byte(*Curr);
    ++Curr;
    return true;
  }

  uint64_t remainingBytesToRead() const {
    return static_cast<uint64_t>(std::distance(Curr, End));
  }

private:
  std::string::const_iterator Curr;
  std::string::const_iterator End;
};

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
  /// \param TBR     Store byte sequence here.
  static void toBytes(const T& Object, ToByteRange& TBR) = delete;

  /// \brief  Deserialize an object from a sequence of bytes.
  ///
  /// \param Object  The object to deserialize.
  /// \param FBR     Read bytes from here.
  /// \return True/false dependending on if the bytes were deserialized
  /// successfully.
  static bool fromBytes(T& Object, FromByteRange& FBR) = delete;

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
    T, typename std::enable_if_t<is_endian_type<T>::value ||
                                 std::is_floating_point<T>::value>> {
  static void toBytes(const T& object, ToByteRange& TBR) {
    // Store as little-endian.
    T ordered = object;

    if constexpr (!std::is_floating_point<T>::value) {
      // Do not reorder floating point values
      boost::endian::conditional_reverse_inplace<boost::endian::order::little,
                                                 boost::endian::order::native>(
          ordered);
    }
    auto srcBytes_begin = reinterpret_cast<const std::byte*>(&ordered);
    auto srcBytes_end = reinterpret_cast<const std::byte*>(&ordered + 1);
    std::for_each(srcBytes_begin, srcBytes_end, [&](auto b) { TBR.write(b); });
  }

  static bool fromBytes(T& object, FromByteRange& FBR) {
    auto dest_begin = reinterpret_cast<std::byte*>(&object);
    auto dest_end = reinterpret_cast<std::byte*>(&object + 1);
    bool Success = true;
    std::for_each(dest_begin, dest_end, [&](auto& b) {
      if (!FBR.read(b))
        Success = false;
    });
    if (!Success) {
      return false;
    }

    // Data stored as little-endian.
    if constexpr (!std::is_floating_point<T>::value) {
      boost::endian::conditional_reverse_inplace<boost::endian::order::little,
                                                 boost::endian::order::native>(
          object);
    }

    return true;
  }
};

template <>
struct auxdata_traits<std::byte> : default_serialization<std::byte> {
  static std::string type_name() { return "byte"; }

  static void toBytes(std::byte object, ToByteRange& TBR) { TBR.write(object); }

  static bool fromBytes(std::byte& object, FromByteRange& FBR) {
    return FBR.read(object);
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

template <> struct auxdata_traits<float> : default_serialization<float> {
  static std::string type_name() { return "float"; }
};

template <> struct auxdata_traits<double> : default_serialization<double> {
  static std::string type_name() { return "double"; }
};

template <> struct auxdata_traits<std::string> {
  static std::string type_name() { return "string"; }

  static void toBytes(const std::string& Object, ToByteRange& TBR) {
    auxdata_traits<uint64_t>::toBytes(Object.size(), TBR);
    std::for_each(Object.begin(), Object.end(),
                  [&](auto& elt) { auxdata_traits<char>::toBytes(elt, TBR); });
  }

  static bool fromBytes(std::string& Object, FromByteRange& FBR) {
    uint64_t Count;
    if (!auxdata_traits<uint64_t>::fromBytes(Count, FBR))
      return false;

    if (Count > FBR.remainingBytesToRead())
      return false;

    Object.resize(Count);
    bool Success = true;
    std::for_each(Object.begin(), Object.end(), [&](auto& elt) {
      if (!auxdata_traits<char>::fromBytes(elt, FBR))
        Success = false;
    });

    return Success;
  }
};

template <> struct auxdata_traits<Offset> {
  static std::string type_name() { return "Offset"; }

  static void toBytes(const Offset& Object, ToByteRange& TBR) {
    auxdata_traits<UUID>::toBytes(Object.ElementId, TBR);
    auxdata_traits<uint64_t>::toBytes(Object.Displacement, TBR);
  }

  static bool fromBytes(Offset& Object, FromByteRange& FBR) {
    if (!auxdata_traits<UUID>::fromBytes(Object.ElementId, FBR))
      return false;
    return auxdata_traits<uint64_t>::fromBytes(Object.Displacement, FBR);
  }
};

template <class T>
struct auxdata_traits<T, typename std::enable_if_t<is_sequence<T>::value>> {
  static std::string type_name() {
    return "sequence<" + TypeId<typename T::value_type>::value() + ">";
  }

  static void toBytes(const T& Object, ToByteRange& TBR) {
    auxdata_traits<uint64_t>::toBytes(Object.size(), TBR);
    std::for_each(Object.begin(), Object.end(), [&](const auto& Elt) {
      auxdata_traits<typename T::value_type>::toBytes(Elt, TBR);
    });
  }

  static bool fromBytes(T& Object, FromByteRange& FBR) {
    uint64_t Count;
    if (!auxdata_traits<uint64_t>::fromBytes(Count, FBR))
      return false;

    if (Count > FBR.remainingBytesToRead())
      return false;

    Object.resize(Count);
    bool Success = true;
    std::for_each(Object.begin(), Object.end(), [&](auto& Elt) {
      if (!auxdata_traits<typename T::value_type>::fromBytes(Elt, FBR))
        Success = false;
    });

    return Success;
  }
};

template <class... Args> struct auxdata_traits<std::set<Args...>> {
  using T = std::set<Args...>;

  static std::string type_name() {
    return "set<" + TypeId<typename T::value_type>::value() + ">";
  }

  static void toBytes(const T& Object, ToByteRange& TBR) {
    auxdata_traits<uint64_t>::toBytes(Object.size(), TBR);
    for (const auto& Elt : Object)
      auxdata_traits<typename T::value_type>::toBytes(Elt, TBR);
  }

  static bool fromBytes(T& Object, FromByteRange& FBR) {
    uint64_t Count;
    if (!auxdata_traits<uint64_t>::fromBytes(Count, FBR))
      return false;

    if (Count > FBR.remainingBytesToRead())
      return false;

    for (uint64_t i = 0; i < Count; i++) {
      typename T::value_type V;
      if (!auxdata_traits<decltype(V)>::fromBytes(V, FBR))
        return false;
      Object.emplace(std::move(V));
    }

    return true;
  }
};

template <class T>
struct auxdata_traits<T, typename std::enable_if_t<is_mapping<T>::value>> {
  static std::string type_name() {
    return "mapping<" +
           TypeId<typename T::key_type, typename T::mapped_type>::value() + ">";
  }

  static void toBytes(const T& Object, ToByteRange& TBR) {
    auxdata_traits<uint64_t>::toBytes(Object.size(), TBR);
    std::for_each(Object.begin(), Object.end(), [&](const auto& Elt) {
      auxdata_traits<typename T::key_type>::toBytes(Elt.first, TBR);
      auxdata_traits<typename T::mapped_type>::toBytes(Elt.second, TBR);
    });
  }

  static bool fromBytes(T& Object, FromByteRange& FBR) {
    uint64_t Count;
    if (!auxdata_traits<uint64_t>::fromBytes(Count, FBR))
      return false;

    if (Count > FBR.remainingBytesToRead())
      return false;

    for (uint64_t i = 0; i < Count; i++) {
      typename T::key_type K;
      if (!auxdata_traits<decltype(K)>::fromBytes(K, FBR))
        return false;
      typename T::mapped_type V;
      if (!auxdata_traits<decltype(V)>::fromBytes(V, FBR))
        return false;
      Object.emplace(std::move(K), std::move(V));
    }
    return true;
  }
};

/// \brief std::variant support
///
/// Warning!
/// Members of the union (std::variant) should be default constructable.
template <class... Args> struct auxdata_traits<std::variant<Args...>> {
  using T = std::variant<Args...>;

  static std::string type_name() {
    return "variant<" + TypeId<Args...>::value() + ">";
  }

  template <uint64_t I = 0>
  static std::optional<std::variant<Args...>> expand_type(uint64_t i) {
    if constexpr (I >= sizeof...(Args)) {
      return std::nullopt;
    } else {
      return i == 0 ? std::variant<Args...>{std::in_place_index<I>,
                                            std::variant_alternative_t<I, T>{}}
                    : expand_type<I + 1>(i - 1);
    }
  };

  static void toBytes(const T& Object, ToByteRange& TBR) {
    uint64_t Index = Object.index();
    auxdata_traits<uint64_t>::toBytes(Index, TBR);
    std::visit(
        [TBR](auto&& arg) mutable {
          auxdata_traits<typename std::remove_const<
              typename std::remove_reference<decltype(arg)>::type>::type>::
              toBytes(arg, TBR);
        },
        Object);
  }

  static bool fromBytes(T& Object, FromByteRange& FBR) {
    uint64_t Index;
    if (!auxdata_traits<uint64_t>::fromBytes(Index, FBR))
      return false;

    if (Index > FBR.remainingBytesToRead())
      return false;

    auto maybeV = expand_type(Index);
    if (!maybeV) {
      return false;
    }
    auto V = *maybeV;
    bool res_code = false;
    std::visit(
        [&Object, &res_code, FBR](auto&& arg) mutable {
          typename std::remove_reference<decltype(arg)>::type Val;
          res_code = auxdata_traits<typename std::remove_reference<decltype(
              arg)>::type>::fromBytes(Val, FBR);
          if (!res_code)
            return;
          Object = Val;
        },
        V);
    if (!res_code)
      return false;
    return true;
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

template <class... Ts> struct tuple_traits<std::pair<Ts...>> {
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
  static void toBytes(const T& Object, ToByteRange& TBR) {
    static_for(
        [&](auto i) {
          const auto& F = std::get<i>(Object);
          auxdata_traits<std::remove_cv_t<
              std::remove_reference_t<decltype(F)>>>::toBytes(F, TBR);
        },
        std::make_index_sequence<std::tuple_size<T>::value>{});
  }

  static bool fromBytes(T& Object, FromByteRange& FBR) {
    bool Success = true;
    static_for(
        [&](auto i) {
          auto& F = std::get<i>(Object);
          if (!auxdata_traits<std::remove_cv_t<
                  std::remove_reference_t<decltype(F)>>>::fromBytes(F, FBR))
            Success = false;
        },
        std::make_index_sequence<std::tuple_size<T>::value>{});

    return Success;
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

protected:
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
  virtual void toProtobuf(MessageType* Message) const {
    toProtobuf(Message, this->SF);
  }

  // This version of protobuf accepts a SerializedForm object to
  // serialize rather than serializing AuxData's SerializedForm
  // member. This is used by AuxDataImpl to serialize a typed AuxData
  // object. We structure it this way to avoid having the AuxDataImpl
  // template (which is instantiated in client code) access
  // MessageType fields directly, which would introduce
  // dllimport/dllexport issues on Windows.
  void toProtobuf(MessageType* Message,
                  const SerializedForm& SFToSerialize) const;

  // Utility function for the AuxDataImpl template that allows us to
  // check the embedded type name in the serialized protobuf against an
  // expected typename without exposing the MessageType to clients.
  static bool checkAuxDataMessageType(const AuxData::MessageType& Message,
                                      const std::string& ExpectedName);

  // Present for testing purposes only.
  void save(std::ostream& Out) const;

  // Present for testing purposes only.
  static std::unique_ptr<AuxData>
  load(std::istream& In, std::unique_ptr<AuxData> (*FPPtr)(const MessageType&));

private:
  SerializedForm SF;

  friend class AuxDataContainer; // Friend to enable fromProtobuf.
  // Enables serialization by AuxDataContainer via containerToProtobuf.
  template <typename T> friend typename T::MessageType toProtobuf(const T&);
  friend class SerializationTestHarness; // Testing support.
};
/// @endcond

/// @cond INTERNAL
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

private:
  static std::unique_ptr<AuxData> fromProtobuf(const MessageType& Message) {
    // Check if the serialized type isn't compatible with the type
    // we're trying to deserialize to.
    if (!checkAuxDataMessageType(
            Message, auxdata_traits<typename Schema::Type>::type_name())) {
      return nullptr;
    }

    // Note: Do not access Message's contents here. That would introduce
    // dllexport/dllimport problems on Windows. Call the base class's
    // fromProtobuf function and then unserialize from its
    // SerializedForm structure.
    auto TypedAuxData = std::make_unique<AuxDataImpl<Schema>>();
    AuxData::fromProtobuf(*TypedAuxData, Message);
    FromByteRange FBR(TypedAuxData->rawData().RawBytes);
    if (!auxdata_traits<typename Schema::Type>::fromBytes(TypedAuxData->Object,
                                                          FBR))
      return nullptr;
    return TypedAuxData;
  }

  /// \brief Serialize into a protobuf message.
  ///
  /// \param Message     The Message to serialize into.
  virtual void toProtobuf(MessageType* Message) const override {
    // Note: Do not edit Message's contents here. That would introduce
    // dllexport/dllimport problems on Windows. Store to a
    // SerializedForm, and then call the base class's toProtobuf
    // function.
    AuxData::SerializedForm TypedSF;
    TypedSF.ProtobufType = auxdata_traits<typename Schema::Type>::type_name();
    ToByteRange TBR(TypedSF.RawBytes);
    auxdata_traits<typename Schema::Type>::toBytes(this->Object, TBR);
    AuxData::toProtobuf(Message, TypedSF);
  }

  // Present for testing purposes only.
  void save(std::ostream& Out) const { AuxData::save(Out); }

  // Present for testing purposes only.
  static std::unique_ptr<AuxDataImpl> load([[maybe_unused]] Context& C,
                                           std::istream& In) {
    return std::unique_ptr<AuxDataImpl>{
        static_cast<AuxDataImpl*>(AuxData::load(In, fromProtobuf).release())};
  }

  typename Schema::Type Object;

  friend class AuxDataContainer;         // Friend to enable to/fromProtobuf.
  friend class SerializationTestHarness; // Testing support.
};
/// @endcond

// (end \defgroup AUXDATA_GROUP)

} // namespace gtirb

#endif // GTIRB_AUXDATA_H
