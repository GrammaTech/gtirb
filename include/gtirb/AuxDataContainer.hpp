//===- AuxDataContainer.hpp -------------------------------------*- C++ -*-===//
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

#ifndef GTIRB_AUXDATACONTAINER_H
#define GTIRB_AUXDATACONTAINER_H

#include <gtirb/AuxData.hpp>
#include <gtirb/Node.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/range/iterator_range.hpp>
#include <type_traits>

/// \file AuxDataContainer.hpp
/// \brief Class gtirb::AuxDataContainer.

namespace gtirb {

namespace proto {
class IR;
class Module;
} // namespace proto

/// @cond INTERNAL
template <typename MessageType> struct message_has_aux_data_container {
  static constexpr bool value = false;
};
template <> struct message_has_aux_data_container<proto::IR> {
  static constexpr bool value = true;
};
template <> struct message_has_aux_data_container<proto::Module> {
  static constexpr bool value = true;
};

/// \brief True if MessageType is a message that holds a \ref AuxDataContainer.
template <typename MessageType>
inline constexpr bool message_has_aux_data_container_v =
    message_has_aux_data_container<MessageType>::value;
/// @endcond

/// \class AuxDataContainer
///
/// \brief Contains the AuxData Tables and serves as a base class.

class GTIRB_EXPORT_API AuxDataContainer : public Node {
  using AuxDataSet = std::map<std::string, std::unique_ptr<gtirb::AuxData>>;

public:
  /// \name AuxData Properties
  /// @{

  /// \brief Register a type to be used with AuxData of the given name.
  template <typename Schema> static void registerAuxDataType() {
    registerAuxDataTypeInternal(Schema::Name,
                                std::make_unique<AuxDataTypeImpl<Schema>>());
  }

  /// \brief Add a new \ref AuxData, transferring ownership.
  ///
  /// \param X        The data itself.
  ///
  /// \return void
  ///
  template <typename Schema> void addAuxData(typename Schema::Type&& X) {
    // Make sure this type matches a registered type.
    checkAuxDataRegistration(Schema::Name,
                             AuxDataImpl<Schema>::staticGetApiTypeId());
    this->AuxDatas[Schema::Name] =
        std::make_unique<AuxDataImpl<Schema>>(std::move(X));
  }

  /// \brief Get a reference to the underlying type stored in the \ref
  ///        AuxData by name.
  ///
  /// \return     A non-owning pointer to the data if found,
  ///             \c nullptr otherwise.
  ///
  /// Note that this function can only be used for AuxData for which a
  /// type has been registered with registerAuxDataType().
  template <typename Schema> typename Schema::Type* getAuxData() {
    return const_cast<typename Schema::Type*>(
        const_cast<const AuxDataContainer*>(this)->getAuxData<Schema>());
  }

  /// \brief Get a reference to the underlying type stored in the \ref
  ///        AuxData by name.
  ///
  /// \return     A non-owning pointer to the data if found,
  ///             \c nullptr otherwise.
  ///
  /// Note that this function can only be used for AuxData for which a
  /// type has been registered with registerAuxDataType().
  template <typename Schema> const typename Schema::Type* getAuxData() const {
    auto Found = this->AuxDatas.find(Schema::Name);

    if (Found == this->AuxDatas.end())
      return nullptr;

    AuxData& AD = *(Found->second);

    // Is this type registered?
    if (AD.getApiTypeId() == AuxData::UNREGISTERED_API_TYPE_ID) {
      assert(false &&
             "Attempting to retrieve AuxData with an unregistered type.");
      return nullptr;
    }

    // Does the type match the type being requested?
    if (AD.getApiTypeId() != AuxDataImpl<Schema>::staticGetApiTypeId()) {
      assert(false && "Attempting to retrieve AuxData with incorrect type.");
      return nullptr;
    }

    // If we get here, it should be safe to downcast to the typed AuxDataImpl.
    auto& ADI = static_cast<AuxDataImpl<Schema>&>(AD);
    return ADI.get();
  }

  /// \brief Remove an \ref AuxData by name.
  ///
  /// This will invalidate any pointers that may have been held externally.
  ///
  /// \return     \c true on success, \c false otherwise.
  ///
  /// Note that this function can only be used for AuxData for which a
  /// type has been registered with registerAuxDataType().
  template <typename Schema> bool removeAuxData() {
    return this->AuxDatas.erase(Schema::Name) > 0;
  }

  /// \brief An interface for accessing the serialized form of an AuxData
  /// instance.
  ///
  /// The \ref const_aux_data_iterator provides this value type for
  /// the AuxData instances contained in this container. This provides
  /// access to all AuxData present in a container regardless of
  /// whether or not appropriate types have been registered.
  ///
  /// The content provided through this interface is only ever
  /// populated at the point at which this container is
  /// unserialized. Edits to AuxData after this point (or newly added
  /// AuxData) will not be reflected through this interface.
  struct AuxDataRaw {

    /// \brief The string name of the AuxData field.
    const std::string& Key;

    /// \brief The raw bytes of the serialized form of the AuxData
    const std::string& RawBytes;

    /// \brief The type of the AuxData as stored in protobuf.
    ///
    /// Note that this is not the AuxData's C++ type.
    const std::string& ProtobufType;

    AuxDataRaw(const std::string& K, const std::string& RB,
               const std::string& PT)
        : Key(K), RawBytes(RB), ProtobufType(PT) {}
  };

private:
  struct AccessRawData {
    auto operator()(const AuxDataSet::value_type& P) const {
      return AuxDataRaw(P.first, P.second->rawData().RawBytes,
                        P.second->rawData().ProtobufType);
    }
  };

public:
  /// \brief An iterator type for traversing the AuxData in this container.
  ///
  /// The value type for this iterator is AuxDataRaw. The iterator
  /// provides access to the serialized form of all AuxData in this
  /// container (regardless of whether or not the AuxData's type is
  /// registered.) Note that the content is only valid with respect to
  /// the most recent unserialization operation. Entries for new or
  /// modified AuxData entities will contain empty or stale content.
  using const_aux_data_iterator =
      boost::transform_iterator<AccessRawData, AuxDataSet::const_iterator>;
  using const_aux_data_range = boost::iterator_range<const_aux_data_iterator>;

  /// \brief Return a constant iterator to the first AuxData.
  const_aux_data_iterator aux_data_begin() const {
    return const_aux_data_iterator(AuxDatas.begin(), AccessRawData());
  }

  /// \brief Return a constant iterator to the element following the last
  /// AuxData.
  const_aux_data_iterator aux_data_end() const {
    return const_aux_data_iterator(AuxDatas.end(), AccessRawData());
  }

  /// \brief Return a constant range of the auxiliary data (\ref AuxData).
  const_aux_data_range aux_data() const {
    return boost::make_iterator_range(aux_data_begin(), aux_data_end());
  }

  /// \brief Get the total number of \ref AuxData objects in this IR.
  ///
  /// \return     The total number of \ref AuxData objects.
  ///
  size_t getAuxDataSize() const { return AuxDatas.size(); }

  /// \brief Check: Is the number of \ref AuxData objects in this IR zero?
  ///
  /// \return \c true if this IR does not contain any \ref AuxData, otherwise \c
  /// false
  ///
  bool getAuxDataEmpty() const { return AuxDatas.empty(); }

  /// \brief Clear all \ref AuxData from the IR.
  ///
  /// \return void
  ///
  void clearAuxData() { AuxDatas.clear(); }

  /// @}
  /// @cond INTERNAL

  /// \brief Serialize the aux data into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  template <
      class MessageType,
      class = std::enable_if_t<message_has_aux_data_container_v<MessageType>>>
  void toProtobuf(MessageType* Message) const {
    containerToProtobuf(this->AuxDatas, Message->mutable_aux_data());
  }

  /// \brief Load the aux data from a protobuf message.
  ///
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return void
  template <
      class MessageType,
      class = std::enable_if_t<message_has_aux_data_container_v<MessageType>>>
  void fromProtobuf(const MessageType& Message) {
    this->AuxDatas.clear();
    for (const auto& M : Message.aux_data()) {
      std::unique_ptr<AuxData> Val;
      std::string Key = M.first;

      // See if the name for this AuxData is registered.
      if (const auto* ADT = lookupAuxDataType(Key)) {
        Val = ADT->fromProtobuf(M.second);
      }

      // If it wasn't registered or was registered with an incompatible
      // type wrt the serialized object, unserialized as just the
      // un-typed raw data.
      if (!Val) {
        Val = std::make_unique<AuxData>();
        AuxData::fromProtobuf(*Val, M.second);
      }
      this->AuxDatas.insert(std::make_pair(Key, std::move(Val)));
    }
    /// @endcond
  }

protected:
  AuxDataContainer(Context& C, Kind knd);
  AuxDataContainer(Context& C, Kind knd, const UUID& U);

private:
  AuxDataSet AuxDatas;

  struct AuxDataType {
    virtual ~AuxDataType() = default;
    virtual std::unique_ptr<AuxData>
    fromProtobuf(const proto::AuxData& Message) const = 0;
    virtual std::size_t getApiTypeId() const = 0;
  };

  template <typename Schema> struct AuxDataTypeImpl : public AuxDataType {
    std::unique_ptr<AuxData>
    fromProtobuf(const proto::AuxData& Message) const override {
      return AuxDataImpl<Schema>::fromProtobuf(Message);
    }

    std::size_t getApiTypeId() const override {
      return AuxDataImpl<Schema>::staticGetApiTypeId();
    }
  };

  static void registerAuxDataTypeInternal(const char* Name,
                                          std::unique_ptr<AuxDataType> ADT);
  static void checkAuxDataRegistration(const char* Name, std::size_t Id);
  static const AuxDataType* lookupAuxDataType(const std::string& Name);
  friend struct AuxDataTypeMap; // Allows AuxDataTypeMap to use AuxDataType
};
} // namespace gtirb
#endif // GTIRB_AUXDATACONTAINER_H
