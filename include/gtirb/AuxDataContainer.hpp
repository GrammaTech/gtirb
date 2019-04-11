//===- AuxDataContainer.hpp -------------------------------------*- C++ -*-===//
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

#ifndef GTIRB_AUXDATACONTAINER_H
#define GTIRB_AUXDATACONTAINER_H

#include <gtirb/AuxData.hpp>
#include <gtirb/Node.hpp>

#include <proto/AuxDataContainer.pb.h>

/// \file AuxDataContainer.hpp
/// \brief Class gtirb::AuxDataContainer.

namespace proto {
class AuxDataContainer;
}

namespace gtirb {

/// \class AuxDataContainer
///
/// \brief Contains the AuxData Tables and serves as a base class.

class GTIRB_EXPORT_API AuxDataContainer : public Node {
  using AuxDataSet = std::map<std::string, gtirb::AuxData>;

public:
  /// \name AuxData Properties
  /// @{

  /// \brief Add a new \ref AuxData, transferring ownership.
  ///
  /// \param Name     The name to assign to the data so it can be found later.
  /// \param X        The data itself.
  ///
  /// \return void
  ///
  void addAuxData(const std::string& Name, AuxData&& X);

  /// \brief Get a reference to the underlying type stored in the \ref
  ///        AuxData by name.
  ///
  /// \param  X   The name of the data to search for.
  ///
  /// \return     A non-owning pointer to the data if found,
  ///             \c nullptr otherwise.
  ///
  template <typename T> const T* getAuxData(const std::string& X) const {
    const gtirb::AuxData* auxData = getAuxData(X);

    return auxData ? auxData->get<T>() : nullptr;
  }

  /// \brief Get an \ref AuxData by name.
  ///
  /// \param  X   The name of the data to search for.
  ///
  /// \return     A non-owning pointer to the data if found,
  ///             \c nullptr otherwise.
  ///
  const gtirb::AuxData* getAuxData(const std::string& X) const;

  /// \brief Get an \ref AuxData by name.
  ///
  /// \param  X   The name of the data to search for.
  ///
  /// \return     A non-owning pointer to the data if found,
  ///             \c nullptr otherwise.
  ///
  gtirb::AuxData* getAuxData(const std::string& X);

  /// \brief Get a reference to the underlying type stored in the \ref
  ///        AuxData by name.
  ///
  /// \param  X   The name of the data to search for.
  ///
  /// \return     A non-owning pointer to the data if found,
  ///             \c nullptr otherwise.
  ///
  template <typename T> T* getAuxData(const std::string& X) {
    gtirb::AuxData* auxData = getAuxData(X);

    return auxData ? auxData->get<T>() : nullptr;
  }
  /// \brief Remove an \ref AuxData by name.
  ///
  /// This will invalidate any pointers that may have been held externally.
  ///
  /// \param  X   The name of the data to search for.
  /// \return     \c true on success, \c false otherwise.
  ///
  bool removeAuxData(const std::string& X);

  using aux_data_iterator = AuxDataSet::iterator;
  using aux_data_range = boost::iterator_range<aux_data_iterator>;
  using const_aux_data_iterator = AuxDataSet::const_iterator;
  using const_aux_data_range = boost::iterator_range<const_aux_data_iterator>;

  /// \brief Return an iterator to the first AuxData.
  aux_data_iterator aux_data_begin() { return AuxDatas.begin(); }
  /// \brief Return a constant iterator to the first AuxData.
  const_aux_data_iterator aux_data_begin() const { return AuxDatas.begin(); }
  /// \brief Return an iterator to the element following the last AuxData.
  aux_data_iterator aux_data_end() { return AuxDatas.end(); }
  /// \brief Return a constant iterator to the element following the last
  /// AuxData.
  const_aux_data_iterator aux_data_end() const { return AuxDatas.end(); }
  /// \brief Return a range of the auxiliary data (\ref AuxData).
  aux_data_range aux_data() {
    return boost::make_iterator_range(aux_data_begin(), aux_data_end());
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

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing IR.
  using MessageType = proto::AuxDataContainer;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a IR from a protobuf message.
  ///
  /// \param C   The Context in which the deserialized IR will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized IR object, or null on failure.
  static void fromProtobuf(AuxDataContainer* in, Context& C,
                           const MessageType& Message);
  /// @endcond
protected:
  /// \cond INTERNAL
  AuxDataContainer(Context& C, Kind knd) : Node(C, knd) {}
private:
  AuxDataSet AuxDatas;
  /// @}
};
} // namespace gtirb
#endif // GTIRB_AUXDATACONTAINER_H
