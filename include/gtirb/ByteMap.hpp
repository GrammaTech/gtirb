//===- ByteMap.hpp ----------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_BYTEMAP_H
#define GTIRB_BYTEMAP_H

#include <gtirb/Addr.hpp>
#include <array>
#include <boost/range/iterator_range.hpp>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <map>
#include <vector>

/// \file ByteMap.hpp
/// \brief Class gtirb::ByteMap.

namespace proto {
class ByteMap;
}

namespace gtirb {
class Context;
class ImageByteMap;

/// \class ByteMap
///
/// \brief Holds the bytes of the loaded image of the binary.
class GTIRB_EXPORT_API ByteMap {
  /// \copybrief gtirb::ImageByteMap
  friend class ImageByteMap;
  bool willOverlapRegion(Addr A, size_t Bytes) const;

public:
  /// \brief Set the byte map at the specified address.
  ///
  /// \param  A       The address to store the data.
  /// \param  Data    The data to store; can be an empty range of data.
  ///
  /// \return  Will return \c true if the data can be assigned at the given
  /// Address, or \c false otherwise. The data passed in at the given address
  /// cannot overlap another memory region (overlays are not supported).
  template <class It, typename = std::enable_if_t<std::is_convertible_v<
                          decltype(*std::declval<It>()), std::byte>>>
  bool setData(Addr A, boost::iterator_range<It> Data) {
    // Look for a region to hold this data. If necessary, extend or merge
    // existing regions to keep allocations contiguous.
    auto data_size = std::distance(Data.begin(), Data.end());
    Addr Limit = A + data_size;
    for (size_t i = 0; i < Regions.size(); i++) {
      auto& Current = Regions[i];

      // Overwrite data in existing region
      if (containsAddr(Current, A) && Limit <= addressLimit(Current)) {
        auto Offset = A - Current.Address;
        std::copy(Data.begin(), Data.end(), Current.Data.begin() + Offset);
        return true;
      }

      // Extend region
      if (A == addressLimit(Current)) {
        bool HasNext = i + 1 < Regions.size();
        if (HasNext && Limit > Regions[i + 1].Address) {
          return false;
        }

        Current.Data.reserve(Current.Data.size() + data_size);
        std::copy(Data.begin(), Data.end(), std::back_inserter(Current.Data));
        // Merge with subsequent region
        if (HasNext && Limit == Regions[i + 1].Address) {
          const auto& D = Regions[i + 1].Data;
          Current.Data.reserve(Current.Data.size() + D.size());
          std::copy(D.begin(), D.end(), std::back_inserter(Current.Data));
          this->Regions.erase(this->Regions.begin() + i + 1);
        }
        return true;
      }

      // Extend region backward
      if (Limit == Current.Address) {
        // Note: this is probably O(N^2), moving existing data on each inserted
        // element.
        std::copy(Data.begin(), Data.end(),
                  std::inserter(Current.Data, Current.Data.begin()));
        Current.Address = A;
        return true;
      }

      if (containsAddr(Current, A) || containsAddr(Current, Limit - 1)) {
        return false;
      }
    }

    // Not contiguous with any existing data. Create a new region.
    Region R = {A, std::vector<std::byte>()};
    R.Data.reserve(data_size);
    std::copy(Data.begin(), Data.end(), std::back_inserter(R.Data));
    this->Regions.insert(
        std::lower_bound(this->Regions.begin(), this->Regions.end(), R,
                         [](const auto& Left, const auto& Right) {
                           return Left.Address < Right.Address;
                         }),
        std::move(R));

    return true;
  }

  /// \brief A constant range of bytes.
  using const_range =
      boost::iterator_range<std::vector<std::byte>::const_iterator>;

  /// \brief Get the data at the specified address.
  ///
  /// \param  A       The starting address for the data.
  /// \param  Bytes   The number of bytes to read.
  ///
  /// \return An iterator range that encodes a contiguous block of memory that
  /// can be accessed directly, such as via memcpy(). Will return an empty
  /// range if the requested address or number of bytes cannot be retrieved.
  ///
  /// For an interator over the contents of a \ref Block, use
  /// Block::getAddress() and Block::getSize() to obtain the arguments
  /// to this method.
  const_range data(Addr A, size_t Bytes) const;

  /// \brief The protobuf message type used for serializing ByteMap.
  using MessageType = proto::ByteMap;

  /// @cond INTERNAL
  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a ByteMap from a protobuf message.
  ///
  /// \param C   The Context in which the deserialized ByteMap will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized ByteMap object, or null on failure.
  void fromProtobuf(Context& C, const MessageType& Message);

  struct Region {
    Addr Address;
    std::vector<std::byte> Data;

    Addr getAddress() const { return this->Address; }

    uint64_t getSize() const { return this->Data.size(); }
  };
  /// @endcond

private:
  std::vector<Region> Regions;
};
} // namespace gtirb

#endif // GTIRB_BYTEMAP_H
