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
#include <gsl/gsl>
#include <limits>
#include <map>
#include <vector>

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
  bool setData(Addr A, gsl::span<const std::byte> Data);

  /// \brief DOCFIXME
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
  const_range data(Addr A, size_t Bytes) const;

  /// \brief DOCFIXME
  using MessageType = proto::ByteMap;

  /// \brief DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief DOCFIXME
  ///
  /// \param C DOCFIXME
  /// \param Message DOCFIXME
  ///
  /// \return void
  void fromProtobuf(Context& C, const MessageType& Message);

  /// \brief DOCFIXME
  struct Region {
    Addr Address;                ///< DOCFIXME
    std::vector<std::byte> Data; ///< DOCFIXME

    /// \brief DOCFIXME
    ///
    /// \return DOCFIXME
    Addr getAddress() const { return this->Address; }

    /// \brief DOCFIXME
    ///
    /// \return DOCFIXME
    uint64_t getSize() const { return this->Data.size(); }
  };

private:
  std::vector<Region> Regions;
};
} // namespace gtirb

#endif // GTIRB_BYTEMAP_H
