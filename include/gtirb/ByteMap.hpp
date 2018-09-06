#ifndef GTIRB_BYTEMAP_H
#define GTIRB_BYTEMAP_H

#include <gtirb/Addr.hpp>
#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <boost/range/iterator_range.hpp>
#include <gsl/gsl>
#include <limits>
#include <map>
#include <vector>

namespace proto {
class ByteMap;
}

namespace gtirb {
class Context;

///
/// \class ByteMap
///
/// \brief Holds the bytes of the loaded image of the binary.
///
class GTIRB_EXPORT_API ByteMap {
public:

  /// \brief Set the byte map at the specified address.
  ///
  /// \param  A       The address to store the data.
  /// \param  Data    The data to store.
  ///
  /// \return  void
  ///
  void setData(Addr A, gsl::span<const std::byte> Data);


  using const_range =
    boost::iterator_range<std::vector<std::byte>::const_iterator>;
  /// \brief Get the data at the specified address.
  ///
  /// \param  A       The starting address for the data.
  /// \param  Bytes   The number of bytes to read.
  ///
  /// \return DOCFIXME
  ///
  /// The iterator range returned encodes a contiguous block of memory that can
  /// be accessed directly, such as via memcpy().
  const_range data(Addr A, size_t Bytes) const;


  /// \brief DOCFIXME
  using MessageType = proto::ByteMap;


  /// \brief DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return void
  ///
  void toProtobuf(MessageType* Message) const;


  /// \brief DOCFIXME
  ///
  /// \param C DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return void
  ///
  void fromProtobuf(Context &C, const MessageType& Message);


  /// \brief DOCFIXME
  struct Region {
    Addr Address;                  ///< DOCFIXME
    std::vector<std::byte> Data;   ///< DOCFIXME

    /// \brief DOCFIXME
    ///
    /// \return DOCFIXME
    ///
    Addr getAddress() const { return this->Address; }

    /// \brief DOCFIXME
    ///
    /// \return DOCFIXME
    ///
    uint64_t getSize() const { return this->Data.size(); }
  };

private:
  // Initialize with sentinel region
  std::vector<Region> Regions;
};
} // namespace gtirb

#endif // GTIRB_BYTEMAP_H
