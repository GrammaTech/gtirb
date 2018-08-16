#pragma once

#include <gtirb/EA.hpp>
#include <array>
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
///
/// \class ByteMap
///
/// Holds the bytes of the loaded image of the binary.
///
class GTIRB_EXPORT_API ByteMap {
public:
  ///
  /// Sets byte map at the given address.
  ///
  /// \param  ea      The address to store the data.
  /// \param  data    The data to store.
  ///
  void setData(EA Ea, gsl::span<const std::byte> Data);

  ///
  /// Get data at the given address.
  ///
  /// \param  ea       The starting address for the data.
  /// \param  bytes   The number of bytes to read.
  ///
  std::vector<std::byte> getData(EA Ea, size_t Bytes) const;

  using MessageType = proto::ByteMap;
  void toProtobuf(MessageType* Message) const;
  void fromProtobuf(const MessageType& Message);

  struct Region {
    EA Address;
    std::vector<std::byte> Data;

    EA getAddress() const { return this->Address; }

    uint64_t getSize() const { return this->Data.size(); }
  };

private:
  // Initialize with sentinel region
  std::vector<Region> Regions{{EA(), std::vector<std::byte>()}};
};
} // namespace gtirb
