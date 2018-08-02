#pragma once

#include <gtirb/EA.hpp>
#include <array>
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
  void setData(EA ea, gsl::span<const gsl::byte> data);

  ///
  /// Get data at the given address.
  ///
  /// \param  ea       The starting address for the data.
  /// \param  bytes   The number of bytes to read.
  ///
  std::vector<gsl::byte> getData(EA ea, size_t bytes) const;

  using MessageType = proto::ByteMap;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);

  struct Region {
    EA address;
    std::vector<gsl::byte> data;

    EA getAddress() const { return this->address; }

    uint64_t getSize() const { return this->data.size(); }
  };

private:
  // Initialize with sentinel region
  std::vector<Region> regions{{EA(), std::vector<gsl::byte>()}};
};
} // namespace gtirb
