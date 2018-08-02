#include "ByteMap.hpp"
#include "Serialization.hpp"
#include <proto/ByteMap.pb.h>
#include <algorithm>
#include <cstring>

using namespace gtirb;

void ByteMap::setData(EA ea, gsl::span<const gsl::byte> bytes) {
  // Look for a region to hold this data. If necessary, extend or merge
  // existing regions to keep allocations contiguous.
  EA limit = ea + uint64_t(bytes.size_bytes());
  for (size_t i = 0; regions[i].address != BadAddress; i++) {
    auto& current = regions[i];

    // Overwrite data in existing region
    if (containsEA(current, ea) && limit <= addressLimit(current)) {
      auto offset = ea - current.address;
      std::transform(bytes.begin(), bytes.end(), current.data.begin() + offset,
                     [](auto x) { return uint8_t(x); });
      return;
    }

    // Extend region
    if (ea == addressLimit(current)) {
      auto& next = regions[i + 1];

      if (limit > next.address) {
        throw std::invalid_argument("Request to setData which overlaps an existing region.");
      }

      current.data.reserve(current.data.size() + bytes.size());
      std::transform(bytes.begin(), bytes.end(), std::back_inserter(current.data),
                     [](auto x) { return uint8_t(x); });
      // Merge with subsequent region
      if (limit == next.address) {
        const auto& data = next.data;
        current.data.reserve(current.data.size() + data.size());
        std::copy(data.begin(), data.end(), std::back_inserter(current.data));
        this->regions.erase(this->regions.begin() + i + 1);
      }
      return;
    }

    // Extend region backward
    if (limit == current.address) {
      // Note: this is probably O(N^2), moving existing data on each inserted
      // element.
      std::transform(bytes.begin(), bytes.end(), std::inserter(current.data, current.data.begin()),
                     [](auto x) { return uint8_t(x); });
      current.address = ea;
      return;
    }

    if (containsEA(current, ea) || containsEA(current, limit - uint64_t(1))) {
      throw std::invalid_argument("setData overlaps an existing region");
    }
  }

  // Not contiguous with any existing data. Create a new region.
  Region region = {ea, std::vector<uint8_t>()};
  region.data.reserve(bytes.size());
  std::transform(bytes.begin(), bytes.end(), std::back_inserter(region.data),
                 [](auto x) { return uint8_t(x); });
  this->regions.insert(this->regions.end() - 1, std::move(region));
}

std::vector<uint8_t> ByteMap::getData(EA ea, size_t bytes) const {
  std::vector<uint8_t> buffer(bytes, uint8_t{0});

  auto region = std::find_if(this->regions.begin(), this->regions.end(),
                             [ea](const auto& r) { return containsEA(r, ea); });

  if (region == this->regions.end() || ea < region->address ||
      (ea + bytes > addressLimit(*region))) {
    throw std::out_of_range("getData on unmapped address");
  }

  auto begin = region->data.begin() + (ea - region->address);
  std::copy(begin, begin + bytes, buffer.begin());

  return buffer;
}

namespace gtirb {
proto::Region toProtobuf(const ByteMap::Region& region) {
  proto::Region message;
  message.set_address(region.address);
  message.set_data(std::string(region.data.begin(), region.data.end()));
  return message;
}

void fromProtobuf(ByteMap::Region& val, const proto::Region& message) {
  val.address = EA(message.address());
  const auto& data = message.data();
  val.data.reserve(data.size());
  std::copy(data.begin(), data.end(), std::back_inserter(val.data));
}
} // namespace gtirb

void ByteMap::toProtobuf(MessageType* message) const {
  containerToProtobuf(this->regions, message->mutable_regions());
}

void ByteMap::fromProtobuf(const MessageType& message) {
  containerFromProtobuf(this->regions, message.regions());
}
