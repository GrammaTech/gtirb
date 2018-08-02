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
      std::copy(bytes.begin(), bytes.end(), current.data.begin() + offset);
      return;
    }

    // Extend region
    if (ea == addressLimit(current)) {
      auto& next = regions[i + 1];

      if (limit > next.address) {
        throw std::invalid_argument("Request to setData which overlaps an existing region.");
      }

      current.data.reserve(current.data.size() + bytes.size());
      std::copy(bytes.begin(), bytes.end(), std::back_inserter(current.data));
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
      std::copy(bytes.begin(), bytes.end(), std::inserter(current.data, current.data.begin()));
      current.address = ea;
      return;
    }

    if (containsEA(current, ea) || containsEA(current, limit - uint64_t(1))) {
      throw std::invalid_argument("setData overlaps an existing region");
    }
  }

  // Not contiguous with any existing data. Create a new region.
  Region region = {ea, std::vector<gsl::byte>()};
  region.data.reserve(bytes.size());
  std::copy(bytes.begin(), bytes.end(), std::back_inserter(region.data));
  this->regions.insert(this->regions.end() - 1, std::move(region));
}

std::vector<gsl::byte> ByteMap::getData(EA ea, size_t bytes) const {
  auto region = std::find_if(this->regions.begin(), this->regions.end(),
                             [ea](const auto& r) { return containsEA(r, ea); });

  if (region == this->regions.end() || ea < region->address ||
      (ea + bytes > addressLimit(*region))) {
    throw std::out_of_range("getData on unmapped address");
  }

  auto begin = region->data.begin() + (ea - region->address);
  return {begin, begin + bytes};
}

namespace gtirb {
proto::Region toProtobuf(const ByteMap::Region& region) {
  proto::Region message;
  message.set_address(region.address);
  std::transform(region.data.begin(), region.data.end(),
                 std::back_inserter(*message.mutable_data()), [](auto x) { return char(x); });
  return message;
}

void fromProtobuf(ByteMap::Region& val, const proto::Region& message) {
  val.address = EA(message.address());
  const auto& data = message.data();
  val.data.reserve(data.size());
  std::transform(data.begin(), data.end(), std::back_inserter(val.data),
                 [](auto x) { return gsl::byte(x); });
}
} // namespace gtirb

void ByteMap::toProtobuf(MessageType* message) const {
  containerToProtobuf(this->regions, message->mutable_regions());
}

void ByteMap::fromProtobuf(const MessageType& message) {
  containerFromProtobuf(this->regions, message.regions());
}
