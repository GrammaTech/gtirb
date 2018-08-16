#include "ByteMap.hpp"
#include "Serialization.hpp"
#include <proto/ByteMap.pb.h>
#include <algorithm>
#include <cstring>

using namespace gtirb;

void ByteMap::setData(EA Ea, gsl::span<const std::byte> Bytes) {
  // Look for a region to hold this data. If necessary, extend or merge
  // existing regions to keep allocations contiguous.
  EA Limit = Ea + uint64_t(Bytes.size_bytes());
  for (size_t i = 0; Regions[i].Address != BadAddress; i++) {
    auto& Current = Regions[i];

    // Overwrite data in existing region
    if (containsEA(Current, Ea) && Limit <= addressLimit(Current)) {
      auto Offset = Ea - Current.Address;
      std::copy(Bytes.begin(), Bytes.end(), Current.Data.begin() + Offset);
      return;
    }

    // Extend region
    if (Ea == addressLimit(Current)) {
      auto& Next = Regions[i + 1];

      if (Limit > Next.Address) {
        throw std::invalid_argument(
            "Request to setData which overlaps an existing region.");
      }

      Current.Data.reserve(Current.Data.size() + Bytes.size());
      std::copy(Bytes.begin(), Bytes.end(), std::back_inserter(Current.Data));
      // Merge with subsequent region
      if (Limit == Next.Address) {
        const auto& Data = Next.Data;
        Current.Data.reserve(Current.Data.size() + Data.size());
        std::copy(Data.begin(), Data.end(), std::back_inserter(Current.Data));
        this->Regions.erase(this->Regions.begin() + i + 1);
      }
      return;
    }

    // Extend region backward
    if (Limit == Current.Address) {
      // Note: this is probably O(N^2), moving existing data on each inserted
      // element.
      std::copy(Bytes.begin(), Bytes.end(),
                std::inserter(Current.Data, Current.Data.begin()));
      Current.Address = Ea;
      return;
    }

    if (containsEA(Current, Ea) || containsEA(Current, Limit - uint64_t(1))) {
      throw std::invalid_argument("setData overlaps an existing region");
    }
  }

  // Not contiguous with any existing data. Create a new region.
  Region R = {Ea, std::vector<std::byte>()};
  R.Data.reserve(Bytes.size());
  std::copy(Bytes.begin(), Bytes.end(), std::back_inserter(R.Data));
  this->Regions.insert(std::lower_bound(this->Regions.begin(),
                                        this->Regions.end(), R,
                                        [](const auto& A, const auto& B) {
                                          return A.Address < B.Address;
                                        }),
                       std::move(R));
}

std::vector<std::byte> ByteMap::getData(EA Ea, size_t Bytes) const {
  auto Reg = std::find_if(this->Regions.begin(), this->Regions.end(),
                          [Ea](const auto& R) { return containsEA(R, Ea); });

  if (Reg == this->Regions.end() || Ea < Reg->Address ||
      (Ea + Bytes > addressLimit(*Reg))) {
    throw std::out_of_range("getData on unmapped address");
  }

  auto Begin = Reg->Data.begin() + (Ea - Reg->Address);
  return {Begin, Begin + Bytes};
}

namespace gtirb {
proto::Region toProtobuf(const ByteMap::Region& Region) {
  proto::Region Message;
  Message.set_address(Region.Address);
  std::transform(Region.Data.begin(), Region.Data.end(),
                 std::back_inserter(*Message.mutable_data()),
                 [](auto x) { return char(x); });
  return Message;
}

void fromProtobuf(ByteMap::Region& Val, const proto::Region& Message) {
  Val.Address = EA(Message.address());
  const auto& Data = Message.data();
  Val.Data.reserve(Data.size());
  std::transform(Data.begin(), Data.end(), std::back_inserter(Val.Data),
                 [](auto x) { return std::byte(x); });
}
} // namespace gtirb

void ByteMap::toProtobuf(MessageType* Message) const {
  containerToProtobuf(this->Regions, Message->mutable_regions());
}

void ByteMap::fromProtobuf(const MessageType& Message) {
  containerFromProtobuf(this->Regions, Message.regions());
}
