#include "ByteMap.hpp"
#include "Serialization.hpp"
#include "gtirb/Context.hpp"
#include <proto/ByteMap.pb.h>
#include <algorithm>
#include <cstring>

using namespace gtirb;

void ByteMap::setData(Addr A, gsl::span<const std::byte> Bytes) {
  // Look for a region to hold this data. If necessary, extend or merge
  // existing regions to keep allocations contiguous.
  Addr Limit = A + uint64_t(Bytes.size_bytes());
  for (size_t i = 0; i < Regions.size(); i++) {
    auto& Current = Regions[i];

    // Overwrite data in existing region
    if (containsAddr(Current, A) && Limit <= addressLimit(Current)) {
      auto Offset = A - Current.Address;
      std::copy(Bytes.begin(), Bytes.end(), Current.Data.begin() + Offset);
      return;
    }

    // Extend region
    if (A == addressLimit(Current)) {
      bool HasNext = i + 1 < Regions.size();
      if (HasNext && Limit > Regions[i + 1].Address) {
        throw std::invalid_argument(
            "Request to setData which overlaps an existing region.");
      }

      Current.Data.reserve(Current.Data.size() + Bytes.size());
      std::copy(Bytes.begin(), Bytes.end(), std::back_inserter(Current.Data));
      // Merge with subsequent region
      if (HasNext && Limit == Regions[i + 1].Address) {
        const auto& Data = Regions[i + 1].Data;
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
      Current.Address = A;
      return;
    }

    if (containsAddr(Current, A) || containsAddr(Current, Limit - 1)) {
      throw std::invalid_argument("setData overlaps an existing region");
    }
  }

  // Not contiguous with any existing data. Create a new region.
  Region R = {A, std::vector<std::byte>()};
  R.Data.reserve(Bytes.size());
  std::copy(Bytes.begin(), Bytes.end(), std::back_inserter(R.Data));
  this->Regions.insert(
      std::lower_bound(this->Regions.begin(), this->Regions.end(), R,
                       [](const auto& Left, const auto& Right) {
                         return Left.Address < Right.Address;
                       }),
      std::move(R));
}

ByteMap::const_range ByteMap::data(Addr A, size_t Bytes) const {
  auto Reg = std::find_if(this->Regions.begin(), this->Regions.end(),
                          [A](const auto& R) { return containsAddr(R, A); });

  if (Reg == this->Regions.end() || A < Reg->Address ||
      (A + Bytes > addressLimit(*Reg))) {
    throw std::out_of_range("getData on unmapped address");
  }

  auto Begin = Reg->Data.begin() + (A - Reg->Address);
  return {Begin, Begin + Bytes};
}

namespace gtirb {
proto::Region toProtobuf(const ByteMap::Region& R) {
  proto::Region Message;
  Message.set_address(static_cast<uint64_t>(R.Address));
  std::transform(R.Data.begin(), R.Data.end(),
                 std::back_inserter(*Message.mutable_data()),
                 [](auto x) { return char(x); });
  return Message;
}

void fromProtobuf(Context&, ByteMap::Region& Val,
                  const proto::Region& Message) {
  Val.Address = Addr(Message.address());
  const auto& Data = Message.data();
  Val.Data.reserve(Data.size());
  std::transform(Data.begin(), Data.end(), std::back_inserter(Val.Data),
                 [](auto x) { return std::byte(x); });
}
} // namespace gtirb

void ByteMap::toProtobuf(MessageType* Message) const {
  containerToProtobuf(this->Regions, Message->mutable_regions());
}

void ByteMap::fromProtobuf(Context& C, const MessageType& Message) {
  containerFromProtobuf(C, this->Regions, Message.regions());
}
