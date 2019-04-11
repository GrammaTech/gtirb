//===- ByteMap.cpp ----------------------------------------------*- C++ -*-===//
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
#include "ByteMap.hpp"
#include "Serialization.hpp"
#include "gtirb/Context.hpp"
#include <proto/ByteMap.pb.h>
#include <algorithm>
#include <cstring>

using namespace gtirb;

bool ByteMap::willOverlapRegion(Addr A, size_t Bytes) const {
  // Look for a region that contains the address and see whether that region
  // can hold all of the data or not. If the region needs to be extended,
  // that's fine so long as the extension doesn't overlap into another region.
  Addr Limit = A + Bytes;
  for (size_t i = 0; i < Regions.size(); i++) {
    auto& Current = Regions[i];
    // If the current region can fully contain the data, we're good.
    if (containsAddr(Current, A) && Limit <= addressLimit(Current)) {
      return false;
    }

    // If the address is at the end of the current region, ensure that extending
    // into the next region doesn't cause overlap.
    if (A == addressLimit(Current)) {
      bool HasNext = i + 1 < Regions.size();
      return HasNext && Limit > Regions[i + 1].Address;
    }

    // We can extend into the previous region.
    if (Limit == Current.Address) {
      return false;
    }

    // Test to ensure there's not overlap with the current region.
    if (containsAddr(Current, A) || containsAddr(Current, Limit - 1)) {
      return true;
    }
  }

  // Not contiguous with any existing data.
  return false;
}

ByteMap::const_range ByteMap::data(Addr A, size_t Bytes) const {
  auto Reg = std::find_if(this->Regions.begin(), this->Regions.end(),
                          [A](const auto& R) { return containsAddr(R, A); });

  if (Reg == this->Regions.end() || A < Reg->Address ||
      (A + Bytes > addressLimit(*Reg))) {
    return ByteMap::const_range{};
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
