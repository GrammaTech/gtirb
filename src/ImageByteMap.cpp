#include "ImageByteMap.hpp"
#include "Serialization.hpp"
#include <proto/ImageByteMap.pb.h>

using namespace gtirb;

bool ImageByteMap::setAddrMinMax(std::pair<Addr, Addr> X) {
  if (X.first <= X.second) {
    this->EaMinMax = std::move(X);
    return true;
  }

  this->EaMinMax = std::make_pair(Addr{}, Addr{});
  return false;
}

void ImageByteMap::setData(Addr Ea, gsl::span<const std::byte> Data) {
  if (Ea >= this->EaMinMax.first &&
      (Ea + Data.size_bytes() - 1) <= this->EaMinMax.second) {
    this->BMap.setData(Ea, Data);
  } else {
    throw std::out_of_range(
        "Attempt to set data at an Addr out of range of the min and max Addr.");
  }
}

void ImageByteMap::setData(Addr Ea, size_t Bytes, std::byte Value) {
  auto Span = gsl::make_span(&Value, 1);
  for (uint64_t I = 0; I < Bytes; I++) {
    this->BMap.setData(Ea + I, Span);
  }
}

ImageByteMap::const_range ImageByteMap::data(Addr X, size_t Bytes) const {
  if (X >= this->EaMinMax.first && (X + Bytes - 1) <= this->EaMinMax.second) {
    return this->BMap.data(X, Bytes);
  }

  throw std::out_of_range(
      "Attempt to get data at an Addr out of range of the min and max Addr.");
}

void ImageByteMap::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  this->BMap.toProtobuf(Message->mutable_byte_map());
  Message->set_file_name(this->FileName);
  Message->set_addr_min(static_cast<uint64_t>(this->EaMinMax.first));
  Message->set_addr_max(static_cast<uint64_t>(this->EaMinMax.second));
  Message->set_base_address(static_cast<uint64_t>(this->BaseAddress));
  Message->set_entry_point_address(
      static_cast<uint64_t>(this->EntryPointAddress));
  Message->set_rebase_delta(this->RebaseDelta);
  Message->set_is_relocated(this->IsRelocated);
}

ImageByteMap *ImageByteMap::fromProtobuf(Context &C, const MessageType& Message) {
  auto *IBM = ImageByteMap::Create(C);
  setNodeUUIDFromBytes(IBM, Message.uuid());
  IBM->BMap.fromProtobuf(C, Message.byte_map());
  IBM->FileName = Message.file_name();
  IBM->EaMinMax = {Addr(Message.addr_min()), Addr(Message.addr_max())};
  IBM->BaseAddress = Addr(Message.base_address());
  IBM->EntryPointAddress = Addr(Message.entry_point_address());
  IBM->RebaseDelta = Message.rebase_delta();
  IBM->IsRelocated = Message.is_relocated();
  return IBM;
}
