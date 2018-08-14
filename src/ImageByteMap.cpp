#include "ImageByteMap.hpp"
#include "Serialization.hpp"
#include <proto/ImageByteMap.pb.h>

using namespace gtirb;

void ImageByteMap::setFileName(boost::filesystem::path X) {
  this->FileName = X;
}

boost::filesystem::path ImageByteMap::getFileName() const {
  return this->FileName;
}

void ImageByteMap::setBaseAddress(EA X) { this->BaseAddress = X; }

EA ImageByteMap::getBaseAddress() const { return this->BaseAddress; }

void ImageByteMap::setEntryPointAddress(EA X) { this->EntryPointAddress = X; }

EA ImageByteMap::getEntryPointAddress() const {
  return this->EntryPointAddress;
}

bool ImageByteMap::setEAMinMax(std::pair<gtirb::EA, gtirb::EA> X) {
  if (X.first <= X.second) {
    this->EaMinMax = std::move(X);
    return true;
  }

  this->EaMinMax = std::pair<gtirb::EA, gtirb::EA>(gtirb::EA{}, gtirb::EA{});
  return false;
}

std::pair<gtirb::EA, gtirb::EA> ImageByteMap::getEAMinMax() const {
  return this->EaMinMax;
}

void ImageByteMap::setRebaseDelta(int64_t X) { this->RebaseDelta = X; }

int64_t ImageByteMap::getRebaseDelta() const { return this->RebaseDelta; }

void ImageByteMap::setIsRelocated() { this->IsRelocated = true; }

bool ImageByteMap::getIsRelocated() const { return this->IsRelocated; }

boost::endian::order ImageByteMap::getByteOrder() const {
  return this->ByteOrder;
}

void ImageByteMap::setByteOrder(boost::endian::order Value) {
  this->ByteOrder = Value;
}

void ImageByteMap::setData(EA Ea, gsl::span<const gsl::byte> Data) {
  if (Ea >= this->EaMinMax.first &&
      (Ea + EA{(uint64_t)Data.size_bytes()} - EA{1}) <= this->EaMinMax.second) {
    this->ByteMap.setData(Ea, Data);
  } else {
    throw std::out_of_range(
        "Attempt to set data at an EA out of range of the min and max EA.");
  }
}

void ImageByteMap::setData(EA Ea, size_t Bytes, gsl::byte Value) {
  auto Span = gsl::make_span(&Value, 1);
  for (uint64_t I = 0; I < Bytes; I++) {
    this->ByteMap.setData(Ea + I, Span);
  }
}

std::vector<gsl::byte> ImageByteMap::getData(EA X, size_t Bytes) const {
  if (X >= this->EaMinMax.first &&
      (X + EA{Bytes} - EA{1}) <= this->EaMinMax.second) {
    return this->ByteMap.getData(X, Bytes);
  }

  throw std::out_of_range(
      "Attempt to get data at an EA out of range of the min and max EA.");
}

void ImageByteMap::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  this->ByteMap.toProtobuf(Message->mutable_byte_map());
  Message->set_file_name(this->FileName.generic_string());
  Message->set_ea_min(this->EaMinMax.first);
  Message->set_ea_max(this->EaMinMax.second);
  Message->set_base_address(this->BaseAddress);
  Message->set_entry_point_address(this->EntryPointAddress);
  Message->set_rebase_delta(this->RebaseDelta);
  Message->set_is_relocated(this->IsRelocated);
}

void ImageByteMap::fromProtobuf(const MessageType& Message) {
  setNodeUUIDFromBytes(this, Message.uuid());
  this->ByteMap.fromProtobuf(Message.byte_map());
  this->FileName = Message.file_name();
  this->EaMinMax = {EA(Message.ea_min()), EA(Message.ea_max())};
  this->BaseAddress = EA(Message.base_address());
  this->EntryPointAddress = EA(Message.entry_point_address());
  this->RebaseDelta = Message.rebase_delta();
  this->IsRelocated = Message.is_relocated();
}
