#include "ImageByteMap.hpp"
#include "Serialization.hpp"
#include <proto/ImageByteMap.pb.h>

using namespace gtirb;

void ImageByteMap::setFileName(boost::filesystem::path x) { this->fileName = x; }

boost::filesystem::path ImageByteMap::getFileName() const { return this->fileName; }

void ImageByteMap::setBaseAddress(EA x) { this->baseAddress = x; }

EA ImageByteMap::getBaseAddress() const { return this->baseAddress; }

void ImageByteMap::setEntryPointAddress(EA x) { this->entryPointAddress = x; }

EA ImageByteMap::getEntryPointAddress() const { return this->entryPointAddress; }

bool ImageByteMap::setEAMinMax(std::pair<gtirb::EA, gtirb::EA> x) {
  if (x.first <= x.second) {
    this->eaMinMax = std::move(x);
    return true;
  }

  this->eaMinMax = std::pair<gtirb::EA, gtirb::EA>(gtirb::EA{}, gtirb::EA{});
  return false;
}

std::pair<gtirb::EA, gtirb::EA> ImageByteMap::getEAMinMax() const { return this->eaMinMax; }

void ImageByteMap::setRebaseDelta(int64_t x) { this->rebaseDelta = x; }

int64_t ImageByteMap::getRebaseDelta() const { return this->rebaseDelta; }

void ImageByteMap::setIsRelocated() { this->isRelocated = true; }

bool ImageByteMap::getIsRelocated() const { return this->isRelocated; }

void ImageByteMap::setData(EA ea, gsl::span<const gsl::byte> data) {
  if (ea >= this->eaMinMax.first &&
      (ea + EA{(uint64_t)data.size_bytes()} - EA{1}) <= this->eaMinMax.second) {
    this->byteMap.setData(ea, data);
  } else {
    throw std::out_of_range("Attempt to set data at an EA out of range of the min and max EA.");
  }
}

void ImageByteMap::setData(EA ea, size_t bytes, uint8_t value) {
  for (uint64_t i = 0; i < bytes; i++) {
    setData(ea + i, value);
  }
}

std::vector<uint8_t> ImageByteMap::getData(EA x, size_t bytes) const {
  if (x >= this->eaMinMax.first && (x + EA{bytes} - EA{1}) <= this->eaMinMax.second) {
    return this->byteMap.getData(x, bytes);
  }

  throw std::out_of_range("Attempt to get data at an EA out of range of the min and max EA.");
}

void ImageByteMap::toProtobuf(MessageType* message) const {
  nodeUUIDToBytes(this, *message->mutable_uuid());
  this->byteMap.toProtobuf(message->mutable_byte_map());
  message->set_file_name(this->fileName.generic_string());
  message->set_ea_min(this->eaMinMax.first);
  message->set_ea_max(this->eaMinMax.second);
  message->set_base_address(this->baseAddress);
  message->set_entry_point_address(this->entryPointAddress);
  message->set_rebase_delta(this->rebaseDelta);
  message->set_is_relocated(this->isRelocated);
}

void ImageByteMap::fromProtobuf(const MessageType& message) {
  setNodeUUIDFromBytes(this, message.uuid());
  this->byteMap.fromProtobuf(message.byte_map());
  this->fileName = message.file_name();
  this->eaMinMax = {EA(message.ea_min()), EA(message.ea_max())};
  this->baseAddress = EA(message.base_address());
  this->entryPointAddress = EA(message.entry_point_address());
  this->rebaseDelta = message.rebase_delta();
  this->isRelocated = message.is_relocated();
}
