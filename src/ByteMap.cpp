#include "ByteMap.hpp"
#include "Serialization.hpp"
#include <gtirb/Utilities.hpp>
#include <proto/ByteMap.pb.h>
#include <algorithm>
#include <cstring>

using namespace gtirb;

constexpr uint64_t PageOffsetMask{static_cast<uint64_t>(PageSize) - 1};
constexpr uint64_t PageIndexMask{~PageOffsetMask};

namespace {
// Align an address to a page boundary.
EA addressToAlignedAddress(const EA x) { return EA{x.get() & PageIndexMask}; }

// Give the offset within a page of an address
size_t addressToOffset(const EA x) { return static_cast<size_t>(x.get()) & PageOffsetMask; }

// Number of bytes residing within the first page.
size_t bytesWithinFirstPage(const EA x, const size_t bytes) {
  const size_t bytesToPageBoundary = PageSize - (static_cast<size_t>(x.get()) & PageOffsetMask);

  return bytes < bytesToPageBoundary ? bytes : bytesToPageBoundary;
}
} // namespace

bool ByteMap::empty() const { return this->data.empty(); }

size_t ByteMap::size() const { return this->data.size() * PageSize; }

void ByteMap::setData(EA ea, gsl::span<const gsl::byte> bytes) {
  int64_t bytesRemaining = bytes.size_bytes();
  auto currentBuffer = bytes.data();
  auto currentAddress = ea;

  while (bytesRemaining > 0) {
    const auto bytesThisPage = bytesWithinFirstPage(currentAddress, bytesRemaining);
    const auto pageAddress = addressToAlignedAddress(currentAddress);
    const auto pageOffset = addressToOffset(currentAddress);

    auto& page = this->getOrCreatePage(pageAddress);

    std::memcpy(&(page[pageOffset]), currentBuffer, bytesThisPage);

    currentBuffer += bytesThisPage;
    currentAddress += EA{bytesThisPage};
    bytesRemaining -= static_cast<int64_t>(bytesThisPage);
  }

  // If we went to negative bytes remaining, something went wrong.
  Expects(bytesRemaining >= 0);
}

std::vector<uint8_t> ByteMap::getData(EA x, size_t bytes) const {
  std::vector<uint8_t> buffer(bytes, uint8_t{0});

  size_t currentBufferPos{0};
  int64_t bytesRemaining = static_cast<int64_t>(bytes);
  auto currentAddress = x;

  while (bytesRemaining > 0) {
    const auto bytesThisPage = bytesWithinFirstPage(currentAddress, bytesRemaining);
    const auto pageAddress = addressToAlignedAddress(currentAddress);

    const auto page = this->getPage(pageAddress);

    if (page != nullptr) {
      const auto pageOffset = addressToOffset(currentAddress);

      auto pageBegin = std::begin(*page);
      std::advance(pageBegin, pageOffset);

      auto bufferBegin = std::begin(buffer);
      std::advance(bufferBegin, currentBufferPos);

      std::copy_n(pageBegin, bytesThisPage, bufferBegin);
    }

    currentBufferPos += bytesThisPage;
    currentAddress += static_cast<EA>(bytesThisPage);
    bytesRemaining -= static_cast<int64_t>(bytesThisPage);
  }

  // If we went to negative bytes remaining, something went wrong.
  Expects(bytesRemaining >= 0);

  return buffer;
}

std::vector<uint8_t> ByteMap::getDataUntil(EA x, uint8_t sentinel, size_t maxBytes) const {
  std::vector<uint8_t> buffer;
  auto bytesRemaining = static_cast<int64_t>(maxBytes);
  auto currentAddress = x;

  while (bytesRemaining > 0) {
    const auto bytesThisPage = bytesWithinFirstPage(currentAddress, bytesRemaining);
    const auto pageAddress = addressToAlignedAddress(currentAddress);
    const auto page = this->getPage(pageAddress);

    if (page != nullptr) {
      const auto pageOffset = addressToOffset(currentAddress);

      for (size_t i = 0; i < bytesThisPage; ++i) {
        const auto byte = (*page)[pageOffset + i];
        buffer.push_back(byte);

        if (byte == sentinel) {
          return buffer;
        }
      }

      currentAddress += static_cast<EA>(bytesThisPage);
      bytesRemaining -= static_cast<int64_t>(bytesThisPage);
    } else {
      if (sentinel == '\0') {
        buffer.push_back('\0');
      }

      break;
    }
  }

  // If we went to negative bytes remaining, something went wrong.
  Expects(bytesRemaining >= 0);

  return buffer;
}

const ByteMap::Page* const ByteMap::getPage(const EA x) const {
  Expects(addressToOffset(x) == 0);

  const auto it = this->data.find(x);
  if (it != std::end(this->data)) {
    return &(it->second);
  }

  return nullptr;
}

ByteMap::Page& ByteMap::getOrCreatePage(const EA x) {
  Expects(addressToOffset(x) == 0);
  return this->data[x];
}

void ByteMap::toProtobuf(MessageType* message) const {
  containerToProtobuf(this->data, message->mutable_data());
}

void ByteMap::fromProtobuf(const MessageType& message) {
  containerFromProtobuf(this->data, message.data());
}
