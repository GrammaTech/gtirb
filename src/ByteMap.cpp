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

void ByteMap::setData(EA ea, uint8_t x) {
  const auto pageAddress = addressToAlignedAddress(ea);
  const auto pageOffset = addressToOffset(ea);

  auto& page = this->getOrCreatePage(pageAddress);
  page[pageOffset] = x;
}

// The implementation of this function uses decltype so that its contents can be easily copy/pasted
// to other sizes of 'x'.  Not templated because there's only three options here.
void ByteMap::setData(EA ea, uint16_t x) {
  const auto bytesThisPage = bytesWithinFirstPage(ea, sizeof(decltype(x)));

  if (bytesThisPage < sizeof(decltype(x))) {
    this->setData(ea, as_bytes(gsl::make_span(&x, 1)));
    return;
  }

  const auto pageAddress = addressToAlignedAddress(ea);
  const auto pageOffset = addressToOffset(ea);

  auto& page = this->getOrCreatePage(pageAddress);
  *(decltype(x)*)&(page[pageOffset]) = x;
}

void ByteMap::setData(EA ea, uint32_t x) {
  const auto bytesThisPage = bytesWithinFirstPage(ea, sizeof(decltype(x)));

  if (bytesThisPage < sizeof(decltype(x))) {
    this->setData(ea, as_bytes(gsl::make_span(&x, 1)));
    return;
  }

  const auto pageAddress = addressToAlignedAddress(ea);
  const auto pageOffset = addressToOffset(ea);

  auto& page = this->getOrCreatePage(pageAddress);
  *(decltype(x)*)&(page[pageOffset]) = x;
}

void ByteMap::setData(EA ea, uint64_t x) {
  const auto bytesThisPage = bytesWithinFirstPage(ea, sizeof(decltype(x)));

  if (bytesThisPage < sizeof(decltype(x))) {
    this->setData(ea, as_bytes(gsl::make_span(&x, 1)));
    return;
  }

  const auto pageAddress = addressToAlignedAddress(ea);
  const auto pageOffset = addressToOffset(ea);

  auto& page = this->getOrCreatePage(pageAddress);
  *(decltype(x)*)&(page[pageOffset]) = x;
}

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

uint8_t ByteMap::getData8(EA x) const { return this->getData(x, 1)[0]; }

uint16_t ByteMap::getData16(EA x) const {
  const auto byteArray = this->getData(x, sizeof(uint16_t));
  const auto wordArray = ByteArray8To16(byteArray, false);
  const auto word = wordArray[0];
  return word;
}

uint32_t ByteMap::getData32(EA x) const {
  const auto byteArray = this->getData(x, sizeof(uint32_t));
  const auto wordArray = ByteArray8To32(byteArray, false);
  const auto word = wordArray[0];
  return word;
}

uint64_t ByteMap::getData64(EA x) const {
  const auto byteArray = this->getData(x, sizeof(uint64_t));
  const auto wordArray = ByteArray8To64(byteArray, false);
  const auto word = wordArray[0];
  return word;
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
