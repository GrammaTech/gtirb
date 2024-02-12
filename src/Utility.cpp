#include "Utility.hpp"

#include <gtirb/Casting.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/DataBlock.hpp>

using namespace gtirb;

static inline auto codeBlockSecondaryKey(const CodeBlock* B) {
  return std::make_tuple(B->getSize(), B->getKind(), B->getDecodeMode(),
                         B->getUUID());
}

static inline auto dataBlockSecondaryKey(const DataBlock* B) {
  // Include a dummy "decode mode" to match the key type for CodeBlocks for
  // blockKey().
  return std::make_tuple(B->getSize(), B->getKind(), DecodeMode::Default,
                         B->getUUID());
}

inline auto blockSecondaryKey(const Node* N) {
  if (const auto* CB = dyn_cast<CodeBlock>(N)) {
    return codeBlockSecondaryKey(CB);
  } else {
    auto* DB = cast<DataBlock>(N);
    return dataBlockSecondaryKey(DB);
  }
}

static inline auto codeBlockAddressKey(const CodeBlock* B) {
  return std::make_tuple(B->getAddress(), codeBlockSecondaryKey(B));
}

static inline auto dataBlockAddressKey(const DataBlock* B) {
  return std::make_tuple(B->getAddress(), dataBlockSecondaryKey(B));
}

static inline auto blockAddressKey(const Node* N) {
  if (const auto* CB = dyn_cast<CodeBlock>(N)) {
    return codeBlockAddressKey(CB);
  } else {
    auto* DB = cast<DataBlock>(N);
    return dataBlockAddressKey(DB);
  }
}

template <>
bool AddressLess::operator()<CodeBlock>(const CodeBlock* B1,
                                        const CodeBlock* B2) const {
  return codeBlockAddressKey(B1) < codeBlockAddressKey(B2);
}

template <>
bool AddressLess::operator()<DataBlock>(const DataBlock* B1,
                                        const DataBlock* B2) const {
  return dataBlockAddressKey(B1) < dataBlockAddressKey(B2);
}

bool BlockAddressLess::operator()(const Node* N1, const Node* N2) const {
  return blockAddressKey(N1) < blockAddressKey(N2);
}

bool BlockOffsetPairLess::operator()(
    std::pair<uint64_t, const Node*> B1,
    std::pair<uint64_t, const Node*> B2) const {
  return std::make_tuple(B1.first, blockSecondaryKey(B1.second)) <
         std::make_tuple(B2.first, blockSecondaryKey(B2.second));
}
