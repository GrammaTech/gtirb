#include "Utility.hpp"

#include <gtirb/Casting.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/DataBlock.hpp>

using namespace gtirb;

inline auto codeBlockKey(const CodeBlock* B) {
  return std::make_tuple(B->getAddress(), B->getSize(), B->getKind(),
                         B->getDecodeMode(), B->getUUID());
}

inline auto dataBlockKey(const DataBlock* B) {
  // Include a dummy "decode mode" to match the key type for CodeBlocks for
  // blockKey().
  return std::make_tuple(B->getAddress(), B->getSize(), B->getKind(),
                         DecodeMode::Default, B->getUUID());
}

inline auto blockKey(const Node& N) {
  if (const auto* CB = dyn_cast<CodeBlock>(&N)) {
    return codeBlockKey(CB);
  } else {
    auto* DB = cast<DataBlock>(&N);
    return dataBlockKey(DB);
  }
}

template <>
bool AddressLess::operator()<CodeBlock>(const CodeBlock* B1,
                                        const CodeBlock* B2) const {
  return codeBlockKey(B1) < codeBlockKey(B2);
}

template <>
bool AddressLess::operator()<DataBlock>(const DataBlock* B1,
                                        const DataBlock* B2) const {
  return dataBlockKey(B1) < dataBlockKey(B2);
}

bool BlockAddressLess::operator()(const Node& N1, const Node& N2) const {
  return blockKey(N1) < blockKey(N2);
}
