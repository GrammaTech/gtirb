#include "Utility.hpp"

#include <gtirb/Casting.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/DataBlock.hpp>

using namespace gtirb;

inline auto codeBlockKey(const CodeBlock* B) {
  return std::make_tuple(B->getAddress(), B->getSize(), B->getDecodeMode(),
                         B->getUUID());
}

template <>
bool AddressLess::operator()<CodeBlock>(const CodeBlock* B1,
                                        const CodeBlock* B2) const {
  return codeBlockKey(B1) < codeBlockKey(B2);
}

BlockAddressLess::key_type BlockAddressLess::key(const Node& N) {
  if (const auto* CB = dyn_cast<CodeBlock>(&N)) {
    return CB->getAddress();
  }

  if (const auto* DB = dyn_cast<DataBlock>(&N)) {
    return DB->getAddress();
  }

  assert(!"BlockAddressLess got an unknown node type!");
  return std::nullopt;
}
