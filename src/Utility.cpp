#include "Utility.hpp"

#include <gtirb/Casting.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/DataBlock.hpp>

using namespace gtirb;

BlockAddressOrder::key_type BlockAddressOrder::getAddress(const Node* N) {
  if (isa<CodeBlock>(N)) {
    return cast<CodeBlock>(N)->getAddress();
  }

  if (isa<DataBlock>(N)) {
    return cast<DataBlock>(N)->getAddress();
  }

  assert(!"BlockAddressOrder got an unknown node type!");
  return std::nullopt;
}
