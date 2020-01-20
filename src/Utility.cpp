#include "Utility.hpp"

#include <gtirb/Casting.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/DataBlock.hpp>

using namespace gtirb;

BlockAddressOrder::key_type BlockAddressOrder::key(const Node& N) {
  if (const auto *CB = dyn_cast<CodeBlock>(&N)) {
    return CB->getAddress();
  }

  if (const auto *DB = dyn_cast<DataBlock>(&N)) {
    return DB->getAddress();
  }

  assert(!"BlockAddressOrder got an unknown node type!");
  return std::nullopt;
}
