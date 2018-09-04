#include "Context.hpp"
#include <gtirb/Node.hpp>

using namespace gtirb;

void Context::unregisterNode(const Node *N) {
  UuidMap.erase(N->getUUID());
}

const Node *Context::findNode(const UUID& ID) const {
  auto Iter = UuidMap.find(ID);
  return Iter != UuidMap.end() ? Iter->second : nullptr;
}

Node *Context::findNode(const UUID& ID) {
  auto Iter = UuidMap.find(ID);
  return Iter != UuidMap.end() ? Iter->second : nullptr;
}
