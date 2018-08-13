#include "IR.hpp"
#include "Serialization.hpp"
#include <gtirb/DataObject.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <gtirb/Table.hpp>
#include <proto/IR.pb.h>

using namespace gtirb;

IR::IR() = default;
IR::IR(IR&&) = default;
IR& IR::operator=(IR&&) = default;
IR::~IR() = default;

std::vector<Module>& IR::getModules() { return this->modules; }

const std::vector<Module>& IR::getModules() const { return this->modules; }

std::vector<const Module*> IR::getModulesWithPreferredEA(EA x) const {
  std::vector<const Module*> results;

  for (const auto& m : this->modules) {
    if (m.getPreferredEA() == x) {
      results.push_back(&m);
    }
  }

  return results;
}

std::vector<const Module*> IR::getModulesContainingEA(EA x) const {
  std::vector<const Module*> results;

  for (const auto& m : this->modules) {
    auto minmax = m.getImageByteMap().getEAMinMax();
    if ((x >= minmax.first) && (x < minmax.second)) {
      results.push_back(&m);
    }
  }

  return results;
}

void IR::addTable(std::string name, Table&& x) { this->tables[std::move(name)] = std::move(x); }

const gtirb::Table* IR::getTable(const std::string& x) const {
  auto found = this->tables.find(x);
  if (found != std::end(this->tables)) {
    return &(found->second);
  }

  return nullptr;
}

gtirb::Table* IR::getTable(const std::string& x) {
  auto found = this->tables.find(x);
  if (found != std::end(this->tables)) {
    return &(found->second);
  }

  return nullptr;
}

bool IR::removeTable(const std::string& x) {
  const auto found = this->tables.find(x);

  if (found != std::end(this->tables)) {
    this->tables.erase(found);
    return true;
  }

  return false;
}

size_t IR::getTableSize() const { return this->tables.size(); }

bool IR::getTablesEmpty() const { return this->tables.empty(); }

void IR::clearTables() { this->tables.clear(); }

void IR::toProtobuf(MessageType* message) const {
  nodeUUIDToBytes(this, *message->mutable_uuid());
  containerToProtobuf(this->modules, message->mutable_modules());
  containerToProtobuf(this->tables, message->mutable_tables());
}

void IR::fromProtobuf(const MessageType& message) {
  setNodeUUIDFromBytes(this, message.uuid());
  containerFromProtobuf(this->modules, message.modules());
  containerFromProtobuf(this->tables, message.tables());
}

void IR::save(std::ostream& out) const {
  MessageType message;
  this->toProtobuf(&message);
  message.SerializeToOstream(&out);
}

void IR::load(std::istream& in) {
  MessageType message;
  message.ParseFromIstream(&in);
  this->fromProtobuf(message);
}
