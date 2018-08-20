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

std::vector<Module *>& IR::getModules() { return this->Modules; }

const std::vector<Module *>& IR::getModules() const { return this->Modules; }

std::vector<const Module*> IR::getModulesWithPreferredEA(EA X) const {
  std::vector<const Module*> Results;

  for (const auto *m : this->Modules) {
    if (m->getPreferredEA() == X) {
      Results.push_back(m);
    }
  }

  return Results;
}

std::vector<const Module*> IR::getModulesContainingEA(EA X) const {
  std::vector<const Module*> Results;

  for (const auto *M : this->Modules) {
    auto MinMax = M->getImageByteMap().getEAMinMax();
    if ((X >= MinMax.first) && (X < MinMax.second)) {
      Results.push_back(M);
    }
  }

  return Results;
}

void IR::addTable(std::string Name, Table&& X) {
  this->Tables[std::move(Name)] = std::move(X);
}

const gtirb::Table* IR::getTable(const std::string& X) const {
  auto Found = this->Tables.find(X);
  if (Found != std::end(this->Tables)) {
    return &(Found->second);
  }

  return nullptr;
}

gtirb::Table* IR::getTable(const std::string& X) {
  auto Found = this->Tables.find(X);
  if (Found != std::end(this->Tables)) {
    return &(Found->second);
  }

  return nullptr;
}

bool IR::removeTable(const std::string& X) {
  const auto Found = this->Tables.find(X);

  if (Found != std::end(this->Tables)) {
    this->Tables.erase(Found);
    return true;
  }

  return false;
}

size_t IR::getTableSize() const { return this->Tables.size(); }

bool IR::getTablesEmpty() const { return this->Tables.empty(); }

void IR::clearTables() { this->Tables.clear(); }

void IR::setUUID() {
  for (auto *M : Modules) {
    M->setUUID();
  }
  Node::setUUID();
}

void IR::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  containerToProtobuf(this->Modules, Message->mutable_modules());
  containerToProtobuf(this->Tables, Message->mutable_tables());
}

IR *IR::fromProtobuf(Context &C, const MessageType& Message) {
  auto *I = IR::Create(C);
  setNodeUUIDFromBytes(I, Message.uuid());
  containerFromProtobuf(C, I->Modules, Message.modules());
  containerFromProtobuf(C, I->Tables, Message.tables());
  return I;
}

void IR::save(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  Message.SerializeToOstream(&Out);
}

IR *IR::load(Context &C, std::istream& In) {
  MessageType Message;
  Message.ParseFromIstream(&In);
  return IR::fromProtobuf(C, Message);
}
