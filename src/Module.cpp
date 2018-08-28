#include "Module.hpp"
#include "Serialization.hpp"
#include <gtirb/Block.hpp>
#include <gtirb/CFG.hpp>
#include <gtirb/DataObject.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <proto/Module.pb.h>
#include <gsl/gsl>
#include <map>

using namespace gtirb;

Module::Module()
    : Node(), Cfg(std::make_unique<CFG>()),
      Data(std::make_unique<std::vector<DataObject>>()),
      ImageByteMap_(std::make_unique<gtirb::ImageByteMap>()),
      Sections(std::make_unique<std::vector<Section>>()),
      Symbols(std::make_unique<SymbolSet>()),
      SymbolicOperands(std::make_unique<SymbolicExpressionSet>()) {}

Module::Module(Module&&) = default;
Module::~Module() = default;

gtirb::SymbolSet& Module::getSymbols() { return *this->Symbols; }

const gtirb::SymbolSet& Module::getSymbols() const { return *this->Symbols; }

gtirb::ImageByteMap& Module::getImageByteMap() {
  return *this->ImageByteMap_.get();
}

const gtirb::ImageByteMap& Module::getImageByteMap() const {
  return *this->ImageByteMap_.get();
}

const CFG& Module::getCFG() const { return *this->Cfg; }

CFG& Module::getCFG() { return *this->Cfg; }

const std::vector<DataObject>& Module::getData() const { return *this->Data; }

std::vector<DataObject>& Module::getData() { return *this->Data; }

std::vector<Section>& Module::getSections() { return *this->Sections; }

const std::vector<Section>& Module::getSections() const {
  return *this->Sections;
}

SymbolicExpressionSet& Module::getSymbolicExpressions() {
  return *this->SymbolicOperands;
}

const SymbolicExpressionSet& Module::getSymbolicExpressions() const {
  return *this->SymbolicOperands;
}

void Module::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_binary_path(this->BinaryPath);
  Message->set_preferred_addr(static_cast<uint64_t>(this->PreferredAddr));
  Message->set_rebase_delta(this->RebaseDelta);
  Message->set_file_format(static_cast<proto::FileFormat>(this->FileFormat));
  Message->set_isa_id(static_cast<proto::ISAID>(this->IsaID));
  Message->set_name(this->Name);
  this->ImageByteMap_->toProtobuf(Message->mutable_image_byte_map());
  *Message->mutable_cfg() = gtirb::toProtobuf(*this->Cfg);
  containerToProtobuf(*this->Data, Message->mutable_data());
  containerToProtobuf(*this->Sections, Message->mutable_sections());
  containerToProtobuf(*this->SymbolicOperands,
                      Message->mutable_symbolic_operands());

  // Special case for symbol set: uses a multimap internally, serialized as a
  // repeated field.
  auto M = Message->mutable_symbols();
  initContainer(M, this->Symbols->size());
  std::for_each(
      this->Symbols->begin(), this->Symbols->end(),
      [M](const auto& N) { addElement(M, gtirb::toProtobuf(N.second)); });
}

void Module::fromProtobuf(const MessageType& Message) {
  setNodeUUIDFromBytes(this, Message.uuid());
  this->BinaryPath = Message.binary_path();
  this->PreferredAddr = Addr(Message.preferred_addr());
  this->RebaseDelta = Message.rebase_delta();
  this->FileFormat = static_cast<gtirb::FileFormat>(Message.file_format());
  this->IsaID = static_cast<ISAID>(Message.isa_id());
  this->Name = Message.name();
  this->ImageByteMap_->fromProtobuf(Message.image_byte_map());
  gtirb::fromProtobuf(*this->Cfg, Message.cfg());
  containerFromProtobuf(*this->Data, Message.data());
  containerFromProtobuf(*this->Sections, Message.sections());
  containerFromProtobuf(*this->SymbolicOperands, Message.symbolic_operands());

  // Special case for symbol set: serialized as a repeated field, uses a
  // multimap internally.
  this->Symbols->clear();
  const auto& M = Message.symbols();
  std::for_each(M.begin(), M.end(), [this](const auto& Elt) {
    Symbol Sym;
    gtirb::fromProtobuf(Sym, Elt);
    addSymbol(*this->Symbols, std::move(Sym));
  });
}
