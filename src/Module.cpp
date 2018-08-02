#include "Module.hpp"
#include "Serialization.hpp"
#include <gtirb/AddrRanges.hpp>
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
    : Node(), addrRanges(std::make_unique<AddrRanges>()), cfg(std::make_unique<CFG>()),
      data(std::make_unique<std::vector<DataObject>>()),
      imageByteMap(std::make_unique<ImageByteMap>()),
      sections(std::make_unique<std::vector<Section>>()), symbols(std::make_unique<SymbolSet>()),
      symbolicOperands(std::make_unique<SymbolicExpressionSet>()) {}

Module::Module(Module&&) = default;
Module::~Module() = default;

void Module::setBinaryPath(boost::filesystem::path x) { this->binaryPath = x; }

boost::filesystem::path Module::getBinaryPath() const { return this->binaryPath; }

void Module::setFileFormat(gtirb::FileFormat x) { this->fileFormat = x; }

gtirb::FileFormat Module::getFileFormat() const { return this->fileFormat; }

void Module::setRebaseDelta(int64_t x) { this->rebaseDelta = x; }

int64_t Module::getRebaseDelta() const { return this->rebaseDelta; }

void Module::setISAID(gtirb::ISAID x) { this->isaID = x; }

gtirb::ISAID Module::getISAID() const { return this->isaID; }

void Module::setPreferredEA(gtirb::EA x) { this->preferredEA = x; }

gtirb::EA Module::getPreferredEA() const { return this->preferredEA; }

gtirb::AddrRanges& Module::getAddrRanges() { return *this->addrRanges.get(); }

const gtirb::AddrRanges& Module::getAddrRanges() const { return *this->addrRanges.get(); }

gtirb::SymbolSet& Module::getSymbols() { return *this->symbols; }

const gtirb::SymbolSet& Module::getSymbols() const { return *this->symbols; }

gtirb::ImageByteMap& Module::getImageByteMap() { return *this->imageByteMap.get(); }

const gtirb::ImageByteMap& Module::getImageByteMap() const { return *this->imageByteMap.get(); }

void Module::setName(std::string x) { this->name = std::move(x); }

std::string Module::getName() const { return this->name; }

void Module::setDecodeMode(uint64_t x) { this->decodeMode = x; }

uint64_t Module::getDecodeMode() const { return this->decodeMode; }

const CFG& Module::getCFG() const { return *this->cfg; }

CFG& Module::getCFG() { return *this->cfg; }

const std::vector<DataObject>& Module::getData() const { return *this->data; }

std::vector<DataObject>& Module::getData() { return *this->data; }

std::vector<Section>& Module::getSections() { return *this->sections; }

const std::vector<Section>& Module::getSections() const { return *this->sections; }

SymbolicExpressionSet& Module::getSymbolicExpressions() { return *this->symbolicOperands; }

const SymbolicExpressionSet& Module::getSymbolicExpressions() const {
  return *this->symbolicOperands;
}

void Module::toProtobuf(MessageType* message) const {
  nodeUUIDToBytes(this, *message->mutable_uuid());
  message->set_binary_path(this->binaryPath.generic_string());
  message->set_preferred_ea(this->preferredEA);
  message->set_rebase_delta(this->rebaseDelta);
  message->set_file_format(static_cast<proto::FileFormat>(this->fileFormat));
  message->set_isa_id(static_cast<proto::ISAID>(this->isaID));
  message->set_name(this->name);
  message->set_decode_mode(this->decodeMode);
  this->addrRanges->toProtobuf(message->mutable_addr_ranges());
  this->imageByteMap->toProtobuf(message->mutable_image_byte_map());
  *message->mutable_cfg() = gtirb::toProtobuf(*this->cfg);
  containerToProtobuf(*this->data, message->mutable_data());
  containerToProtobuf(*this->sections, message->mutable_sections());
  containerToProtobuf(*this->symbolicOperands, message->mutable_symbolic_operands());

  // Special case for symbol set: uses a multimap internally, serialized as a
  // repeated field.
  auto m = message->mutable_symbols();
  initContainer(m, this->symbols->size());
  std::for_each(this->symbols->begin(), this->symbols->end(),
                [m](const auto& node) { addElement(m, gtirb::toProtobuf(node.second)); });
}

void Module::fromProtobuf(const MessageType& message) {
  setNodeUUIDFromBytes(this, message.uuid());
  this->binaryPath = message.binary_path();
  this->preferredEA = gtirb::EA(message.preferred_ea());
  this->rebaseDelta = message.rebase_delta();
  this->fileFormat = static_cast<FileFormat>(message.file_format());
  this->isaID = static_cast<ISAID>(message.isa_id());
  this->name = message.name();
  this->decodeMode = message.decode_mode();
  this->addrRanges->fromProtobuf(message.addr_ranges());
  this->imageByteMap->fromProtobuf(message.image_byte_map());
  gtirb::fromProtobuf(*this->cfg, message.cfg());
  containerFromProtobuf(*this->data, message.data());
  containerFromProtobuf(*this->sections, message.sections());
  containerFromProtobuf(*this->symbolicOperands, message.symbolic_operands());

  // Special case for symbol set: serialized as a repeated field, uses a
  // multimap internally.
  this->symbols->clear();
  const auto& m = message.symbols();
  std::for_each(m.begin(), m.end(), [this](const auto& elt) {
    Symbol sym;
    gtirb::fromProtobuf(sym, elt);
    addSymbol(*this->symbols, std::move(sym));
  });
}
