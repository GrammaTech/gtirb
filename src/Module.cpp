#include <proto/Module.pb.h>
#include <gsl/gsl>
#include <gtirb/AddrRanges.hpp>
#include <gtirb/Block.hpp>
#include <gtirb/Data.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Procedure.hpp>
#include <gtirb/Relocation.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Serialization.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicOperand.hpp>

using namespace gtirb;

Module::Module()
    : Node(),
      addrRanges(std::make_unique<AddrRanges>()),
      imageByteMap(std::make_unique<ImageByteMap>()),
      procedureSet(std::make_unique<ProcedureSet>()),
      symbolSet(std::make_unique<SymbolSet>()),
      blocks(std::make_unique<BlockSet>()),
      relocations(std::make_unique<RelocationSet>()),
      data(std::make_unique<std::vector<Data>>()),
      sections(std::make_unique<std::vector<Section>>()),
      symbolicOperands(std::make_unique<SymbolicOperandSet>())
{
}

Module::Module(Module&&) = default;
Module::~Module() = default;

void Module::setBinaryPath(boost::filesystem::path x)
{
    this->binaryPath = x;
}

boost::filesystem::path Module::getBinaryPath() const
{
    return this->binaryPath;
}

void Module::setFileFormat(gtirb::FileFormat x)
{
    this->fileFormat = x;
}

gtirb::FileFormat Module::getFileFormat() const
{
    return this->fileFormat;
}

void Module::setRebaseDelta(int64_t x)
{
    this->rebaseDelta = x;
}

int64_t Module::getRebaseDelta() const
{
    return this->rebaseDelta;
}

void Module::setISAID(gtirb::ISAID x)
{
    this->isaID = x;
}

gtirb::ISAID Module::getISAID() const
{
    return this->isaID;
}

void Module::setPreferredEA(gtirb::EA x)
{
    this->preferredEA = x;
}

gtirb::EA Module::getPreferredEA() const
{
    return this->preferredEA;
}

gtirb::AddrRanges& Module::getAddrRanges()
{
    return *this->addrRanges.get();
}

const gtirb::AddrRanges& Module::getAddrRanges() const
{
    return *this->addrRanges.get();
}

gtirb::SymbolSet& Module::getSymbolSet()
{
    return *this->symbolSet;
}

const gtirb::SymbolSet& Module::getSymbolSet() const
{
    return *this->symbolSet;
}

gtirb::ProcedureSet& Module::getProcedureSet()
{
    return *this->procedureSet.get();
}

const gtirb::ProcedureSet& Module::getProcedureSet() const
{
    return *this->procedureSet.get();
}

gtirb::ImageByteMap& Module::getImageByteMap()
{
    return *this->imageByteMap.get();
}

const gtirb::ImageByteMap& Module::getImageByteMap() const
{
    return *this->imageByteMap.get();
}

void Module::setName(std::string x)
{
    this->name = std::move(x);
}

std::string Module::getName() const
{
    return this->name;
}

void Module::setDecodeMode(uint64_t x)
{
    this->decodeMode = x;
}

uint64_t Module::getDecodeMode() const
{
    return this->decodeMode;
}

const std::vector<Block>& Module::getBlocks() const
{
    return *this->blocks;
}

std::vector<Block>& Module::getBlocks()
{
    return *this->blocks;
}

const std::vector<Relocation>& Module::getRelocations() const
{
    return *this->relocations;
}

std::vector<Relocation>& Module::getRelocations()
{
    return *this->relocations;
}

const std::vector<Data>& Module::getData() const
{
    return *this->data;
}

std::vector<Data>& Module::getData()
{
    return *this->data;
}

std::vector<Section>& Module::getSections()
{
    return *this->sections;
}

const std::vector<Section>& Module::getSections() const
{
    return *this->sections;
}

SymbolicOperandSet& Module::getSymbolicOperands()
{
    return *this->symbolicOperands;
}

const SymbolicOperandSet& Module::getSymbolicOperands() const
{
    return *this->symbolicOperands;
}

void Module::toProtobuf(MessageType* message) const
{
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
    containerToProtobuf(*this->procedureSet, message->mutable_procedure_set());
    containerToProtobuf(*this->symbolSet, message->mutable_symbol_set());
    containerToProtobuf(*this->blocks, message->mutable_blocks());
    containerToProtobuf(*this->data, message->mutable_data());
    containerToProtobuf(*this->relocations, message->mutable_relocations());
    containerToProtobuf(*this->sections, message->mutable_sections());
    containerToProtobuf(*this->symbolicOperands, message->mutable_symbolic_operands());
}

void Module::fromProtobuf(const MessageType& message)
{
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
    containerFromProtobuf(*this->procedureSet, message.procedure_set());
    containerFromProtobuf(*this->symbolSet, message.symbol_set());
    containerFromProtobuf(*this->blocks, message.blocks());
    containerFromProtobuf(*this->data, message.data());
    containerFromProtobuf(*this->relocations, message.relocations());
    containerFromProtobuf(*this->sections, message.sections());
    containerFromProtobuf(*this->symbolicOperands, message.symbolic_operands());
}
