#include <gtirb/Serialization.hpp>
#include <gtirb/Symbol.hpp>

using namespace gtirb;

Symbol::Symbol(EA x) : Node(), ea(x)
{
}

Symbol::Symbol(EA x, std::string name_) : Node(), ea(x), name(name_)
{
}

void Symbol::setEA(gtirb::EA x)
{
    this->ea = x;
}

gtirb::EA Symbol::getEA() const
{
    return this->ea;
}

void Symbol::setName(std::string x)
{
    this->name = x;
}

std::string Symbol::getName() const
{
    return this->name;
}

void Symbol::setType(gtirb::Symbol::Type x)
{
    this->type = x;
}

gtirb::Symbol::Type Symbol::getType() const
{
    return this->type;
}

void Symbol::setOffset(int64_t x)
{
    this->offset = x;
}

int64_t Symbol::getOffset()
{
    return this->offset;
}

void Symbol::setElementSize(int64_t x)
{
    this->elementSize = x;
}

int64_t Symbol::getElementSize() const
{
    return this->elementSize;
}

void Symbol::setBitSize(int64_t x)
{
    this->bitSize = x;
}

int64_t Symbol::getBitSize() const
{
    return this->bitSize;
}

void Symbol::setIsFormal(bool x)
{
    this->isFormal = x;
}

bool Symbol::getIsFormal() const
{
    return this->isFormal;
}

void Symbol::setEnableForceName(bool x)
{
    this->enableForceName = x;
}

bool Symbol::getEnableForceName() const
{
    return this->enableForceName;
}

void Symbol::setEnableGapSize(bool x)
{
    this->enableGapSize = x;
}

bool Symbol::getEnableGapSize() const
{
    return this->enableGapSize;
}

void Symbol::setIsNameOnly(bool x)
{
    this->isNameOnly = x;
}

bool Symbol::getIsNameOnly() const
{
    return this->isNameOnly;
}

void Symbol::setDeclarationKind(gtirb::Symbol::DeclarationKind x)
{
    this->declarationKind = x;
}

gtirb::Symbol::DeclarationKind Symbol::getDeclarationKind() const
{
    return this->declarationKind;
}

void Symbol::setLinkType(Symbol::LinkType x)
{
    this->linkType = x;
}

gtirb::Symbol::LinkType Symbol::getLinkType() const
{
    return this->linkType;
}

void Symbol::setStorageKind(Symbol::StorageKind x)
{
    this->storageKind = x;
}

gtirb::Symbol::StorageKind Symbol::getStorageKind() const
{
    return this->storageKind;
}

void Symbol::setIsGlobal(bool x)
{
    this->isGlobal = x;
}

bool Symbol::getIsGlobal() const
{
    return this->isGlobal;
}

void Symbol::toProtobuf(MessageType* message) const
{
    nodeUUIDToBytes(this, *message->mutable_uuid());
    message->set_ea(this->ea);
    message->set_name(this->name);
    message->set_offset(this->offset);
    message->set_element_size(this->elementSize);
    message->set_bit_size(this->bitSize);
    message->set_type(static_cast<proto::Type>(this->type));
    message->set_declaration_kind(static_cast<proto::DeclarationKind>(this->declarationKind));
    message->set_link_type(static_cast<proto::LinkType>(this->linkType));
    message->set_storage_kind(static_cast<proto::StorageKind>(this->storageKind));
    message->set_enable_force_name(this->enableForceName);
    message->set_enable_gap_size(this->enableGapSize);
    message->set_is_formal(this->isFormal);
    message->set_is_name_only(this->isNameOnly);
    message->set_is_global(this->isGlobal);
}

void Symbol::fromProtobuf(const MessageType& message)
{
    setNodeUUIDFromBytes(this, message.uuid());
    this->ea = EA(message.ea());
    this->name = message.name();
    this->offset = message.offset();
    this->elementSize = message.element_size();
    this->bitSize = message.bit_size();
    this->type = static_cast<Type>(message.type());
    this->declarationKind = static_cast<DeclarationKind>(message.declaration_kind());
    this->linkType = static_cast<LinkType>(message.link_type());
    this->storageKind = static_cast<StorageKind>(message.storage_kind());
    this->enableForceName = message.enable_force_name();
    this->enableGapSize = message.enable_gap_size();
    this->isFormal = message.is_formal();
    this->isNameOnly = message.is_name_only();
    this->isGlobal = message.is_global();
}
