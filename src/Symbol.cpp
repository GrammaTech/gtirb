#include <gtirb/Module.hpp>
#include <gtirb/Symbol.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Symbol);
BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::SymbolReference);

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
