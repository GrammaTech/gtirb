#include <boost/serialization/export.hpp>
#include <gsl/gsl>
#include <gtirb/AddrRanges.hpp>
#include <gtirb/CFGSet.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/ProcedureSet.hpp>
#include <gtirb/SectionTable.hpp>
#include <gtirb/SymbolSet.hpp>
#include <gtirb/Table.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Module);

Module::Module()
    : Node(),
      addrRanges(std::make_unique<AddrRanges>()),
      cfgSet(std::make_unique<CFGSet>()),
      imageByteMap(std::make_unique<ImageByteMap>()),
      procedureSet(std::make_unique<ProcedureSet>()),
      symbolSet(std::make_unique<SymbolSet>())
{
}

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

bool Module::setEAMinMax(std::pair<gtirb::EA, gtirb::EA> x)
{
    if(x.first <= x.second)
    {
        this->eaMinMax = std::move(x);
        return true;
    }

    this->eaMinMax = std::pair<gtirb::EA, gtirb::EA>(gtirb::EA{}, gtirb::EA{});
    return false;
}

std::pair<gtirb::EA, gtirb::EA> Module::getEAMinMax() const
{
    return this->eaMinMax;
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

gtirb::AddrRanges* Module::getAddrRanges()
{
    return this->addrRanges.get();
}

gtirb::SymbolSet* Module::getSymbolSet()
{
    return this->symbolSet.get();
}

gtirb::ProcedureSet* Module::getProcedureSet()
{
    return this->procedureSet.get();
}

gtirb::ImageByteMap* Module::getImageByteMap()
{
    return this->imageByteMap.get();
}

CFGSet* Module::getCFGSet()
{
    return this->cfgSet.get();
}

const CFGSet* Module::getCFGSet() const
{
    return this->cfgSet.get();
}

SectionTable& Module::getOrCreateSectionTable()
{
    const std::string name{"sections"};
    auto table = getTable(name);
    if(!table)
    {
        table = addTable(name, std::make_unique<SectionTable>());
    }

    auto result = dynamic_cast<SectionTable*>(table);
    assert(result);

    return *result;
}

bool Module::getIsSetupComplete() const
{
    return this->isSetupComplete;
}

bool Module::getIsReadOnly() const
{
    return this->isReadOnly;
}

void Module::setIsSetupComplete()
{
    this->isSetupComplete = true;
}

void Module::setIsReadOnly(bool x)
{
    this->isReadOnly = x;
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

void Module::setBlocks(const std::vector<Block> x)
{
    this->blocks = std::make_unique<std::vector<Block>>(x);
}

template <class Archive>
void Module::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar& boost::serialization::base_object<Node>(*this);
    GTIRB_SERIALIZE_FILESYSTEM_PATH(ar, this->binaryPath);
    ar & this->eaMinMax;
    ar & this->preferredEA;
    ar & this->rebaseDelta;
    ar & this->fileFormat;
    ar & this->isaID;
    ar & this->isSetupComplete;
    ar & this->isReadOnly;
    ar & this->name;
    ar & this->decodeMode;
    ar & this->addrRanges;
    ar & this->cfgSet;
    ar & this->imageByteMap;
    ar & this->procedureSet;
    ar & this->symbolSet;
    ar & this->blocks;
}
