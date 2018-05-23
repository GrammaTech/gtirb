#include <boost/serialization/export.hpp>
#include <gsl/gsl>
#include <gtirb/AddrRanges.hpp>
#include <gtirb/CFGSet.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/NodeUtilities.hpp>
#include <gtirb/NodeValidators.hpp>
#include <gtirb/ProcedureSet.hpp>
#include <gtirb/SectionTable.hpp>
#include <gtirb/SymbolSet.hpp>
#include <gtirb/Table.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Module);

Module::Module()
{
    this->addParentValidator(NodeValidatorHasParentOfType<gtirb::IR>);
}

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

gtirb::AddrRanges* Module::getOrCreateAddrRanges()
{
    return gtirb::GetOrCreateChildOfType<gtirb::AddrRanges>(this);
}

gtirb::SymbolSet* Module::getOrCreateSymbolSet()
{
    return gtirb::GetOrCreateChildOfType<gtirb::SymbolSet>(this);
}

gtirb::ProcedureSet* Module::getOrCreateProcedureSet()
{
    return gtirb::GetOrCreateChildOfType<gtirb::ProcedureSet>(this);
}

gtirb::ImageByteMap* Module::getOrCreateImageByteMap()
{
    return gtirb::GetOrCreateChildOfType<gtirb::ImageByteMap>(this);
}

gtirb::CFGSet* Module::getOrCreateCFGSet()
{
    return gtirb::GetOrCreateChildOfType<gtirb::CFGSet>(this);
}

gtirb::CFGSet* Module::getCFGSet()
{
    const auto allChildren = gtirb::GetChildrenOfType<gtirb::CFGSet>(this);
    Expects(allChildren.size() <= 1);

    if(allChildren.empty() == false)
    {
        return allChildren[0];
    }

    return nullptr;
}

const gtirb::CFGSet* const Module::getCFGSet() const
{
    const auto allChildren = gtirb::GetChildrenOfType<gtirb::CFGSet>(this);
    Expects(allChildren.size() <= 1);

    if(allChildren.empty() == false)
    {
        return allChildren[0];
    }

    return nullptr;
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
