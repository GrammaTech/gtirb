#include <gtirb/AddrRanges.hpp>
#include <gtirb/CFG.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/ModuleAux.hpp>
#include <gtirb/ModuleCore.hpp>
#include <gtirb/ModuleSummary.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/NodeUtilities.hpp>
#include <gtirb/NodeValidators.hpp>
#include <gtirb/SymbolSet.hpp>
#include <gtirb/ProcedureSet.hpp>
#include <boost/serialization/export.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Module);

Module::Module()
{
    this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::IR>());
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

void Module::setPreferredEA(gtirb::EA x)
{
    this->preferredEA = x;
}

gtirb::EA Module::getPreferredEA() const
{
    return this->preferredEA;
}

gtirb::ModuleSummary* Module::getOrCreateModuleSummary()
{
    return gtirb::GetOrCreateChildOfType<gtirb::ModuleSummary>(this);
}

gtirb::ModuleCore* Module::getOrCreateModuleCore()
{
    return gtirb::GetOrCreateChildOfType<gtirb::ModuleCore>(this);
}

gtirb::ModuleAux* Module::getOrCreateModuleAux()
{
    return gtirb::GetOrCreateChildOfType<gtirb::ModuleAux>(this);
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

gtirb::CFG* Module::getOrCreateCFG()
{
    return gtirb::GetOrCreateChildOfType<gtirb::CFG>(this);
}
