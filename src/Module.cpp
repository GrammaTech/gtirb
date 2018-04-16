#include <gtirb/Module.hpp>
#include <gtirb/ModuleSummary.hpp>
#include <gtirb/ModuleCore.hpp>
#include <gtirb/ModuleAux.hpp>
#include <gtirb/NodeStructureError.hpp>

using namespace gtirb;

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

gtirb::ModuleSummary* gtirb::GetOrCreateModuleSummary(gtirb::Module* const x)
{
    const auto children = GetChildrenOfType<gtirb::ModuleSummary>(x);
    if(children.empty() == false)
    {
        if(children.size() == 1)
        {
            return children[0];
        }
        else
        {
            throw gtirb::NodeStructureError("Multiple \"ModuleSummary\" children were found under one Module.");
        }
    }

    auto moduleSummary = std::make_unique<gtirb::ModuleSummary>();
    auto moduleSummaryPtr = moduleSummary.get();
    x->push_back(std::move(moduleSummary));
    return moduleSummaryPtr;
}

gtirb::ModuleCore* gtirb::GetOrCreateModuleCore(Module* const x)
{
    const auto children = GetChildrenOfType<gtirb::ModuleCore>(x);
    if(children.empty() == false)
    {
        if(children.size() == 1)
        {
            return children[0];
        }
        else
        {
            throw gtirb::NodeStructureError("Multiple \"ModuleCore\" children were found under one Module.");
        }
    }

    auto moduleSummary = std::make_unique<gtirb::ModuleCore>();
    auto moduleSummaryPtr = moduleSummary.get();
    x->push_back(std::move(moduleSummary));
    return moduleSummaryPtr;
}

gtirb::ModuleAux* gtirb::GetOrCreateModuleAux(Module* const x)
{
    const auto children = GetChildrenOfType<gtirb::ModuleAux>(x);
    if(children.empty() == false)
    {
        if(children.size() == 1)
        {
            return children[0];
        }
        else
        {
            throw gtirb::NodeStructureError("Multiple \"ModuleAux\" children were found under one Module.");
        }
    }

    auto moduleSummary = std::make_unique<gtirb::ModuleAux>();
    auto moduleSummaryPtr = moduleSummary.get();
    x->push_back(std::move(moduleSummary));
    return moduleSummaryPtr;
}
