#include <gtirb/ModuleSummary.hpp>

using namespace gtirb;

void ModuleSummary::setName(std::string x)
{
    this->name = std::move(x);
}

std::string ModuleSummary::getName() const
{
    return this->name;
}

void ModuleSummary::setDecodeMode(uint64_t x)
{
    this->decodeMode = x;
}

uint64_t ModuleSummary::getDecodeMode() const
{
    return this->decodeMode;
}
