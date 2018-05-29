#include <gtirb/Section.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Section);

Section::Section(std::string n, uint64_t s, EA ea) : Node(), name(n), size(s), startingAddress(ea)
{
}

EA Section::addressLimit() const
{
    // If base address is bad, return a bad EA.
    if(this->startingAddress == EA{})
    {
        return EA{};
    }
    else
    {
        return EA(this->startingAddress.get() + this->size);
    }
}

bool Section::contains(EA ea) const
{
    return (ea >= this->startingAddress) && (ea < this->addressLimit());
}

bool Section::operator==(const Section& other) const
{
    return this->startingAddress == other.startingAddress && this->size == other.size
           && this->name == other.name;
}

bool Section::operator!=(const Section& other) const
{
    return !(*this == other);
}
