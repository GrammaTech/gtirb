#include <gtirb/EA.hpp>
#include <sstream>

using namespace gtirb;

void EA::set(uint64_t x)
{
    this->ea = x;
}

uint64_t EA::get() const
{
    return this->ea;
}

EA::operator uint64_t() const
{
    return this->ea;
}

EA EA::operator=(EA x)
{
    this->ea = x.ea;
    return *this;
}

bool EA::operator==(uint64_t x) const
{
    return this->ea == x;
}

bool EA::operator==(const EA x) const
{
    return this->ea == x.ea;
}

bool EA::operator!=(const EA x) const
{
    return this->ea != x.ea;
}

bool EA::operator>(const EA x) const
{
    return this->ea > x.ea;
}

bool EA::operator<(const EA x) const
{
    return this->ea < x.ea;
}

EA EA::operator+(const EA x) const
{
    return EA(this->ea + x.ea);
}

EA EA::operator+=(const EA x)
{
    return EA(this->ea += x.ea);
}

EA EA::operator-(const EA x) const
{
    return EA(this->ea - x.ea);
}

EA EA::operator-=(const EA x)
{
    return EA(this->ea -= x.ea);
}

EA::operator std::string() const
{
    std::stringstream ss;
    ss << std::hex << this->ea;
    return ss.str();
}

void EA::serialize(boost::archive::polymorphic_iarchive& ar, const unsigned int)
{
    ar & this->ea;
}

void EA::serialize(boost::archive::polymorphic_oarchive& ar, const unsigned int) const
{
    ar & this->ea;
}
