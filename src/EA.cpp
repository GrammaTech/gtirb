#include <gtirb/EA.hpp>
#include <iostream>
#include <sstream>

using namespace gtirb;

void EA::set(uint64_t x) { this->ea = x; }

uint64_t EA::get() const { return this->ea; }

EA::operator uint64_t() const { return this->ea; }

EA& EA::operator=(EA x) {
  this->ea = x.ea;
  return *this;
}

bool EA::operator==(uint64_t x) const { return this->ea == x; }

bool EA::operator==(const EA x) const { return this->ea == x.ea; }

bool EA::operator!=(const EA x) const { return this->ea != x.ea; }

bool EA::operator>(const EA x) const { return this->ea > x.ea; }

bool EA::operator<(const EA x) const { return this->ea < x.ea; }

EA EA::operator+(const EA x) const { return EA(this->ea + x.ea); }

EA EA::operator+=(const EA x) { return EA(this->ea += x.ea); }

EA EA::operator-(const EA x) const { return EA(this->ea - x.ea); }

EA EA::operator-=(const EA x) { return EA(this->ea -= x.ea); }

EA::operator std::string() const {
  std::stringstream ss;
  ss << this;
  return ss.str();
}

std::ostream& operator<<(std::ostream& os, const EA& ea) {
  auto flags = os.flags();
  os << "0x" << std::hex << ea.get();
  os.flags(flags);
  return os;
}
