#include "EA.hpp"
#include <iostream>
#include <sstream>

using namespace gtirb;

void EA::set(uint64_t X) { this->Address = X; }

uint64_t EA::get() const { return this->Address; }

EA::operator uint64_t() const { return this->Address; }
EA::operator int64_t() const { return this->Address; }
EA::operator uint32_t() const { return static_cast<uint32_t>(this->Address); }
EA::operator int32_t() const { return static_cast<int32_t>(this->Address); }
EA::operator uint16_t() const { return static_cast<uint16_t>(this->Address); }
EA::operator int16_t() const { return static_cast<int16_t>(this->Address); }
EA::operator uint8_t() const { return static_cast<uint8_t>(this->Address); }
EA::operator int8_t() const { return static_cast<int8_t>(this->Address); }

EA& EA::operator=(const EA X) {
  this->Address = X.Address;
  return *this;
}

bool EA::operator==(uint64_t X) const { return this->Address == X; }

bool EA::operator==(EA X) const { return this->Address == X.Address; }

bool EA::operator!=(EA X) const { return this->Address != X.Address; }

bool EA::operator>(EA X) const { return this->Address > X.Address; }

bool EA::operator<(EA X) const { return this->Address < X.Address; }

EA EA::operator+(EA X) const { return EA(this->Address + X.Address); }

EA EA::operator+(uint64_t X) const { return EA(this->Address + X); }

EA& EA::operator+=(EA X) {
  this->Address += X.Address;
  return *this;
}

EA& EA::operator+=(uint64_t X) {
  this->Address += X;
  return *this;
}

EA EA::operator-(EA X) const { return EA(this->Address - X.Address); }

EA EA::operator-(uint64_t X) const { return EA(this->Address - X); }

EA& EA::operator-=(EA X) {
  this->Address -= X.Address;
  return *this;
}

EA& EA::operator-=(uint64_t X) {
  this->Address -= X;
  return *this;
}

EA::operator std::string() const {
  std::stringstream S;
  S << this;
  return S.str();
}

std::ostream& operator<<(std::ostream& Os, const EA& Ea) {
  auto Flags = Os.flags();
  Os << "0x" << std::hex << Ea.get();
  Os.flags(Flags);
  return Os;
}
