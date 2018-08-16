#include "Addr.hpp"
#include <iostream>
#include <sstream>

using namespace gtirb;

void Addr::set(uint64_t X) { this->Address = X; }

uint64_t Addr::get() const { return this->Address; }

Addr::operator uint64_t() const { return this->Address; }
Addr::operator int64_t() const { return this->Address; }
Addr::operator uint32_t() const { return static_cast<uint32_t>(this->Address); }
Addr::operator int32_t() const { return static_cast<int32_t>(this->Address); }
Addr::operator uint16_t() const { return static_cast<uint16_t>(this->Address); }
Addr::operator int16_t() const { return static_cast<int16_t>(this->Address); }
Addr::operator uint8_t() const { return static_cast<uint8_t>(this->Address); }
Addr::operator int8_t() const { return static_cast<int8_t>(this->Address); }

Addr& Addr::operator=(const Addr X) {
  this->Address = X.Address;
  return *this;
}

bool Addr::operator==(uint64_t X) const { return this->Address == X; }

bool Addr::operator==(Addr X) const { return this->Address == X.Address; }

bool Addr::operator!=(Addr X) const { return this->Address != X.Address; }

bool Addr::operator>(Addr X) const { return this->Address > X.Address; }

bool Addr::operator<(Addr X) const { return this->Address < X.Address; }

Addr Addr::operator+(Addr X) const { return Addr(this->Address + X.Address); }

Addr Addr::operator+(uint64_t X) const { return Addr(this->Address + X); }

Addr& Addr::operator+=(Addr X) {
  this->Address += X.Address;
  return *this;
}

Addr& Addr::operator+=(uint64_t X) {
  this->Address += X;
  return *this;
}

Addr Addr::operator-(Addr X) const { return Addr(this->Address - X.Address); }

Addr Addr::operator-(uint64_t X) const { return Addr(this->Address - X); }

Addr& Addr::operator-=(Addr X) {
  this->Address -= X.Address;
  return *this;
}

Addr& Addr::operator-=(uint64_t X) {
  this->Address -= X;
  return *this;
}

Addr::operator std::string() const {
  std::stringstream S;
  S << this;
  return S.str();
}

std::ostream& operator<<(std::ostream& Os, const Addr& Ea) {
  auto Flags = Os.flags();
  Os << "0x" << std::hex << Ea.get();
  Os.flags(Flags);
  return Os;
}
