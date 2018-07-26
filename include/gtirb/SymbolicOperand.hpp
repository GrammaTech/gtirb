#pragma once

#include <boost/variant.hpp>
#include <cstdint>
#include <gtirb/EA.hpp>
#include <gtirb/Symbol.hpp>
#include <map>
#include <string>

namespace proto {
class SymbolicOperand;
}
namespace gtirb {
///
/// Represents a symbolic operand of the form "StackVar + Const".
///
struct SymStackConst {
  // TODO: What's this?
  bool negate;
  int offset;
  int displacement;
  NodeReference<Symbol> symbol;
};

///
/// Represents a symbolic operand of the form "Addr + Const".
///
struct SymAddrConst {
  int64_t displacement;
  NodeReference<Symbol> symbol;
};

///
/// Represents a symbolic operand of the form "(Addr - Addr) / Scale + Offset"
///
struct SymAddrAddr {
  int64_t scale;
  int64_t offset;
  NodeReference<Symbol> symbol1;
  NodeReference<Symbol> symbol2;
};

using SymbolicOperand = boost::variant<SymStackConst, SymAddrConst, SymAddrAddr>;

void fromProtobuf(SymbolicOperand& result, const proto::SymbolicOperand& message);
proto::SymbolicOperand toProtobuf(const SymbolicOperand& operand);

} // namespace gtirb
