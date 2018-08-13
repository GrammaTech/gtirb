#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/NodeRef.hpp>
#include <gtirb/Symbol.hpp>
#include <boost/variant.hpp>
#include <cstdint>
#include <map>
#include <string>

namespace proto {
class SymbolicExpression;
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
  NodeRef<Symbol> symbol;
};

///
/// Represents a symbolic operand of the form "Addr + Const".
///
struct SymAddrConst {
  int64_t displacement;
  NodeRef<Symbol> symbol;
};

///
/// Represents a symbolic operand of the form "(Addr - Addr) / Scale + Offset"
///
struct SymAddrAddr {
  int64_t scale;
  int64_t offset;
  NodeRef<Symbol> symbol1;
  NodeRef<Symbol> symbol2;
};

using SymbolicExpression = boost::variant<SymStackConst, SymAddrConst, SymAddrAddr>;

GTIRB_EXPORT_API void fromProtobuf(SymbolicExpression& result,
                                   const proto::SymbolicExpression& message);
GTIRB_EXPORT_API proto::SymbolicExpression toProtobuf(const SymbolicExpression& operand);

} // namespace gtirb
