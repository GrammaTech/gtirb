#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/NodeRef.hpp>
#include <gtirb/Symbol.hpp>
#include <cstdint>
#include <map>
#include <string>
#include <variant>

namespace proto {
class SymbolicExpression;
}
namespace gtirb {
///
/// Represents a symbolic operand of the form "StackVar + Const".
///
struct SymStackConst {
  // TODO: What's this?
  bool Negate;
  int Offset;
  int Displacement;
  NodeRef<Symbol> Sym;
};

///
/// Represents a symbolic operand of the form "Addr + Const".
///
struct SymAddrConst {
  int64_t Displacement;
  NodeRef<Symbol> Sym;
};

///
/// Represents a symbolic operand of the form "(Addr - Addr) / Scale + Offset"
///
struct SymAddrAddr {
  int64_t Scale;
  int64_t Offset;
  NodeRef<Symbol> Sym1;
  NodeRef<Symbol> Sym2;
};

using SymbolicExpression =
    std::variant<SymStackConst, SymAddrConst, SymAddrAddr>;

GTIRB_EXPORT_API void fromProtobuf(SymbolicExpression& Result,
                                   const proto::SymbolicExpression& Message);
GTIRB_EXPORT_API proto::SymbolicExpression
toProtobuf(const SymbolicExpression& Expr);

} // namespace gtirb
