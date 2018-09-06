#include "SymbolicExpression.hpp"
#include "Serialization.hpp"
#include <gtirb/Context.hpp>
#include <gtirb/Symbol.hpp>
#include <proto/SymbolicExpression.pb.h>
#include <variant>

namespace gtirb {
class SymbolicVisitor {
public:
  proto::SymbolicExpression* Message;

  SymbolicVisitor(proto::SymbolicExpression* M) : Message(M) {}

  void operator()(const SymStackConst& Val) const {
    auto M = Message->mutable_stack_const();
    M->set_negate(Val.Negate);
    M->set_offset(Val.Offset);
    M->set_displacement(Val.Displacement);
    uuidToBytes(Val.Sym.getUUID(), *M->mutable_symbol_uuid());
  }

  void operator()(const SymAddrConst& Val) const {
    auto M = Message->mutable_addr_const();
    M->set_displacement(Val.Displacement);
    uuidToBytes(Val.Sym.getUUID(), *M->mutable_symbol_uuid());
  }

  void operator()(const SymAddrAddr& Val) const {
    auto M = Message->mutable_addr_addr();
    M->set_scale(Val.Scale);
    M->set_offset(Val.Offset);
    uuidToBytes(Val.Sym1.getUUID(), *M->mutable_symbol1_uuid());
    uuidToBytes(Val.Sym2.getUUID(), *M->mutable_symbol2_uuid());
  }
};

proto::SymbolicExpression toProtobuf(const SymbolicExpression& Value) {
  proto::SymbolicExpression Message;
  std::visit(SymbolicVisitor(&Message), Value);
  return Message;
}

void fromProtobuf(Context&, SymbolicExpression& Result,
                  const proto::SymbolicExpression& Message) {
  switch (Message.value_case()) {
  case proto::SymbolicExpression::kStackConst: {
    auto Val = Message.stack_const();
    Result = SymStackConst{Val.negate(), Val.offset(), Val.displacement(),
                           uuidFromBytes(Val.symbol_uuid())};
    break;
  }
  case proto::SymbolicExpression::kAddrConst: {
    auto Val = Message.addr_const();
    Result = SymAddrConst{Val.displacement(), uuidFromBytes(Val.symbol_uuid())};
    break;
  }
  case proto::SymbolicExpression::kAddrAddr: {
    auto Val = Message.addr_addr();
    Result = SymAddrAddr{Val.scale(), Val.offset(),
                         uuidFromBytes(Val.symbol1_uuid()),
                         uuidFromBytes(Val.symbol2_uuid())};
    break;
  }
  case proto::SymbolicExpression::VALUE_NOT_SET:
    assert(false);
  }
}
} // namespace gtirb
