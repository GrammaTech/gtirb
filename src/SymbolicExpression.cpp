#include <proto/SymbolicExpression.pb.h>
#include <boost/variant/static_visitor.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include "Serialization.hpp"

namespace gtirb {
class SymbolicVisitor : public boost::static_visitor<> {
public:
  proto::SymbolicExpression* message;

  SymbolicVisitor(proto::SymbolicExpression* m) : message(m) {}

  void operator()(const SymStackConst& val) const {
    auto m = message->mutable_stack_const();
    m->set_negate(val.negate);
    m->set_offset(val.offset);
    m->set_displacement(val.displacement);
    uuidToBytes(val.symbol.getUUID(), *m->mutable_symbol_uuid());
  }

  void operator()(const SymAddrConst& val) const {
    auto m = message->mutable_addr_const();
    m->set_displacement(val.displacement);
    uuidToBytes(val.symbol.getUUID(), *m->mutable_symbol_uuid());
  }

  void operator()(const SymAddrAddr& val) const {
    auto m = message->mutable_addr_addr();
    m->set_scale(val.scale);
    m->set_offset(val.offset);
    uuidToBytes(val.symbol1.getUUID(), *m->mutable_symbol1_uuid());
    uuidToBytes(val.symbol2.getUUID(), *m->mutable_symbol2_uuid());
  }
};

proto::SymbolicExpression toProtobuf(const SymbolicExpression& operand) {
  proto::SymbolicExpression message;
  boost::apply_visitor(SymbolicVisitor(&message), operand);
  return message;
}

void fromProtobuf(SymbolicExpression& result, const proto::SymbolicExpression& message) {
  switch (message.value_case()) {
  case proto::SymbolicExpression::kStackConst: {
    auto val = message.stack_const();
    result = SymStackConst{val.negate(), val.offset(), val.displacement(),
                           uuidFromBytes(val.symbol_uuid())};
    break;
  }
  case proto::SymbolicExpression::kAddrConst: {
    auto val = message.addr_const();
    result = SymAddrConst{val.displacement(), uuidFromBytes(val.symbol_uuid())};
    break;
  }
  case proto::SymbolicExpression::kAddrAddr: {
    auto val = message.addr_addr();
    result = SymAddrAddr{val.scale(), val.offset(), uuidFromBytes(val.symbol1_uuid()),
                         uuidFromBytes(val.symbol2_uuid())};
    break;
  }
  case proto::SymbolicExpression::VALUE_NOT_SET:
    assert(false);
  }
}
}
