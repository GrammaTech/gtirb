#include "Table.hpp"
#include "Serialization.hpp"
#include <proto/Table.pb.h>
#include <boost/variant/static_visitor.hpp>

namespace gtirb {
template <typename MessageT>
class TableVisitor : public boost::static_visitor<> {
public:
  MessageT* Message;

  TableVisitor(MessageT* m) : Message(m) {}

  void operator()(EA Val) const { Message->set_ea(Val.get()); }

  void operator()(int64_t Val) const { Message->set_int_(Val); }

  void operator()(const std::string& Val) const { Message->set_str(Val); }

  void operator()(const UUID& Val) const {
    uuidToBytes(Val, *Message->mutable_uuid());
  }

  void operator()(const InstructionRef& Val) const {
    Val.toProtobuf(Message->mutable_instruction());
  }

  void operator()(const table::InnerMapType& Val) const {
    containerToProtobuf(Val, Message->mutable_map()->mutable_contents());
  }

  void operator()(const std::vector<EA>& Val) const {
    containerToProtobuf(Val, Message->mutable_ea_vector()->mutable_contents());
  }

  void operator()(const std::vector<int64_t>& Val) const {
    containerToProtobuf(Val, Message->mutable_int_vector()->mutable_contents());
  }

  void operator()(const std::vector<std::string>& Val) const {
    containerToProtobuf(Val,
                        Message->mutable_string_vector()->mutable_contents());
  }

  void operator()(const std::vector<UUID>& Val) const {
    containerToProtobuf(Val,
                        Message->mutable_uuid_vector()->mutable_contents());
  }

  void operator()(const std::vector<InstructionRef>& Val) const {
    containerToProtobuf(
        Val, Message->mutable_instruction_vector()->mutable_contents());
  }

  void operator()(const std::map<EA, table::ValueType>& Val) const {
    containerToProtobuf(Val, Message->mutable_by_ea()->mutable_contents());
  }

  void operator()(const std::map<int64_t, table::ValueType>& Val) const {
    containerToProtobuf(Val, Message->mutable_by_int()->mutable_contents());
  }

  void operator()(const std::map<std::string, table::ValueType>& Val) const {
    containerToProtobuf(Val, Message->mutable_by_string()->mutable_contents());
  }

  void operator()(const std::map<UUID, table::ValueType>& Val) const {
    // Special case, convert UUIDs to string form to make protobuf happy.
    auto Field = Message->mutable_by_uuid()->mutable_contents();
    Field->clear();
    std::for_each(Val.begin(), Val.end(), [Field](auto V) {
      (*Field)[uuidToString(V.first)] = toProtobuf(V.second);
    });
  }

  void operator()(const std::vector<table::InnerMapType>& Val) const {
    auto Field = Message->mutable_map_vector()->mutable_contents();
    Field->Clear();
    Field->Reserve(static_cast<int>(Val.size()));
    std::for_each(Val.begin(), Val.end(), [Field](auto V) {
      containerToProtobuf(V, Field->Add()->mutable_contents());
    });
  }
};

proto::Value toProtobuf(const table::ValueType& Value) {
  proto::Value Message;
  boost::apply_visitor(TableVisitor<proto::Value>(&Message), Value);
  return Message;
}

proto::InnerValue toProtobuf(const table::InnerValueType& Value) {
  proto::InnerValue Message;
  boost::apply_visitor(TableVisitor<proto::InnerValue>(&Message), Value);
  return Message;
}

proto::Table toProtobuf(const Table& table) {
  proto::Table Message;
  boost::apply_visitor(TableVisitor<proto::Table>(&Message), table);
  return Message;
}

void fromProtobuf(table::ValueType& Value, const proto::Value& Message) {
  switch (Message.value_case()) {
  case proto::Value::kEa:
    Value = EA(Message.ea());
    break;
  case proto::Value::kInt:
    Value = Message.int_();
    break;
  case proto::Value::kStr:
    Value = Message.str();
    break;
  case proto::Value::kUuid:
    Value = uuidFromBytes(Message.uuid());
    break;
  case proto::Value::kInstruction: {
    InstructionRef Ref;
    fromProtobuf(Ref, Message.instruction());
    Value = std::move(Ref);
    break;
  }
  case proto::Value::kMap: {
    table::InnerMapType Map;
    containerFromProtobuf(Map, Message.map().contents());
    Value = std::move(Map);
    break;
  }
  case proto::Value::VALUE_NOT_SET:
    assert(false);
    break;
  }
}

void fromProtobuf(table::InnerValueType& Value,
                  const proto::InnerValue& Message) {
  switch (Message.value_case()) {
  case proto::InnerValue::kEa:
    Value = EA(Message.ea());
    break;
  case proto::InnerValue::kInt:
    Value = Message.int_();
    break;
  case proto::InnerValue::kStr:
    Value = Message.str();
    break;
  case proto::InnerValue::kUuid:
    Value = uuidFromBytes(Message.uuid());
    break;
  case proto::InnerValue::kInstruction: {
    InstructionRef Ref;
    fromProtobuf(Ref, Message.instruction());
    Value = std::move(Ref);
    break;
  }
  case proto::InnerValue::kEaVector: {
    std::vector<EA> V;
    containerFromProtobuf(V, Message.ea_vector().contents());
    Value = std::move(V);
    break;
  }
  case proto::InnerValue::kIntVector: {
    std::vector<int64_t> V;
    containerFromProtobuf(V, Message.int_vector().contents());
    Value = std::move(V);
    break;
  }
  case proto::InnerValue::kStringVector: {
    std::vector<std::string> V;
    containerFromProtobuf(V, Message.string_vector().contents());
    Value = std::move(V);
    break;
  }
  case proto::InnerValue::kUuidVector: {
    std::vector<UUID> V;
    containerFromProtobuf(V, Message.uuid_vector().contents());
    Value = std::move(V);
    break;
  }
  case proto::InnerValue::kInstructionVector: {
    std::vector<InstructionRef> v;
    containerFromProtobuf(v, Message.instruction_vector().contents());
    Value = std::move(v);
    break;
  }

  case proto::InnerValue::VALUE_NOT_SET:
    assert(false);
    break;
  }
}

void fromProtobuf(Table& Result, const proto::Table& Message) {
  switch (Message.value_case()) {
  case proto::Table::kByEa: {
    std::map<EA, table::ValueType> Val;
    containerFromProtobuf(Val, Message.by_ea().contents());
    Result = std::move(Val);
    break;
  }
  case proto::Table::kByInt: {
    std::map<int64_t, table::ValueType> Val;
    containerFromProtobuf(Val, Message.by_int().contents());
    Result = std::move(Val);
    break;
  }
  case proto::Table::kByString: {
    std::map<std::string, table::ValueType> Val;
    containerFromProtobuf(Val, Message.by_string().contents());
    Result = std::move(Val);
    break;
  }
  case proto::Table::kByUuid: {
    // Special case, UUIDs stored in string form by protobuf
    std::map<UUID, table::ValueType> Val;
    const auto& field = Message.by_uuid().contents();
    std::for_each(field.begin(), field.end(), [&Val](const auto& M) {
      table::ValueType V;
      fromProtobuf(V, M.second);
      Val.emplace(uuidFromString(M.first), std::move(V));
    });
    Result = std::move(Val);
    break;
  }
  case proto::Table::kMapVector: {
    std::vector<table::InnerMapType> Values;
    auto& field = Message.map_vector().contents();

    std::for_each(field.begin(), field.end(), [&Values](auto V) {
      table::InnerMapType Map;
      containerFromProtobuf(Map, V.contents());
      Values.push_back(std::move(Map));
    });
    Result = std::move(Values);
    break;
  }

  case proto::Table::kEaVector: {
    std::vector<EA> Val;
    containerFromProtobuf(Val, Message.ea_vector().contents());
    Result = std::move(Val);
    break;
  }
  case proto::Table::kIntVector: {
    std::vector<int64_t> Val;
    containerFromProtobuf(Val, Message.int_vector().contents());
    Result = std::move(Val);
    break;
  }
  case proto::Table::kStringVector: {
    std::vector<std::string> Val;
    containerFromProtobuf(Val, Message.string_vector().contents());
    Result = std::move(Val);
    break;
  }
  case proto::Table::kUuidVector: {
    std::vector<UUID> V;
    containerFromProtobuf(V, Message.uuid_vector().contents());
    Result = std::move(V);
    break;
  }
  case proto::Table::kInstructionVector: {
    std::vector<InstructionRef> V;
    containerFromProtobuf(V, Message.instruction_vector().contents());
    Result = std::move(V);
    break;
  }

  case proto::Table::VALUE_NOT_SET:
    assert(false);
    break;
  }
}
} // namespace gtirb
