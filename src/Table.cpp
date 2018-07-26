#include <proto/Table.pb.h>
#include <boost/variant/static_visitor.hpp>
#include <gtirb/Serialization.hpp>
#include <gtirb/Table.hpp>

namespace gtirb
{
    template <typename MessageT>
    class TableVisitor : public boost::static_visitor<>
    {
    public:
        MessageT* message;

        TableVisitor(MessageT* m) : message(m)
        {
        }

        void operator()(EA val) const
        {
            message->set_ea(val.get());
        }

        void operator()(int64_t val) const
        {
            message->set_int_(val);
        }

        void operator()(const std::string& val) const
        {
            message->set_str(val);
        }

        void operator()(const table::InnerMapType& val) const
        {
            containerToProtobuf(val, message->mutable_map()->mutable_contents());
        }

        void operator()(const std::vector<EA>& val) const
        {
            containerToProtobuf(val, message->mutable_ea_vector()->mutable_contents());
        }

        void operator()(const std::vector<int64_t>& val) const
        {
            containerToProtobuf(val, message->mutable_int_vector()->mutable_contents());
        }

        void operator()(const std::vector<std::string>& val) const
        {
            containerToProtobuf(val, message->mutable_string_vector()->mutable_contents());
        }

        void operator()(const std::map<EA, table::ValueType>& val) const
        {
            containerToProtobuf(val, message->mutable_by_ea()->mutable_contents());
        }

        void operator()(const std::map<int64_t, table::ValueType>& val) const
        {
            containerToProtobuf(val, message->mutable_by_int()->mutable_contents());
        }

        void operator()(const std::map<std::string, table::ValueType>& val) const
        {
            containerToProtobuf(val, message->mutable_by_string()->mutable_contents());
        }

        void operator()(const std::vector<table::InnerMapType>& val) const
        {
            auto field = message->mutable_map_vector()->mutable_contents();
            field->Clear();
            field->Reserve(val.size());
            std::for_each(val.begin(), val.end(), [field](auto v) {
                containerToProtobuf(v, field->Add()->mutable_contents());
            });
        }
    };

    proto::Value toProtobuf(const table::ValueType& value)
    {
        proto::Value message;
        boost::apply_visitor(TableVisitor<proto::Value>(&message), value);
        return message;
    }

    proto::InnerValue toProtobuf(const table::InnerValueType& value)
    {
        proto::InnerValue message;
        boost::apply_visitor(TableVisitor<proto::InnerValue>(&message), value);
        return message;
    }

    proto::Table toProtobuf(const Table& table)
    {
        proto::Table message;
        boost::apply_visitor(TableVisitor<proto::Table>(&message), table);
        return message;
    }

    void fromProtobuf(table::ValueType& value, const proto::Value& message)
    {
        switch(message.value_case())
        {
            case proto::Value::kEa:
                value = EA(message.ea());
                break;
            case proto::Value::kInt:
                value = message.int_();
                break;
            case proto::Value::kStr:
                value = message.str();
                break;
            case proto::Value::kMap:
            {
                table::InnerMapType map;
                containerFromProtobuf(map, message.map().contents());
                value = map;
                break;
            }
            case proto::Value::VALUE_NOT_SET:
                assert(false);
                break;
        }
    }

    void fromProtobuf(table::InnerValueType& value, const proto::InnerValue& message)
    {
        switch(message.value_case())
        {
            case proto::InnerValue::kEa:
                value = EA(message.ea());
                break;
            case proto::InnerValue::kInt:
                value = message.int_();
                break;
            case proto::InnerValue::kStr:
                value = message.str();
                break;
            case proto::InnerValue::kEaVector:
            {
                std::vector<EA> v;
                containerFromProtobuf(v, message.ea_vector().contents());
                value = v;
                break;
            }
            case proto::InnerValue::kIntVector:
            {
                std::vector<int64_t> v;
                containerFromProtobuf(v, message.int_vector().contents());
                value = v;
                break;
            }
            case proto::InnerValue::kStringVector:
            {
                std::vector<std::string> v;
                containerFromProtobuf(v, message.string_vector().contents());
                value = v;
                break;
            }
            case proto::InnerValue::VALUE_NOT_SET:
                assert(false);
                break;
        }
    }

    void fromProtobuf(Table& result, const proto::Table& message)
    {
        switch(message.value_case())
        {
            case proto::Table::kByEa:
            {
                std::map<EA, table::ValueType> val;
                containerFromProtobuf(val, message.by_ea().contents());
                result = std::move(val);
                break;
            }
            case proto::Table::kByInt:
            {
                std::map<int64_t, table::ValueType> val;
                containerFromProtobuf(val, message.by_int().contents());
                result = std::move(val);
                break;
            }
            case proto::Table::kByString:
            {
                std::map<std::string, table::ValueType> val;
                containerFromProtobuf(val, message.by_string().contents());
                result = std::move(val);
                break;
            }
            case proto::Table::kMapVector:
            {
                std::vector<table::InnerMapType> values;
                auto& field = message.map_vector().contents();

                std::for_each(field.begin(), field.end(), [&values](auto v) {
                    table::InnerMapType map;
                    containerFromProtobuf(map, v.contents());
                    values.push_back(std::move(map));
                });
                result = std::move(values);
                break;
            }

            case proto::Table::kEaVector:
            {
                std::vector<EA> val;
                containerFromProtobuf(val, message.ea_vector().contents());
                result = std::move(val);
                break;
            }
            case proto::Table::kIntVector:
            {
                std::vector<int64_t> val;
                containerFromProtobuf(val, message.int_vector().contents());
                result = std::move(val);
                break;
            }
            case proto::Table::kStringVector:
            {
                std::vector<std::string> val;
                containerFromProtobuf(val, message.string_vector().contents());
                result = std::move(val);
                break;
            }
            case proto::Table::VALUE_NOT_SET:
                assert(false);
                break;
        }
    }
}
