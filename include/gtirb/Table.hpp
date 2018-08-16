#pragma once

#include <gtirb/Block.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <map>
#include <string>
#include <variant>
#include <vector>

namespace proto {
class Table;
}

namespace gtirb {
class Context;

namespace table {
using InnerValueType = std::variant<EA,                       //
                                    int64_t,                  //
                                    std::string,              //
                                    UUID,                     //
                                    InstructionRef,           //
                                    std::vector<EA>,          //
                                    std::vector<int64_t>,     //
                                    std::vector<std::string>, //
                                    std::vector<UUID>,        //
                                    std::vector<InstructionRef>>;

/// Table values can also be maps, but they can only store a limited
/// set of types.
using InnerMapType = std::map<std::string, InnerValueType>;

/// Table values can be any of these types.
using ValueType = std::variant<EA,             //
                               int64_t,        //
                               std::string,    //
                               UUID,           //
                               InstructionRef, //
                               InnerMapType>;
} // namespace table

///
/// \class Table
///
/// A generic table for storing additional, client-specific data.
///
using Table = std::variant<std::map<EA, table::ValueType>,          //
                           std::map<int64_t, table::ValueType>,     //
                           std::map<std::string, table::ValueType>, //
                           std::map<UUID, table::ValueType>,        //
                           std::vector<table::InnerMapType>,        //
                           std::vector<EA>,                         //
                           std::vector<int64_t>,                    //
                           std::vector<std::string>,                //
                           std::vector<UUID>,                       //
                           std::vector<InstructionRef>>;

GTIRB_EXPORT_API proto::Table toProtobuf(const Table& Table);
GTIRB_EXPORT_API void fromProtobuf(Context& C, Table& Result,
                                   const proto::Table& Message);
} // namespace gtirb
