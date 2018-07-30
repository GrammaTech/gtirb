#pragma once

#include <gtirb/EA.hpp>
#include <boost/variant.hpp>
#include <map>
#include <string>
#include <vector>

namespace proto {
class Table;
}

namespace gtirb {
namespace table {
using InnerValueType = boost::variant<EA,                   //
                                      int64_t,              //
                                      std::string,          //
                                      std::vector<EA>,      //
                                      std::vector<int64_t>, //
                                      std::vector<std::string>>;

/// Table values can also be maps, but they can only store a limited
/// set of types.
using InnerMapType = std::map<std::string, InnerValueType>;

/// Table values can be any of these types.
using ValueType = boost::variant<EA,          //
                                 int64_t,     //
                                 std::string, //
                                 InnerMapType>;
} // namespace table

///
/// \class Table
///
/// A generic table for storing additional, client-specific data.
///
using Table = boost::variant<std::map<EA, table::ValueType>,          //
                             std::map<int64_t, table::ValueType>,     //
                             std::map<std::string, table::ValueType>, //
                             std::vector<table::InnerMapType>,        //
                             std::vector<EA>,                         //
                             std::vector<int64_t>,                    //
                             std::vector<std::string>>;

proto::Table toProtobuf(const Table& table);
void fromProtobuf(Table& result, const proto::Table& message);
} // namespace gtirb
