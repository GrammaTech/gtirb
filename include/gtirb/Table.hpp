//===- Table.hpp ------------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018 GrammaTech, Inc.
//
//  This code is licensed under the MIT license. See the LICENSE file in the
//  project root for license terms.
//
//  This project is sponsored by the Office of Naval Research, One Liberty
//  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
//  N68335-17-C-0700.  The content of the information does not necessarily
//  reflect the position or policy of the Government and no official
//  endorsement should be inferred.
//
//===----------------------------------------------------------------------===//
#ifndef GTIRB_TABLE_H
#define GTIRB_TABLE_H

#include <gtirb/Addr.hpp>
#include <gtirb/Block.hpp>
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

/// \brief DOCFIXME
using InnerValueType = std::variant<Addr,                     //
                                    int64_t,                  //
                                    std::string,              //
                                    UUID,                     //
                                    InstructionRef,           //
                                    std::vector<Addr>,        //
                                    std::vector<int64_t>,     //
                                    std::vector<std::string>, //
                                    std::vector<UUID>,        //
                                    std::vector<InstructionRef>>;

/// \brief Table values can also be maps, but they can only store a
/// limited set of types.
using InnerMapType = std::map<std::string, InnerValueType>;

/// \brief Table values can be any of these types.
using ValueType = std::variant<Addr,           //
                               int64_t,        //
                               std::string,    //
                               UUID,           //
                               InstructionRef, //
                               InnerMapType>;
} // namespace table

/// \class Table
///
/// \brief A generic table for storing additional, client-specific data.
using Table = std::variant<std::map<Addr, table::ValueType>,        //
                           std::map<int64_t, table::ValueType>,     //
                           std::map<std::string, table::ValueType>, //
                           std::map<UUID, table::ValueType>,        //
                           std::vector<table::InnerMapType>,        //
                           std::vector<Addr>,                       //
                           std::vector<int64_t>,                    //
                           std::vector<std::string>,                //
                           std::vector<UUID>,                       //
                           std::vector<InstructionRef>>;

/// \brief DOCFIXME
///
/// \param Table DOCFIXME
///
/// \return DOCFIXME
GTIRB_EXPORT_API proto::Table toProtobuf(const Table& Table);

///
/// \brief DOCFIXME
///
/// \param C        DOCFIXME
/// \param Result   DOCFIXME
/// \param Message  DOCFIXME
///
/// \return void
GTIRB_EXPORT_API void fromProtobuf(Context& C, Table& Result,
                                   const proto::Table& Message);
} // namespace gtirb

#endif // GTIRB_TABLE_H
