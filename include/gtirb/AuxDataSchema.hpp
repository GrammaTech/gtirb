//===- AuxDataSchema.hpp ---------------------------------------------*-
// C++-*-===//
//
//  Copyright (C) 2018-2019 GrammaTech, Inc.
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
#ifndef GTIRB_AUXDATASCHEMA_HPP
#define GTIRB_AUXDATASCHEMA_HPP

#include <gtirb/Addr.hpp>
#include <gtirb/Context.hpp> // UUID
#include <cstdint>
#include <map>
#include <string>

/// \file AuxDataSchema.hpp
/// \ingroup AUXDATA_GROUP
/// \brief  Type schema for sanctioned AuxData types
/// \see AUXDATA_GROUP

namespace gtirb {
namespace schema {

/// \brief Schema class for functionBlocks auxiliary data.
struct FunctionBlocks {
  static constexpr const char* Name = "functionBlocks";
  typedef std::map<gtirb::UUID, std::set<gtirb::UUID>> Type;
};

/// \brief Schema class for functionEntries auxiliary data.
struct FunctionEntries {
  static constexpr const char* Name = "functionEntries";
  typedef std::map<gtirb::UUID, std::set<gtirb::UUID>> Type;
};

/// \brief Schema class for functionNames auxiliary data.
struct FunctionNames {
  static constexpr const char* Name = "functionNames";
  typedef std::map<gtirb::UUID, gtirb::UUID> Type;
};

/// \brief Schema class for types auxiliary data.
struct Types {
  static constexpr const char* Name = "types";
  typedef std::map<gtirb::UUID, std::string> Type;
};

/// \brief Schema class for alignment auxiliary data.
struct Alignment {
  static constexpr const char* Name = "alignment";
  typedef std::map<gtirb::UUID, uint64_t> Type;
};

/// \brief Schema class for comments auxiliary data.
struct Comments {
  static constexpr const char* Name = "comments";
  typedef std::map<gtirb::Offset, std::string> Type;
};

/// \brief Schema class for symbolForwarding auxiliary data.
struct SymbolForwarding {
  static constexpr const char* Name = "symbolForwarding";
  typedef std::map<gtirb::UUID, gtirb::UUID> Type;
};

/// \brief Schema class for padding auxiliary data.
struct Padding {
  static constexpr const char* Name = "padding";
  typedef std::map<gtirb::Offset, uint64_t> Type;
};

} // namespace schema
} // namespace gtirb

#endif // GTIRB_AUXDATASCHEMA_HPP
