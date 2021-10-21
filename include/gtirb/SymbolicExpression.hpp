//===- SymbolicExpression.hpp -----------------------------------*- C++ -*-===//
//
//  Copyright (C) 2020 GrammaTech, Inc.
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
#ifndef GTIRB_SYMBOLICEXPRESSION_H
#define GTIRB_SYMBOLICEXPRESSION_H

#include <gtirb/Addr.hpp>
#include <gtirb/proto/SymbolicExpression.pb.h>
#include <bitset>
#include <boost/range/iterator_range.hpp>
#include <cstdint>
#include <functional>
#include <map>
#include <string>
#include <variant>

/// \file SymbolicExpression.hpp
/// \ingroup SYMBOLIC_EXPRESSION_GROUP
/// \brief Types and operations for symbolic expressions.
///
/// \see \ref SYMBOLIC_EXPRESSION_GROUP.
namespace gtirb {
class Symbol; // Forward refernece for Sym, Sym1, Sym2, etc.

/// \defgroup SYMBOLIC_EXPRESSION_GROUP Symbolic Expressions and Operands
/// \brief Represent data values or instruction operands which
/// should be intepreted as referring to symbols.
/// @{

/// \enum SymAttribute
///
/// \brief The space of attributes that can be applied to a symbolic
/// expression.
///
/// See doc/general/SymbolicExpression.md for more details.
enum class SymAttribute : uint16_t {
  // Common ELF relocation labels.
  GOT = proto::SymAttribute::GOT,
  GOTPC = proto::SymAttribute::GOTPC,
  GOTOFF = proto::SymAttribute::GOTOFF,
  GOTREL = proto::SymAttribute::GOTREL,
  PLT = proto::SymAttribute::PLT,
  PLTOFF = proto::SymAttribute::PLTOFF,
  PCREL = proto::SymAttribute::PCREL,
  SECREL = proto::SymAttribute::SECREL,
  TLS = proto::SymAttribute::TLS,
  TLSGD = proto::SymAttribute::TLSGD,
  TLSLD = proto::SymAttribute::TLSLD,
  TLSLDM = proto::SymAttribute::TLSLDM,
  TLSCALL = proto::SymAttribute::TLSCALL,
  TLSDESC = proto::SymAttribute::TLSDESC,
  TPREL = proto::SymAttribute::TPREL,
  TPOFF = proto::SymAttribute::TPOFF,
  DTPREL = proto::SymAttribute::DTPREL,
  DTPOFF = proto::SymAttribute::DTPOFF,
  DTPMOD = proto::SymAttribute::DTPMOD,
  NTPOFF = proto::SymAttribute::NTPOFF,
  PAGE = proto::SymAttribute::PAGE,
  PAGEOFF = proto::SymAttribute::PAGEOFF,
  CALL = proto::SymAttribute::CALL,
  LO = proto::SymAttribute::LO,
  HI = proto::SymAttribute::HI,
  HIGHER = proto::SymAttribute::HIGHER,
  HIGHEST = proto::SymAttribute::HIGHEST,

  // X86-specific relocation labels.
  GOTNTPOFF = proto::SymAttribute::GOTNTPOFF,
  INDNTPOFF = proto::SymAttribute::INDNTPOFF,

  // ARM-specific relocation labels.
  G0 = proto::SymAttribute::G0,
  G1 = proto::SymAttribute::G1,
  G2 = proto::SymAttribute::G2,
  G3 = proto::SymAttribute::G3,
  UPPER16 = proto::SymAttribute::UPPER16,
  LOWER16 = proto::SymAttribute::LOWER16,
  LO12 = proto::SymAttribute::LO12,
  LO15 = proto::SymAttribute::LO15,
  LO14 = proto::SymAttribute::LO14,
  HI12 = proto::SymAttribute::HI12,
  HI21 = proto::SymAttribute::HI21,
  S = proto::SymAttribute::S,
  PG = proto::SymAttribute::PG,
  NC = proto::SymAttribute::NC,
  ABS = proto::SymAttribute::ABS,
  PREL = proto::SymAttribute::PREL,
  PREL31 = proto::SymAttribute::PREL31,
  TARGET1 = proto::SymAttribute::TARGET1,
  TARGET2 = proto::SymAttribute::TARGET2,
  SBREL = proto::SymAttribute::SBREL,
  TLSLDO = proto::SymAttribute::TLSLDO,

  // MIPS-specific relocation labels.
  HI16 = proto::SymAttribute::HI16,
  LO16 = proto::SymAttribute::LO16,
  GPREL = proto::SymAttribute::GPREL,
  DISP = proto::SymAttribute::DISP,
  OFST = proto::SymAttribute::OFST,

  // PPC
  H = proto::SymAttribute::H,
  L = proto::SymAttribute::L,
  HA = proto::SymAttribute::HA,
  HIGH = proto::SymAttribute::HIGH,
  HIGHA = proto::SymAttribute::HIGHA,
  HIGHERA = proto::SymAttribute::HIGHERA,
  HIGHESTA = proto::SymAttribute::HIGHESTA,
  TOCBASE = proto::SymAttribute::TOCBASE,
  TOC = proto::SymAttribute::TOC,
  NOTOC = proto::SymAttribute::NOTOC,
};

using SymAttributeSet = std::set<SymAttribute>;

/// \brief Represents a
/// \ref SYMBOLIC_EXPRESSION_GROUP "symbolic operand" of the form
/// "Sym + Offset".
struct SymAddrConst {
  int64_t Offset; ///< Constant offset.
  Symbol* Sym;    ///< Symbol representing an address.
  SymAttributeSet Attributes = SymAttributeSet();

  friend bool operator==(const SymAddrConst& LHS, const SymAddrConst& RHS) {
    return LHS.Offset == RHS.Offset && LHS.Sym == RHS.Sym &&
           LHS.Attributes == RHS.Attributes;
  }

  friend bool operator!=(const SymAddrConst& LHS, const SymAddrConst& RHS) {
    return !operator==(LHS, RHS);
  }
};

/// \brief Represents a
/// \ref SYMBOLIC_EXPRESSION_GROUP "symbolic operand" of the form
/// "(Sym1 - Sym2) / Scale + Offset"
struct SymAddrAddr {
  int64_t Scale;  ///< Constant scale factor.
  int64_t Offset; ///< Constant offset.
  Symbol* Sym1;   ///< Symbol representing the base address.
  Symbol* Sym2;   ///< Symbol to subtract from \p Sym1.
  SymAttributeSet Attributes = SymAttributeSet();

  friend bool operator==(const SymAddrAddr& LHS, const SymAddrAddr& RHS) {
    return LHS.Scale == RHS.Scale && LHS.Offset == RHS.Offset &&
           LHS.Sym1 == RHS.Sym1 && LHS.Sym2 == RHS.Sym2 &&
           LHS.Attributes == RHS.Attributes;
  }

  friend bool operator!=(const SymAddrAddr& LHS, const SymAddrAddr& RHS) {
    return !operator==(LHS, RHS);
  }
};

/// \brief A \ref SYMBOLIC_EXPRESSION_GROUP "symbolic expression".
using SymbolicExpression = std::variant<SymAddrConst, SymAddrAddr>;

/// @}
// (end \defgroup SYMBOLIC_EXPRESSION_GROUP)

} // namespace gtirb

namespace std {
template <> struct hash<gtirb::SymAddrConst> {
  typedef gtirb::SymAddrConst argument_type;
  typedef std::size_t result_type;

  result_type operator()(const argument_type& Obj) const noexcept {
    const result_type Off = std::hash<int64_t>{}(Obj.Offset);
    const result_type P = std::hash<gtirb::Symbol*>{}(Obj.Sym);
    return Off ^ (P << 1);
  }
};

template <> struct hash<gtirb::SymAddrAddr> {
  typedef gtirb::SymAddrAddr argument_type;
  typedef std::size_t result_type;

  result_type operator()(const argument_type& Obj) const noexcept {
    result_type S = std::hash<int64_t>{}(Obj.Scale);
    comb(S, std::hash<int64_t>{}(Obj.Offset));
    comb(S, std::hash<gtirb::Symbol*>{}(Obj.Sym1));
    comb(S, std::hash<gtirb::Symbol*>{}(Obj.Sym2));
    return S;
  }

private:
  void comb(result_type& One, result_type Two) const noexcept {
    One ^= Two + 0x9e3779b9 + (One << 6) + (One >> 2);
  }
};
} // namespace std

#endif // GTIRB_SYMBOLICEXPRESSION_H
