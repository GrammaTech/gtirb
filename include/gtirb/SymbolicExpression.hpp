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
enum class SymAttribute : uint8_t {
  Part0 = proto::SEAttributeFlag::Part0,
  Part1 = proto::SEAttributeFlag::Part1,
  Part2 = proto::SEAttributeFlag::Part2,
  Part3 = proto::SEAttributeFlag::Part3,
  Adjusted = proto::SEAttributeFlag::Adjusted,
  Got = proto::SEAttributeFlag::Got,
  GotRelPC = proto::SEAttributeFlag::GotRelPC,
  GotRelGot = proto::SEAttributeFlag::GotRelGot,
  AddrRelGot = proto::SEAttributeFlag::AddrRelGot,
  GotRelAddr = proto::SEAttributeFlag::GotRelAddr,
  GotPage = proto::SEAttributeFlag::GotPage,
  GotPageOfst = proto::SEAttributeFlag::GotPageOfst,
  PltCall = proto::SEAttributeFlag::PltCall,
  PltRef = proto::SEAttributeFlag::PltRef,

  Max = PltRef
};

/// \brief A class for tracking a set of boolean flags that represent attributes
/// for SymbolicExpressions.
class SymAttributeSet {
public:
  /// \brief Adds the flag to the SymbolicExpression.
  ///
  /// \param F The flag to be added.
  void addFlag(SymAttribute F) {
    size_t index = static_cast<size_t>(F);
    assert(index <= static_cast<size_t>(SymAttribute::Max));
    Flags.set(index);
  }

  /// \brief Adds all of the flags to the SymbolicExpression.
  /// \tparam Fs A pack of \ref Attribute flags.
  /// \param F The flags to be added to the SymbolicExpression.
  template <typename... Fs> void addFlags(Fs... F) { (addFlag(F), ...); }

  /// \brief Removes the flag from the SymbolicExpression.
  ///
  /// \param F The flag to be removed.
  void removeFlag(SymAttribute F) {
    size_t index = static_cast<size_t>(F);
    assert(index <= static_cast<size_t>(SymAttribute::Max));
    Flags.reset(index);
  }

  /// \brief Tests whether the given flag is set for the SymbolicExpression.
  ///
  /// \param F The flag to test.
  /// \return true if the flag is set, false otherwise.
  bool isFlagSet(SymAttribute F) const {
    size_t index = static_cast<size_t>(F);
    assert(index <= static_cast<size_t>(SymAttribute::Max));
    return Flags.test(index);
  }

  friend bool operator==(const SymAttributeSet& LHS,
                         const SymAttributeSet& RHS) {
    return LHS.Flags == RHS.Flags;
  }

  /// \brief Iterator over \ref Attribute flags.
  class const_iterator
      : public boost::iterator_facade<const_iterator, SymAttribute,
                                      boost::bidirectional_traversal_tag,
                                      SymAttribute> {
  public:
    SymAttribute dereference() const {
      assert(CurrIndex <= static_cast<size_t>(SymAttribute::Max));
      assert(SASet.Flags.test(CurrIndex));
      return static_cast<SymAttribute>(CurrIndex);
    }

    bool equal(const const_iterator& other) const {
      return SASet == other.SASet && CurrIndex == other.CurrIndex;
    }

    void increment() {
      assert(CurrIndex <= static_cast<size_t>(SymAttribute::Max));
      moveToNextBit();
    }

    void decrement() {
      assert(CurrIndex > 0);
      moveToPreviousBit();
    }

  private:
    const_iterator(const SymAttributeSet& SASet_, size_t start)
        : SASet(SASet_), CurrIndex(start) {
      if (start <= static_cast<size_t>(SymAttribute::Max) &&
          !SASet.Flags.test(CurrIndex))
        moveToNextBit();
    }

    void moveToNextBit() {
      if (CurrIndex <= static_cast<size_t>(SymAttribute::Max)) {
        do {
          ++CurrIndex;
        } while (CurrIndex <= static_cast<size_t>(SymAttribute::Max) &&
                 !SASet.Flags.test(CurrIndex));
      }
    }

    void moveToPreviousBit() {
      size_t NewIndex = CurrIndex;
      while (NewIndex > 0) {
        --NewIndex;
        if (SASet.Flags.test(NewIndex))
          break;
      }
      // Assert if there is no earlier bit.
      // This would indicate an attempt to move before begin().
      assert(SASet.Flags.test(NewIndex));
      if (SASet.Flags.test(NewIndex)) {
        CurrIndex = NewIndex;
      }
    }

    const SymAttributeSet& SASet;
    size_t CurrIndex;

    friend class SymAttributeSet;
  };

  /// \brief Range of \ref Attribute flags.
  using const_range = boost::iterator_range<const_iterator>;

  /// \brief Return a const iterator to the first \ref Attribute.
  const_iterator begin() const { return const_iterator(*this, 0); }

  /// \brief Return a const iterator to the element following the last \ref
  /// Attribute.
  const_iterator end() const {
    return const_iterator(*this, static_cast<size_t>(SymAttribute::Max) + 1);
  }

  /// \brief Return a range of the \ref SymAttribute flags set for the
  /// SymbolicExpression.
  const_range flags() const {
    return boost::make_iterator_range(begin(), end());
  }

private:
  std::bitset<static_cast<size_t>(SymAttribute::Max) + 1> Flags;

  friend class const_iterator;
};

/// \brief Represents a
/// \ref SYMBOLIC_EXPRESSION_GROUP "symbolic operand" of the form
/// "Sym + Offset", representing an offset from a stack variable.
struct SymStackConst {
  int Offset;  ///< Constant offset.
  Symbol* Sym; ///< Symbol representing a stack variable.
  SymAttributeSet Attributes = SymAttributeSet();

  friend bool operator==(const SymStackConst& LHS, const SymStackConst& RHS) {
    return LHS.Offset == RHS.Offset && LHS.Sym == RHS.Sym &&
           LHS.Attributes == RHS.Attributes;
  }

  friend bool operator!=(const SymStackConst& LHS, const SymStackConst& RHS) {
    return !operator==(LHS, RHS);
  }
};

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
using SymbolicExpression =
    std::variant<SymStackConst, SymAddrConst, SymAddrAddr>;

/// @}
// (end \defgroup SYMBOLIC_EXPRESSION_GROUP)

} // namespace gtirb

namespace std {
template <> struct hash<gtirb::SymStackConst> {
  typedef gtirb::SymStackConst argument_type;
  typedef std::size_t result_type;

  result_type operator()(const argument_type& Obj) const noexcept {
    const result_type Off = std::hash<int>{}(Obj.Offset);
    const result_type P = std::hash<gtirb::Symbol*>{}(Obj.Sym);
    return Off ^ (P << 1);
  }
};

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
