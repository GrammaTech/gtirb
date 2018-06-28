#pragma once

#include <boost/serialization/export.hpp>
#include <boost/variant.hpp>
#include <cstdint>
#include <gtirb/EA.hpp>
#include <gtirb/Symbol.hpp>
#include <map>
#include <string>

namespace gtirb
{
    ///
    /// Represents a symbolic operand of the form "StackVar + Const".
    ///
    struct SymStackConst
    {
        // TODO: What's this?
        bool negate;
        int offset;
        int displacement;
        SymbolReference symbol;

        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);
    };

    ///
    /// Represents a symbolic operand of the form "Addr + Const".
    ///
    struct SymAddrConst
    {
        int64_t displacement;
        SymbolReference symbol;

        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);
    };

    ///
    /// Represents a symbolic operand of the form "(Addr - Addr) / Scale + Offset"
    ///
    struct SymAddrAddr
    {
        int64_t scale;
        int64_t offset;
        SymbolReference symbol1;
        SymbolReference symbol2;

        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);
    };

    using SymbolicOperand = boost::variant<SymStackConst, SymAddrConst, SymAddrAddr>;
} // namespace gtirb

BOOST_CLASS_EXPORT_KEY(gtirb::SymStackConst);
BOOST_CLASS_EXPORT_KEY(gtirb::SymAddrConst);
BOOST_CLASS_EXPORT_KEY(gtirb::SymAddrAddr);
