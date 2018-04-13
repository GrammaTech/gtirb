#pragma once

#include <cstdint>
#include <gtirb/Constants.hpp>
#include <gtirb/Export.hpp>
#include <limits>
#include <string>

namespace gtirb
{
    ///
    /// \class EA
    ///
    /// A special class to store an Effective Address.
    /// This is initialized to `gtirb::constants::BadAddress`.
    /// It is compatible with a uint64_t for 64-bit address storage.
    ///
    class GTIRB_GTIRB_EXPORT_API EA
    {
    public:
        EA() = default;

        explicit EA(uint64_t x);

        void set(uint64_t x);
        uint64_t get() const;

        EA operator=(EA);

        bool operator==(uint64_t x) const;
        bool operator==(EA x) const;
        bool operator!=(EA x) const;
        bool operator>(EA x) const;
        bool operator<(EA x) const;

        EA operator+(EA x) const;
        EA operator+=(EA x);
        EA operator-(EA x) const;
        EA operator-=(EA x);

        operator std::string() const;

    private:
        // Prevent automatic type conversions.
        template <typename T>
        void set(T);
        template <typename T>
        EA operator+(T);
        template <typename T>
        EA operator+=(T);
        template <typename T>
        EA operator-(T);
        template <typename T>
        EA operator-=(T);

        uint64_t ea{gtirb::constants::BadAddress};
    };
} // namespace gtirb

inline bool operator==(const uint64_t rhs, const gtirb::EA lhs)
{
    return gtirb::EA(rhs) == lhs;
}
