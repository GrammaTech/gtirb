#pragma once

#include <gtirb/ModuleSectionBase.hpp>

namespace gtirb
{
    ///
    /// \class ModuleSummary
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API ModuleSummary final : public ModuleSectionBase
    {
    public:
        ModuleSummary();
        virtual ~ModuleSummary() = default;

        void setName(std::string x);
        std::string getName() const;

        void setDecodeMode(uint64_t x);
        uint64_t getDecodeMode() const;

    private:
        std::string name{};
        uint64_t decodeMode{0};
    };
}
