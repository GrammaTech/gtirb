#pragma once

#include <gtirb/ModuleSectionBase.hpp>

namespace gtirb
{
    ///
    /// \class ModuleSummary
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API ModuleSummary : public ModuleSectionBase
    {
    public:
        ModuleSummary();
        ~ModuleSummary() override = default;

        void setName(std::string x);
        std::string getName() const;

        void setDecodeMode(uint64_t x);
        uint64_t getDecodeMode() const;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar& boost::serialization::base_object<ModuleSectionBase>(*this);
            ar & this->name;
            ar & this->decodeMode;
        }

    private:
        std::string name{};
        uint64_t decodeMode{0};
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::ModuleSummary);
