#pragma once

#include <boost/serialization/string.hpp>
#include <cstdint>
#include <gtirb/EA.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Table.hpp>

namespace gtirb
{
    class Section;
    class EA;

    /// \class SectionTable
    /// \author Nathan Weston
    class GTIRB_GTIRB_EXPORT_API SectionTable : public Table
    {
    public:
        typedef std::map<EA, Section> SectionMap;
        typedef SectionMap::const_iterator const_iterator;

        virtual size_t size() const override;
        virtual void clear() override;
        virtual void serialize(boost::archive::polymorphic_iarchive& ar,
                               const unsigned int version = 0) override;
        virtual void serialize(boost::archive::polymorphic_oarchive& ar,
                               const unsigned int version = 0) const override;
        const_iterator begin() const noexcept;
        const_iterator end() const noexcept;
        const_iterator find(const EA& startAddress) const;

        void addSection(Section s);

    private:
        SectionMap data;
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::SectionTable);
