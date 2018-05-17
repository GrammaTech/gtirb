#include <gtest/gtest.h>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/SectionTable.hpp>
#include <sstream>

using namespace gtirb;

TEST(Unit_SectionTable, emptyTable)
{
    SectionTable table;
    EXPECT_EQ(size_t{0}, table.size());
    EXPECT_EQ(table.end(), table.find(EA{}));
}

TEST(Unit_SectionTable, addSection)
{
    SectionTable table;
    table.addSection({".text", 1234, EA{}});

    EXPECT_EQ(size_t{1}, table.size());
    EXPECT_EQ(table.begin(), table.find(EA{}));
}

TEST(Unit_SectionTable, find)
{
    SectionTable table;
    Section text{".text", 100, EA{1234}}, data{".data", 200, EA{1334}};
    table.addSection(text);
    table.addSection(data);

    auto result = table.find(text.startingAddress);
    ASSERT_FALSE(result == table.end());
    EXPECT_EQ(result->second, text);
}

TEST(Unit_SectionTable, iterator)
{
    SectionTable table;
    Section text{".text", 100, EA{1234}}, data{".data", 200, EA{1334}};
    table.addSection(text);
    table.addSection(data);

    auto it = table.begin();
    EXPECT_EQ(it->second, text);

    it++;
    EXPECT_EQ(it->second, data);

    it++;
    EXPECT_EQ(it, table.end());
}

TEST(Unit_SectionTable, serialize)
{
    SectionTable original;
    Section text{".text", 100, EA{1234}}, data{".data", 200, EA{1334}};
    original.addSection(text);
    original.addSection(data);

    // Serialize
    std::ostringstream out;
    boost::archive::polymorphic_text_oarchive oa{out};
    EXPECT_NO_THROW(oa << original);

    auto archived = out.str();

    // Deserialize and check
    SectionTable from_archive;
    std::istringstream in{archived};
    boost::archive::polymorphic_text_iarchive ia{in};
    EXPECT_NO_THROW(ia >> from_archive);

    EXPECT_EQ(from_archive.size(), original.size());
    {
        auto result = from_archive.find(text.startingAddress);
        ASSERT_FALSE(result == from_archive.end());
        EXPECT_EQ(result->second, text);
    }

    {
        auto result = from_archive.find(data.startingAddress);
        ASSERT_FALSE(result == from_archive.end());
        EXPECT_EQ(result->second, data);
    }
}
