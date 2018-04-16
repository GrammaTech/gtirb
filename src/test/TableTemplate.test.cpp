#include <gtest/gtest.h>
#include <gtirb/TableTemplate.hpp>

TEST(Unit_TableTemplate, ctor_0)
{
    EXPECT_NO_THROW(gtirb::TableTemplate<>());
}

TEST(Unit_TableTemplate, twoDimentionalIndex)
{
    auto t = gtirb::TableTemplate<>();
    t[21][12] = {"Foo"};
    t[12][21] = {"Bar"};

    EXPECT_EQ(std::string{"Foo"}, t[21][12]);
    EXPECT_EQ(std::string{"Bar"}, t[12][21]);
    EXPECT_EQ(size_t{2}, t.size());
}

TEST(Unit_TableTemplate, size_0)
{
    auto t = gtirb::TableTemplate<>();
    t[0][0] = {"Foo"};
    t[0][1] = {"Foo"};
    t[0][2] = {"Foo"};

    EXPECT_EQ(size_t{3}, t.size());
}

TEST(Unit_TableTemplate, size_1)
{
    auto t = gtirb::TableTemplate<>();
    t[0][0] = {"Foo"};
    t[1][0] = {"Foo"};
    t[2][0] = {"Foo"};

    EXPECT_EQ(size_t{3}, t.size());

    // Check base class access.
    EXPECT_EQ(size_t{3}, dynamic_cast<gtirb::Table*>(&t)->size());
}

TEST(Unit_TableTemplate, size_2)
{
    auto t = gtirb::TableTemplate<>();
    t[0][0] = {"Foo"};
    t[1][0] = {"Foo"};
    t[2][0] = {"Foo"};
    t[2][1] = {"Foo"};

    EXPECT_EQ(size_t{4}, t.size());
    
    // Check base class access.
    EXPECT_EQ(size_t{4}, dynamic_cast<gtirb::Table*>(&t)->size());
}

TEST(Unit_TableTemplate, clear)
{
    auto t = gtirb::TableTemplate<>();

    EXPECT_EQ(size_t{0}, t.size());
    EXPECT_NO_THROW(t.clear());

    t[0][0] = {"Foo"};
    t[1][0] = {"Foo"};
    t[2][0] = {"Foo"};

    EXPECT_EQ(size_t{3}, t.size());
   
    // Check base class access.
    EXPECT_EQ(size_t{3}, dynamic_cast<gtirb::Table*>(&t)->size());

    EXPECT_NO_THROW(t.clear());
    EXPECT_EQ(size_t{0}, t.size());
}

TEST(Unit_TableTemplate, rotate)
{
    auto t = gtirb::TableTemplate<>();
    EXPECT_EQ(size_t{0}, t.size());

    t[21][12] = {"Foo"};
    t[12][21] = {"Bar"};

    EXPECT_EQ(std::string{"Foo"}, t[21][12]);
    EXPECT_EQ(std::string{"Bar"}, t[12][21]);
    EXPECT_EQ(size_t{2}, t.size());

    auto r = t.getRotated();

    EXPECT_EQ(std::string{"Bar"}, r[21][12]);
    EXPECT_EQ(std::string{"Foo"}, r[12][21]);
    EXPECT_EQ(size_t{2}, r.size());
}

TEST(Unit_TableTemplate, at)
{
    auto t = gtirb::TableTemplate<>();
    EXPECT_EQ(size_t{0}, t.size());

    t[21][12] = {"Foo"};
    t[12][21] = {"Bar"};

    EXPECT_EQ(std::string{"Foo"}, t[21][12]);
    EXPECT_EQ(std::string{"Bar"}, t[12][21]);
    EXPECT_EQ(std::string{"Foo"}, t.at(21).at(12));
    EXPECT_EQ(std::string{"Bar"}, t.at(12).at(21));
    EXPECT_EQ(std::string{"Foo"}, t.at(21, 12));
    EXPECT_EQ(std::string{"Bar"}, t.at(12, 21));
    EXPECT_EQ(size_t{2}, t.size());
}

// This should compile warning free.
TEST(Unit_TableTemplate, constAt)
{
    auto t = gtirb::TableTemplate<>();
    EXPECT_EQ(size_t{0}, t.size());

    t[21][12] = {"Foo"};
    t[12][21] = {"Bar"};

    auto constTableAt = [](const auto& ct) {
        EXPECT_EQ(std::string{"Foo"}, ct.at(21).at(12));
        EXPECT_EQ(std::string{"Bar"}, ct.at(12).at(21));
        EXPECT_EQ(std::string{"Foo"}, ct.at(21, 12));
        EXPECT_EQ(std::string{"Bar"}, ct.at(12, 21));
        EXPECT_EQ(size_t{2}, ct.size());
    };

    constTableAt(t);
}
