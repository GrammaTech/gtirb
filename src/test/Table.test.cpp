#include <gtest/gtest.h>
#include <gtirb/Table.hpp>

TEST(Unit_Table, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Table<>());
}

TEST(Unit_Table, ctor_1)
{
    EXPECT_NO_THROW(gtirb::Table<>("Foo"));
}

TEST(Unit_Table, getTableName_0)
{
    auto t = gtirb::Table<>();
    EXPECT_EQ(std::string{}, t.getTableName());
}

TEST(Unit_Table, getTableName_1)
{
    const std::string tableName{"foo"};

    auto t = gtirb::Table<>(tableName);
    EXPECT_EQ(tableName, t.getTableName());
}

TEST(Unit_Table, getTableName_2)
{
    const std::string tableName{};

    auto t = gtirb::Table<>();
    EXPECT_EQ(std::string{}, t.getTableName());

    t.setTableName(tableName);
    EXPECT_EQ(tableName, t.getTableName());
}

TEST(Unit_Table, twoDimentionalIndex)
{
    auto t = gtirb::Table<>();
    t[21][12] = {"Foo"};
    t[12][21] = {"Bar"};

    EXPECT_EQ(std::string{"Foo"}, t[21][12]);
    EXPECT_EQ(std::string{"Bar"}, t[12][21]);
    EXPECT_EQ(size_t{2}, t.size());
}

TEST(Unit_Table, size_0)
{
    auto t = gtirb::Table<>();
    t[0][0] = {"Foo"};
    t[0][1] = {"Foo"};
    t[0][2] = {"Foo"};

    EXPECT_EQ(size_t{3}, t.size());
}

TEST(Unit_Table, size_1)
{
    auto t = gtirb::Table<>();
    t[0][0] = {"Foo"};
    t[1][0] = {"Foo"};
    t[2][0] = {"Foo"};

    EXPECT_EQ(size_t{3}, t.size());
}

TEST(Unit_Table, size_2)
{
    auto t = gtirb::Table<>();
    t[0][0] = {"Foo"};
    t[1][0] = {"Foo"};
    t[2][0] = {"Foo"};
    t[2][1] = {"Foo"};

    EXPECT_EQ(size_t{4}, t.size());
}

TEST(Unit_Table, clear)
{
    auto t = gtirb::Table<>();

    EXPECT_EQ(size_t{0}, t.size());
    EXPECT_NO_THROW(t.clear());

    t[0][0] = {"Foo"};
    t[1][0] = {"Foo"};
    t[2][0] = {"Foo"};

    EXPECT_EQ(size_t{3}, t.size());

    EXPECT_NO_THROW(t.clear());
    EXPECT_EQ(size_t{0}, t.size());
}

TEST(Unit_Table, rotate)
{
    auto t = gtirb::Table<>();
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

TEST(Unit_Table, at)
{
    auto t = gtirb::Table<>();
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
TEST(Unit_Table, constAt)
{
    auto t = gtirb::Table<>();
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
