#include <gtest/gtest.h>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/unordered_map.hpp>
#include <fstream>
#include <gtirb/TableTemplate.hpp>

class TableIntIntString : public gtirb::TableTemplate<int, int, std::string>
{
public:
    // template <typename Archive>
    // void serialize(Archive& ar, const unsigned int version)
    //{
    //    // ar & boost::serialization::base_object<gtirb::TableTemplate<int, int,
    //    std::string>>(*this); ar & this->table;
    //}

    ///
    /// Serialize this node (and all of its children).
    ///
    virtual void serialize(boost::archive::polymorphic_iarchive& ar,
                           [[maybe_unused]] const unsigned int version = 0) override
    {
        ar & this->table;
    }

    ///
    /// Serialize this node (and all of its children).
    ///
    virtual void serialize(boost::archive::polymorphic_oarchive& ar,
                           [[maybe_unused]] const unsigned int version = 0) const override
    {
        ar & this->table;
    }
};

TEST(Unit_TableTemplate, ctor_0)
{
    EXPECT_NO_THROW(TableIntIntString());
}

TEST(Unit_TableTemplate, twoDimentionalIndex)
{
    auto t = TableIntIntString();
    t[21][12] = {"Foo"};
    t[12][21] = {"Bar"};

    EXPECT_EQ(std::string{"Foo"}, t[21][12]);
    EXPECT_EQ(std::string{"Bar"}, t[12][21]);
    EXPECT_EQ(size_t{2}, t.size());
}

TEST(Unit_TableTemplate, size_0)
{
    auto t = TableIntIntString();
    t[0][0] = {"Foo"};
    t[0][1] = {"Foo"};
    t[0][2] = {"Foo"};

    EXPECT_EQ(size_t{3}, t.size());
}

TEST(Unit_TableTemplate, size_1)
{
    auto t = TableIntIntString();
    t[0][0] = {"Foo"};
    t[1][0] = {"Foo"};
    t[2][0] = {"Foo"};

    EXPECT_EQ(size_t{3}, t.size());

    // Check base class access.
    EXPECT_EQ(size_t{3}, dynamic_cast<gtirb::Table*>(&t)->size());
}

TEST(Unit_TableTemplate, size_2)
{
    auto t = TableIntIntString();
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
    auto t = TableIntIntString();

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
    auto t = TableIntIntString();
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
    auto t = TableIntIntString();
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
    auto t = TableIntIntString();
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

TEST(Unit_TableTemplate, serialize)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    // Scope objects so they are destroyed
    {
        auto t = TableIntIntString();
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

        // Serialize Out.
        std::ofstream ofs{tempPathString.c_str()};
        boost::archive::polymorphic_text_oarchive oa{ofs};
        EXPECT_TRUE(ofs.is_open());

        oa << t;

        EXPECT_NO_THROW(ofs.close());
        EXPECT_FALSE(ofs.is_open());
    }

    // Read it back in and re-test
    {
        auto tIn = TableIntIntString();

        // Serialize In.
        std::ifstream ifs{tempPathString.c_str()};
        boost::archive::polymorphic_text_iarchive ia{ifs};

        ia >> tIn;

        EXPECT_NO_THROW(ifs.close());

        EXPECT_EQ(std::string{"Foo"}, tIn[21][12]);
        EXPECT_EQ(std::string{"Bar"}, tIn[12][21]);
        EXPECT_EQ(std::string{"Foo"}, tIn.at(21).at(12));
        EXPECT_EQ(std::string{"Bar"}, tIn.at(12).at(21));
        EXPECT_EQ(std::string{"Foo"}, tIn.at(21, 12));
        EXPECT_EQ(std::string{"Bar"}, tIn.at(12, 21));
        EXPECT_EQ(size_t{2}, tIn.size());
    }
}
