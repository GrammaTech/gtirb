#include <gtest/gtest.h>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <boost/filesystem.hpp>
#include <gtirb/AddrRanges.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/RuntimeError.hpp>
#include <memory>

TEST(Unit_AddrRanges, ctor_0)
{
    EXPECT_NO_THROW(gtirb::AddrRanges());
}

TEST(Unit_AddrRanges, validParent)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::AddrRanges>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_AddrRanges, validParent_noException)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::AddrRanges>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_AddrRanges, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::AddrRanges>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}

TEST(Unit_AddrRanges, alreadyAdded)
{
    auto module = std::make_unique<gtirb::Module>();

    auto child = std::make_unique<gtirb::AddrRanges>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));

    auto childAgain = std::make_unique<gtirb::AddrRanges>();
    EXPECT_FALSE(childAgain->getIsValidParent(module.get()));
    EXPECT_THROW(module->push_back(std::move(childAgain)), gtirb::NodeStructureError);
}

TEST(Unit_AddrRanges, validRange)
{
    auto node = std::make_unique<gtirb::AddrRanges>();

    EXPECT_TRUE(node->data().empty());
    EXPECT_NO_THROW(node->addRange({gtirb::EA{1221}, gtirb::EA{2112}}));
    EXPECT_FALSE(node->data().empty());
    EXPECT_EQ(size_t{1}, node->data().size());
}

TEST(Unit_AddrRanges, invalidRange)
{
    auto node = std::make_unique<gtirb::AddrRanges>();

    EXPECT_TRUE(node->data().empty());
    EXPECT_THROW(node->addRange({gtirb::EA{2112}, gtirb::EA{1221}}), gtirb::RuntimeError);
    EXPECT_TRUE(node->data().empty());
    EXPECT_EQ(size_t{0}, node->data().size());
}

TEST(Unit_AddrRanges, grow)
{
    gtirb::AddrRanges ar;

    EXPECT_TRUE(ar.data().empty());
    EXPECT_EQ(size_t{0}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{7}, gtirb::EA{8}}));
    EXPECT_EQ(size_t{1}, ar.data().size());
    EXPECT_EQ(size_t{1}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{3}, gtirb::EA{4}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{2}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{4}, gtirb::EA{5}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{3}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{6}, gtirb::EA{7}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{4}, ar.getBytesCoveredByRanges());
}

TEST(Unit_AddrRanges, clearRanges)
{
    gtirb::AddrRanges ar;

    EXPECT_TRUE(ar.data().empty());
    EXPECT_EQ(size_t{0}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{7}, gtirb::EA{8}}));
    EXPECT_EQ(size_t{1}, ar.data().size());
    EXPECT_EQ(size_t{1}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{3}, gtirb::EA{4}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{2}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{4}, gtirb::EA{5}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{3}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{6}, gtirb::EA{7}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{4}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.clearRanges());
    EXPECT_TRUE(ar.data().empty());
    EXPECT_EQ(size_t{0}, ar.getBytesCoveredByRanges());
}

TEST(Unit_AddrRanges, range)
{
    gtirb::AddrRanges ar;

    EXPECT_TRUE(ar.data().empty());
    EXPECT_EQ(size_t{0}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{70}, gtirb::EA{80}}));
    EXPECT_EQ(size_t{1}, ar.data().size());
    EXPECT_EQ(size_t{10}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{30}, gtirb::EA{40}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{20}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{40}, gtirb::EA{50}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{30}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{60}, gtirb::EA{70}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{40}, ar.getBytesCoveredByRanges());

    // Print additional debugging information
    if(ar.data().size() != 2)
    {
        for(auto& range : ar.data())
        {
            ADD_FAILURE() << "Range {" << range.first << ", " << range.second << "}";
        }
    }

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{50}, gtirb::EA{60}}));
    EXPECT_EQ(size_t{1}, ar.data().size());
    EXPECT_EQ(size_t{50}, ar.getBytesCoveredByRanges());
}

TEST(Unit_AddrRanges, subtractRange)
{
    gtirb::AddrRanges ar;

    EXPECT_TRUE(ar.data().empty());
    EXPECT_EQ(size_t{0}, ar.getBytesCoveredByRanges());

    EXPECT_TRUE(ar.addRange({gtirb::EA{70}, gtirb::EA{80}}));
    EXPECT_EQ(size_t{1}, ar.data().size());
    EXPECT_EQ(size_t{10}, ar.getBytesCoveredByRanges());

    EXPECT_TRUE(ar.addRange({gtirb::EA{30}, gtirb::EA{40}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{20}, ar.getBytesCoveredByRanges());

    EXPECT_TRUE(ar.subtractRange({gtirb::EA{31}, gtirb::EA{36}}));
    EXPECT_EQ(size_t{3}, ar.data().size());
    EXPECT_EQ(size_t{15}, ar.getBytesCoveredByRanges());

    EXPECT_TRUE(ar.addRange({gtirb::EA{40}, gtirb::EA{50}}));
    EXPECT_EQ(size_t{3}, ar.data().size());
    EXPECT_EQ(size_t{25}, ar.getBytesCoveredByRanges());

    EXPECT_TRUE(ar.addRange({gtirb::EA{60}, gtirb::EA{70}}));
    EXPECT_EQ(size_t{3}, ar.data().size());
    EXPECT_EQ(size_t{35}, ar.getBytesCoveredByRanges());

    EXPECT_TRUE(ar.addRange({gtirb::EA{50}, gtirb::EA{60}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{45}, ar.getBytesCoveredByRanges());
}

TEST(Unit_AddrRanges, serialize)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    gtirb::AddrRanges original;

    EXPECT_TRUE(original.data().empty());
    EXPECT_EQ(size_t{0}, original.getBytesCoveredByRanges());

    EXPECT_TRUE(original.addRange({gtirb::EA{10}, gtirb::EA{20}}));
    EXPECT_TRUE(original.addRange({gtirb::EA{30}, gtirb::EA{40}}));
    EXPECT_TRUE(original.addRange({gtirb::EA{50}, gtirb::EA{60}}));
    EXPECT_TRUE(original.addRange({gtirb::EA{70}, gtirb::EA{80}}));
    EXPECT_EQ(size_t{4}, original.data().size());
    EXPECT_EQ(size_t{40}, original.getBytesCoveredByRanges());

    original.setLocalProperty("Name", std::string("Value"));
    EXPECT_EQ(size_t{1}, original.getLocalPropertySize());
    EXPECT_EQ(std::string{"Value"}, boost::get<std::string>(original.getLocalProperty("Name")));

    // Scope objects so they are destroyed
    {
        // Serialize Out.
        std::ofstream ofs{tempPathString.c_str()};
        boost::archive::polymorphic_text_oarchive oa{ofs};
        EXPECT_TRUE(ofs.is_open());

        EXPECT_NO_THROW(oa << original);

        EXPECT_NO_THROW(ofs.close());
        EXPECT_FALSE(ofs.is_open());
    }

    // Read it back in and re-test
    {
        gtirb::AddrRanges serialized;

        // Serialize In.
        std::ifstream ifs{tempPathString.c_str()};
        boost::archive::polymorphic_text_iarchive ia{ifs};

        EXPECT_NO_THROW(ia >> serialized);

        EXPECT_NO_THROW(ifs.close());

        EXPECT_EQ(size_t{4}, serialized.data().size());
        EXPECT_EQ(size_t{40}, serialized.getBytesCoveredByRanges());

        EXPECT_EQ(size_t{1}, serialized.getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"},
                  boost::get<std::string>(serialized.getLocalProperty("Name")));
    }
}
