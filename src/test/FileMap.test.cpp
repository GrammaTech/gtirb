#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/FileMap.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <memory>

TEST(Unit_FileMap, ctor_0)
{
    EXPECT_NO_THROW(gtirb::FileMap());
}

TEST(Unit_FileMap, validParent)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::FileMap>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_FileMap, validParent_noException)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::FileMap>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_FileMap, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::FileMap>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}
