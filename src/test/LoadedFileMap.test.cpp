#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/LoadedFileMap.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <memory>

TEST(Unit_LoadedFileMap, ctor_0)
{
    EXPECT_NO_THROW(gtirb::LoadedFileMap());
}

TEST(Unit_LoadedFileMap, validParent)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::LoadedFileMap>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_LoadedFileMap, validParent_noException)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::LoadedFileMap>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_LoadedFileMap, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::LoadedFileMap>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}
