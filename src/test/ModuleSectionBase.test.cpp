#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/ModuleSectionBase.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <memory>

TEST(Unit_ModuleSectionBase, ctor_0)
{
    EXPECT_NO_THROW(gtirb::ModuleSectionBase());
}

TEST(Unit_ModuleSectionBase, validParent)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::ModuleSectionBase>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_ModuleSectionBase, validParent_noException)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::ModuleSectionBase>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_ModuleSectionBase, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::ModuleSectionBase>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}

TEST(Unit_ModuleSectionBase, getIsSetupComplete)
{
    auto m = std::make_unique<gtirb::ModuleSectionBase>();
    EXPECT_NO_THROW(m->getIsSetupComplete());
    EXPECT_FALSE(m->getIsSetupComplete());
}

TEST(Unit_ModuleSectionBase, getIsReadOnly)
{
    auto m = std::make_unique<gtirb::ModuleSectionBase>();
    EXPECT_NO_THROW(m->getIsReadOnly());
    EXPECT_FALSE(m->getIsReadOnly());
}
