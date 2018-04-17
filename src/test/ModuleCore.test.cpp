#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/ModuleCore.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <memory>

TEST(Unit_ModuleCore, ctor_0)
{
    EXPECT_NO_THROW(gtirb::ModuleCore());
}

TEST(Unit_ModuleCore, validParent)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::ModuleCore>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_ModuleCore, validParent_noException)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::ModuleCore>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_ModuleCore, invalidParent)
{
    auto notAModule = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::ModuleCore>();

    EXPECT_FALSE(child->getIsValidParent(notAModule.get()));
    EXPECT_THROW(notAModule->push_back(std::move(child)), gtirb::NodeStructureError);
}

TEST(Unit_ModuleCore, alreadyAdded)
{
    auto module = std::make_unique<gtirb::Module>();

    auto child = std::make_unique<gtirb::ModuleCore>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));

    auto childAgain = std::make_unique<gtirb::ModuleCore>();
    EXPECT_FALSE(childAgain->getIsValidParent(module.get()));
    EXPECT_THROW(module->push_back(std::move(childAgain)), gtirb::NodeStructureError);
}
