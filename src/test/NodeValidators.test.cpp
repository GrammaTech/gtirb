#include <gtest/gtest.h>
#include <gtirb/NodeValidators.hpp>
#include <memory>

TEST(Unit_NodeValidators, NodeValidatorHasParentOfType_Pass)
{
    class Foo : public gtirb::Node
    {
    };

    class Bar : public gtirb::Node
    {
    public:
        Bar()
        {
            // We must have a "Foo" type parent.
            this->addParentValidator(gtirb::NodeValidatorHasParentOfType<Foo>);
        }
    };

    auto foo = std::make_shared<Foo>();
    EXPECT_NO_THROW(foo->push_back(std::make_unique<Bar>()));
    EXPECT_NO_THROW(foo->push_back(std::make_unique<Bar>()));
}

TEST(Unit_NodeValidators, NodeValidatorHasParentOfType_Fail)
{
    class Foo : public gtirb::Node
    {
    };

    class Bar : public gtirb::Node
    {
    public:
        Bar()
        {
            // We must have a "Foo" type parent.
            this->addParentValidator(gtirb::NodeValidatorHasParentOfType<Foo>);
        }
    };

    class Baz : public gtirb::Node
    {
    };

    auto baz = std::make_shared<Baz>();
    EXPECT_THROW(baz->push_back(std::make_unique<Bar>()), gtirb::NodeStructureError);
}

TEST(Unit_NodeValidators, NodeValidatorHasNoSiblingsOfType_Pass)
{
    class Foo : public gtirb::Node
    {
    };

    class Bar : public gtirb::Node
    {
    public:
        Bar()
        {
            // We must be the only "Bar" child.
            this->addParentValidator(gtirb::NodeValidatorHasNoSiblingsOfType<Bar>);
        }
    };

    auto foo = std::make_shared<Foo>();
    EXPECT_NO_THROW(foo->push_back(std::make_unique<Bar>()));
}

TEST(Unit_NodeValidators, NodeValidatorHasNoSiblingsOfType_Fail)
{
    class Foo : public gtirb::Node
    {
    };

    class Bar : public gtirb::Node
    {
    public:
        Bar()
        {
            // We must be the only "Bar" child.
            this->addParentValidator(gtirb::NodeValidatorHasNoSiblingsOfType<Bar>);
        }
    };

    auto foo = std::make_shared<Foo>();

    // Pass the first time...
    EXPECT_NO_THROW(foo->push_back(std::make_unique<Bar>()));

    // Fail the second time...
    EXPECT_THROW(foo->push_back(std::make_unique<Bar>()), gtirb::NodeStructureError);
}
