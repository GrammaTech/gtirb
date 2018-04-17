#include <gtest/gtest.h>
#include <gtirb/NodeUtilities.hpp>
#include <memory>

TEST(Unit_NodeUtilitites, GetChildrenOfType)
{
    class Foo : public gtirb::Node
    {
    };

    class Bar : public gtirb::Node
    {
    };

    const int fooChildren = 3;
    const int barChildren = 5;

    auto node = gtirb::Node();

    for(int i = 0; i < fooChildren; i++)
    {
        node.push_back(std::make_unique<Foo>());
    }

    for(int i = 0; i < barChildren; i++)
    {
        node.push_back(std::make_unique<Bar>());
    }

    const auto childrenOfTypeFoo = gtirb::GetChildrenOfType<Foo>(&node);
    EXPECT_EQ(size_t(fooChildren), childrenOfTypeFoo.size());

    const auto childrenOfTypeBar = gtirb::GetChildrenOfType<Bar>(&node);
    EXPECT_EQ(size_t(barChildren), childrenOfTypeBar.size());

    const auto allChildren = gtirb::GetChildrenOfType<gtirb::Node>(&node);
    EXPECT_EQ(size_t(fooChildren + barChildren), allChildren.size());
}

TEST(Unit_NodeUtilitites, GetOrCreateChildOfType)
{
    class Foo : public gtirb::Node
    {
    };

    auto node = gtirb::Node();

    auto childrenOfTypeFoo = gtirb::GetChildrenOfType<Foo>(&node);
    EXPECT_TRUE(childrenOfTypeFoo.empty());
    EXPECT_EQ(size_t{0}, childrenOfTypeFoo.size());

    // Create
    EXPECT_NO_THROW(gtirb::GetOrCreateChildOfType<Foo>(&node));
    childrenOfTypeFoo = gtirb::GetChildrenOfType<Foo>(&node);
    EXPECT_FALSE(childrenOfTypeFoo.empty());
    EXPECT_EQ(size_t{1}, childrenOfTypeFoo.size());

    // Get (should not create a second one)
    EXPECT_NO_THROW(gtirb::GetOrCreateChildOfType<Foo>(&node));
    childrenOfTypeFoo = gtirb::GetChildrenOfType<Foo>(&node);
    EXPECT_FALSE(childrenOfTypeFoo.empty());
    EXPECT_EQ(size_t{1}, childrenOfTypeFoo.size());
}
