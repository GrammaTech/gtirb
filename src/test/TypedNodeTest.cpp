#include <gtest/gtest.h>
#include <gtirb/AddrRanges.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/ModuleAux.hpp>
#include <gtirb/ModuleCore.hpp>
#include <gtirb/ModuleSectionBase.hpp>
#include <gtirb/ModuleSummary.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/NodeUtilities.hpp>
#include <gtirb/Procedure.hpp>
#include <gtirb/SymbolSet.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/Region.hpp>
#include <gtirb/FileMap.hpp>
#include <memory>

using testing::Types;

typedef Types<gtirb::Module, gtirb::ModuleSectionBase, gtirb::ModuleCore, gtirb::ModuleAux,
              gtirb::ModuleSummary, gtirb::AddrRanges, gtirb::Procedure, gtirb::Instruction,
              gtirb::SymbolSet, gtirb::Symbol, gtirb::IR, gtirb::Region, gtirb::FileMap>
    TypeImplementations;

// ----------------------------------------------------------------------------
// Typed test fixture.

template <class T>
class TypedNodeTest : public testing::Test
{
protected:
    TypedNodeTest() = default;
    virtual ~TypedNodeTest() = default;
};

TYPED_TEST_CASE_P(TypedNodeTest);

// ----------------------------------------------------------------------------
// Tests to run on all types.

TYPED_TEST_P(TypedNodeTest, ctor_0)
{
    EXPECT_NO_THROW(TypeParam{});
}

TYPED_TEST_P(TypedNodeTest, uniqueUuids)
{
    std::vector<boost::uuids::uuid> uuids;

    // Create a bunch of UUID's, then make sure we don't have any duplicates.

    for(size_t i = 0; i < 64; ++i)
    {
        const auto n = TypeParam{};
        uuids.push_back(n.getUUID());
    }

    std::sort(std::begin(uuids), std::end(uuids));
    const auto end = std::unique(std::begin(uuids), std::end(uuids));

    EXPECT_EQ(std::end(uuids), end) << "Duplicate UUID's were generated.";
}

TYPED_TEST_P(TypedNodeTest, setLocalProperty)
{
    auto node = TypeParam{};

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp{"Foo", "Bar"};
    EXPECT_NO_THROW(node.setLocalProperty(nvp.first, nvp.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());
}

TYPED_TEST_P(TypedNodeTest, setLocalPropertyReset)
{
    auto node = TypeParam{};

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Foo", "Bah"};

    EXPECT_NO_THROW(node.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());
}

TYPED_TEST_P(TypedNodeTest, setLocalProperties)
{
    auto node = TypeParam{};

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Bar", "Foo"};

    EXPECT_NO_THROW(node.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(2), node.getLocalPropertySize());
}

TYPED_TEST_P(TypedNodeTest, removeLocalProperty)
{
    auto node = TypeParam{};

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Bar", "Foo"};

    EXPECT_NO_THROW(node.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(2), node.getLocalPropertySize());

    EXPECT_TRUE(node.removeLocalProperty("Foo"));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_TRUE(node.removeLocalProperty("Bar"));
    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());
}

TYPED_TEST_P(TypedNodeTest, clearLocalProperties)
{
    auto node = TypeParam{};

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Bar", "Foo"};

    EXPECT_NO_THROW(node.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(2), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.clearLocalProperties());

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());
}

TYPED_TEST_P(TypedNodeTest, push_back)
{
    auto node = TypeParam{};

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));

    EXPECT_FALSE(node.empty());
    EXPECT_EQ(size_t(1), node.size());
}

TYPED_TEST_P(TypedNodeTest, size)
{
    const int totalChildren = 64;
    auto node = TypeParam{};

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));
        EXPECT_FALSE(node.empty());
        EXPECT_EQ(size_t(i + 1), node.size());
    }
}

TYPED_TEST_P(TypedNodeTest, clear)
{
    const int totalChildren = 64;
    auto node = TypeParam{};

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));
        EXPECT_FALSE(node.empty());
        EXPECT_EQ(size_t(i + 1), node.size());
    }

    EXPECT_NO_THROW(node.clear());

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());
}

TYPED_TEST_P(TypedNodeTest, iterator)
{
    const int totalChildren = 8;

    auto node = TypeParam{};

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));
        EXPECT_FALSE(node.empty());
        EXPECT_EQ(size_t(i + 1), node.size());
    }

    // We should now be able to use our begin iterator "totalChildren" times.
    auto nodeIterator = std::begin(node);
    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_TRUE(nodeIterator != std::end(node));

        EXPECT_EQ(node.at(i)->getUUID(), nodeIterator->getUUID());
        ++nodeIterator;

        EXPECT_NE(nodeIterator, std::begin(node));
    }

    EXPECT_NE(nodeIterator, std::begin(node));
    EXPECT_EQ(nodeIterator, std::end(node));
}

TYPED_TEST_P(TypedNodeTest, const_iterator)
{
    const int totalChildren = 8;

    auto node = TypeParam{};

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));
        EXPECT_FALSE(node.empty());
        EXPECT_EQ(size_t(i + 1), node.size());
    }

    // Make a const node by copy construction.
    const auto constNode{node};

    // We should now be able to use our begin iterator "totalChildren" times.
    auto nodeIterator = std::begin(constNode);
    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_TRUE(nodeIterator != std::end(constNode));

        EXPECT_EQ(constNode.at(i)->getUUID(), nodeIterator->getUUID());
        ++nodeIterator;

        EXPECT_NE(nodeIterator, std::begin(constNode));
    }

    EXPECT_NE(nodeIterator, std::begin(constNode));
    EXPECT_EQ(nodeIterator, std::end(constNode));
}

TYPED_TEST_P(TypedNodeTest, GetChildrenOfType)
{
    class Foo : public gtirb::Node
    {
    };

    class Bar : public gtirb::Node
    {
    };

    const int fooChildren = 3;
    const int barChildren = 5;
    const int typeChildren = 8;

    auto node = gtirb::Node{};

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

REGISTER_TYPED_TEST_CASE_P(TypedNodeTest, ctor_0, clear, clearLocalProperties, const_iterator,
                           iterator, push_back, removeLocalProperty, setLocalProperties,
                           setLocalProperty, setLocalPropertyReset, size, uniqueUuids,
                           GetChildrenOfType);

INSTANTIATE_TYPED_TEST_CASE_P(Unit_Exceptions,      // Instance name
                              TypedNodeTest,        // Test case name
                              TypeImplementations); // Type list