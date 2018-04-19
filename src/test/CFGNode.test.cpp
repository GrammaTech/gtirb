#include <gtest/gtest.h>
#include <gtirb/CFG.hpp>
#include <gtirb/CFGNode.hpp>
#include <gtirb/CFGNodeInfoCall.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/RuntimeError.hpp>
#include <memory>

TEST(Unit_CFGNode, ctor_0)
{
    EXPECT_NO_THROW(gtirb::CFGNode());
}

TEST(Unit_CFGNode, validParent_cfg)
{
    auto parent = std::make_unique<gtirb::CFG>();
    auto child = std::make_unique<gtirb::CFGNode>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));
}

TEST(Unit_CFGNode, validParent_cfgnode)
{
    auto parent = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNode>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));
}

TEST(Unit_CFGNode, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::CFGNode>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}

TEST(Unit_CFGNode, alreadyAdded)
{
    auto parent = std::make_unique<gtirb::CFG>();

    auto child = std::make_unique<gtirb::CFGNode>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));

    // This should work just fine.
    auto childAgain = std::make_unique<gtirb::CFGNode>();
    EXPECT_TRUE(childAgain->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(childAgain)));
}

TEST(Unit_CFGNode, getCFGNodeInfo)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    EXPECT_TRUE(node->getCFGNodeInfo() == nullptr);

    auto nodeInfo = std::make_unique<gtirb::CFGNodeInfoCall>();
    EXPECT_NO_THROW(node->push_back(std::move(nodeInfo)));

    EXPECT_TRUE(node->getCFGNodeInfo() != nullptr);
}

TEST(Unit_CFGNode, addSuccessor_self)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNode>();
    auto childPtr = child.get();

    EXPECT_NO_THROW(node->push_back(std::move(child)));
    EXPECT_FALSE(node->empty());

    EXPECT_NO_THROW(node->addSuccessor(childPtr));
    EXPECT_EQ(size_t{1}, node->getSuccessorSize());

    EXPECT_NO_THROW(node->addSuccessor(childPtr, true));
    EXPECT_EQ(size_t{2}, node->getSuccessorSize());
    
    EXPECT_NO_THROW(node->addSuccessor(childPtr, false));
    EXPECT_EQ(size_t{3}, node->getSuccessorSize());

    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
}

TEST(Unit_CFGNode, addSuccessor_other)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto other = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNode>();
    auto childPtr = child.get();

    EXPECT_NO_THROW(other->push_back(std::move(child)));
    EXPECT_FALSE(other->empty());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());

    EXPECT_NO_THROW(node->addSuccessor(childPtr));
    EXPECT_EQ(size_t{1}, node->getSuccessorSize());

    EXPECT_NO_THROW(node->addSuccessor(childPtr, true));
    EXPECT_EQ(size_t{2}, node->getSuccessorSize());

    EXPECT_NO_THROW(node->addSuccessor(childPtr, false));
    EXPECT_EQ(size_t{3}, node->getSuccessorSize());

    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
}

TEST(Unit_CFGNode, addSuccessor_throws)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNode>();
    auto childPtr = child.get();

    // The child has not been added to a child anywhere, so this should fail.
    EXPECT_THROW(node->addSuccessor(childPtr), std::bad_weak_ptr);
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
}

TEST(Unit_CFGNode, addSuccessor_unique)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNode>();

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(node->addSuccessor(std::move(child)));
    EXPECT_EQ(size_t{1}, node->getSuccessorSize());
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
}

TEST(Unit_CFGNode, setSuccessor_0)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child0Ptr = child0.get();

    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child1Ptr = child1.get();
    
    auto child2 = std::make_unique<gtirb::CFGNode>();
    auto child2Ptr = child2.get();

    auto childFoo = std::make_unique<gtirb::CFGNode>();
    auto childFooPtr = childFoo.get();

    EXPECT_NO_THROW(node->push_back(std::move(child0)));
    EXPECT_NO_THROW(node->push_back(std::move(child1)));
    EXPECT_NO_THROW(node->push_back(std::move(child2)));
    EXPECT_NO_THROW(node->push_back(std::move(childFoo)));

    EXPECT_EQ(size_t{4}, node->size());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(node->addSuccessor(child0Ptr));
    EXPECT_NO_THROW(node->addSuccessor(child1Ptr));
    EXPECT_NO_THROW(node->addSuccessor(child2Ptr));
    EXPECT_EQ(size_t{3}, node->getSuccessorSize());

    EXPECT_NE(childFooPtr, node->getSuccessor(size_t(0)).first);
    EXPECT_NE(childFooPtr, node->getSuccessor(size_t(1)).first);
    EXPECT_NE(childFooPtr, node->getSuccessor(size_t(2)).first);
    EXPECT_EQ(size_t{3}, node->getSuccessorSize());

    EXPECT_NO_THROW(node->setSuccessor(size_t(0), childFooPtr, true));
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
}

TEST(Unit_CFGNode, setSuccessor_1)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child0Ptr = child0.get();

    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child1Ptr = child1.get();
    
    auto child2 = std::make_unique<gtirb::CFGNode>();
    auto child2Ptr = child2.get();

    auto childFoo = std::make_unique<gtirb::CFGNode>();
    auto childFooPtr = childFoo.get();

    EXPECT_NO_THROW(node->push_back(std::move(childFoo)));

    EXPECT_EQ(size_t{1}, node->size());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(node->addSuccessor(std::move(child0)));
    EXPECT_NO_THROW(node->addSuccessor(std::move(child1)));
    EXPECT_NO_THROW(node->addSuccessor(std::move(child2)));
    EXPECT_EQ(size_t{4}, node->size());
    EXPECT_EQ(size_t{3}, node->getSuccessorSize());

    EXPECT_NE(childFooPtr, node->getSuccessor(size_t(0)).first);
    EXPECT_NE(childFooPtr, node->getSuccessor(size_t(1)).first);
    EXPECT_NE(childFooPtr, node->getSuccessor(size_t(2)).first);
    EXPECT_EQ(size_t{3}, node->getSuccessorSize());

    EXPECT_NO_THROW(node->setSuccessor(size_t(0), childFooPtr, true));
    
    EXPECT_EQ(childFooPtr, node->getSuccessor(size_t(0)).first);
    EXPECT_TRUE(node->getSuccessor(size_t(0)).second);
    EXPECT_NE(childFooPtr, node->getSuccessor(size_t(1)).first);
    EXPECT_FALSE(node->getSuccessor(size_t(1)).second);
    EXPECT_NE(childFooPtr, node->getSuccessor(size_t(2)).first);
    EXPECT_FALSE(node->getSuccessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getSuccessorSize());

    EXPECT_NO_THROW(node->setSuccessor(size_t(2), childFooPtr, true));

    EXPECT_EQ(childFooPtr, node->getSuccessor(size_t(0)).first);
    EXPECT_TRUE(node->getSuccessor(size_t(0)).second);
    EXPECT_NE(childFooPtr, node->getSuccessor(size_t(1)).first);
    EXPECT_FALSE(node->getSuccessor(size_t(1)).second);
    EXPECT_EQ(childFooPtr, node->getSuccessor(size_t(2)).first);
    EXPECT_TRUE(node->getSuccessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getSuccessorSize());

    EXPECT_NO_THROW(node->setSuccessor(size_t(1), childFooPtr, true));
    
    EXPECT_EQ(childFooPtr, node->getSuccessor(size_t(0)).first);
    EXPECT_TRUE(node->getSuccessor(size_t(0)).second);
    EXPECT_EQ(childFooPtr, node->getSuccessor(size_t(1)).first);
    EXPECT_TRUE(node->getSuccessor(size_t(1)).second);
    EXPECT_EQ(childFooPtr, node->getSuccessor(size_t(2)).first);
    EXPECT_TRUE(node->getSuccessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getSuccessorSize());
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
}

TEST(Unit_CFGNode, setSuccessor_throws)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child0Ptr = child0.get();

    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child1Ptr = child1.get();
    
    auto child2 = std::make_unique<gtirb::CFGNode>();
    auto child2Ptr = child2.get();

    auto childFoo = std::make_unique<gtirb::CFGNode>();
    auto childFooPtr = childFoo.get();

    EXPECT_NO_THROW(node->push_back(std::move(child0)));
    EXPECT_NO_THROW(node->push_back(std::move(child1)));
    EXPECT_NO_THROW(node->push_back(std::move(child2)));
    EXPECT_NO_THROW(node->push_back(std::move(childFoo)));

    EXPECT_EQ(size_t{4}, node->size());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());

    EXPECT_THROW(node->setSuccessor(size_t{64}, childFooPtr), std::out_of_range);
}

/*
        void setSuccessor(size_t index, std::unique_ptr<CFGNode>&& x, bool isExecutable = false);

        std::pair<CFGNode*, bool> getSuccessor(size_t x) const;

        bool getSuccessorsEmpty() const;

        size_t getSuccessorSize() const;

        void removeSuccessor(size_t x);

        void removeSuccessor(const CFGNode* const x);

        void removeSuccessor(const CFGNode* const x, bool isExecutable);
        */