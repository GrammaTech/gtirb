#include <gtest/gtest.h>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <boost/filesystem.hpp>
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

TEST(Unit_CFGNode, getCFGNodeInfo)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    EXPECT_TRUE(node->getCFGNodeInfo() == nullptr);

    auto nodeInfo = std::make_unique<gtirb::CFGNodeInfoCall>();
    EXPECT_NO_THROW(node->setCFGNodeInfo(std::move(nodeInfo)));

    EXPECT_TRUE(node->getCFGNodeInfo() != nullptr);
}

TEST(Unit_CFGNode, addSuccessor_self)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNode>();
    auto childPtr = child.get();

    EXPECT_NO_THROW(node->addChild(std::move(child)));

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

    EXPECT_NO_THROW(other->addChild(std::move(child)));
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

    EXPECT_NO_THROW(node->addChild(std::move(child0)));
    EXPECT_NO_THROW(node->addChild(std::move(child1)));
    EXPECT_NO_THROW(node->addChild(std::move(child2)));
    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

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
    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child2 = std::make_unique<gtirb::CFGNode>();

    auto childFoo = std::make_unique<gtirb::CFGNode>();
    auto childFooPtr = childFoo.get();

    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, node->getSuccessorSize());

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(node->addSuccessor(std::move(child0)));
    EXPECT_NO_THROW(node->addSuccessor(std::move(child1)));
    EXPECT_NO_THROW(node->addSuccessor(std::move(child2)));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
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
    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child2 = std::make_unique<gtirb::CFGNode>();

    auto childFoo = std::make_unique<gtirb::CFGNode>();
    auto childFooPtr = childFoo.get();

    EXPECT_NO_THROW(node->addChild(std::move(child0)));
    EXPECT_NO_THROW(node->addChild(std::move(child1)));
    EXPECT_NO_THROW(node->addChild(std::move(child2)));
    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());

    EXPECT_THROW(node->setSuccessor(size_t{64}, childFooPtr), std::out_of_range);
}

TEST(Unit_CFGNode, setSuccessor_move)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child2 = std::make_unique<gtirb::CFGNode>();
    auto childFoo = std::make_unique<gtirb::CFGNode>();

    EXPECT_NO_THROW(node->addChild(std::move(child0)));
    EXPECT_NO_THROW(node->addChild(std::move(child1)));
    EXPECT_NO_THROW(node->addChild(std::move(child2)));
    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
}

TEST(Unit_CFGNode, getSuccessor)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNode>();
    auto childPtr = child.get();

    EXPECT_NO_THROW(node->addChild(std::move(child)));
    EXPECT_TRUE(node->getSuccessorsEmpty());

    EXPECT_NO_THROW(node->addSuccessor(childPtr));
    EXPECT_FALSE(node->getSuccessorsEmpty());
    EXPECT_EQ(size_t{1}, node->getSuccessorSize());

    EXPECT_NO_THROW(node->addSuccessor(childPtr, true));
    EXPECT_EQ(size_t{2}, node->getSuccessorSize());

    EXPECT_NO_THROW(node->addSuccessor(childPtr, false));
    EXPECT_EQ(size_t{3}, node->getSuccessorSize());

    EXPECT_EQ(size_t{0}, node->getPredecessorSize());

    auto successor0 = node->getSuccessor(size_t{0});
    EXPECT_EQ(childPtr, successor0.first);
    EXPECT_EQ(false, successor0.second);

    auto successor1 = node->getSuccessor(size_t{1});
    EXPECT_EQ(childPtr, successor1.first);
    EXPECT_EQ(true, successor1.second);
}

TEST(Unit_CFGNode, removeSuccessor_index)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child2 = std::make_unique<gtirb::CFGNode>();

    auto childFoo = std::make_unique<gtirb::CFGNode>();
    auto childFooPtr = childFoo.get();

    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, node->getSuccessorSize());

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(node->addSuccessor(std::move(child0)));
    EXPECT_NO_THROW(node->addSuccessor(std::move(child1)));
    EXPECT_NO_THROW(node->addSuccessor(std::move(child2)));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
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

    // Now remove Child 1 from the list of successors.
    // It will not be removed from the list of node children.
    EXPECT_EQ(size_t{3}, node->getSuccessorSize());
    EXPECT_NO_THROW(node->removeSuccessor(size_t{1}));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
    EXPECT_EQ(size_t{2}, node->getSuccessorSize());
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
}

TEST(Unit_CFGNode, removeSuccessor_index_empty)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    EXPECT_NO_THROW(node->removeSuccessor(size_t{0}));
}

TEST(Unit_CFGNode, removeSuccessor_ptr)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child2 = std::make_unique<gtirb::CFGNode>();

    auto childFoo = std::make_unique<gtirb::CFGNode>();
    auto childFooPtr = childFoo.get();

    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, node->getSuccessorSize());

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(node->addSuccessor(std::move(child0)));
    EXPECT_NO_THROW(node->addSuccessor(std::move(child1)));
    EXPECT_NO_THROW(node->addSuccessor(std::move(child2)));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
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

    // Now remove Child 1 from the list of successors.
    // It will not be removed from the list of node children.
    EXPECT_NO_THROW(node->removeSuccessor(childFooPtr, true));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
    EXPECT_TRUE(node->getSuccessorsEmpty());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
}

TEST(Unit_CFGNode, removeSuccessor_ptr_isExecutable)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child2 = std::make_unique<gtirb::CFGNode>();

    auto childFoo = std::make_unique<gtirb::CFGNode>();
    auto childFooPtr = childFoo.get();

    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, node->getSuccessorSize());

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(node->addSuccessor(std::move(child0)));
    EXPECT_NO_THROW(node->addSuccessor(std::move(child1)));
    EXPECT_NO_THROW(node->addSuccessor(std::move(child2)));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
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

    // Now remove Child 1 from the list of successors...but we specify the WRONG isExecuable flag,
    // so nothing happens. It will not be removed from the list of node children.
    EXPECT_NO_THROW(node->removeSuccessor(childFooPtr, false));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
    EXPECT_EQ(size_t{3}, node->getSuccessorSize());
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());

    // This time, we set the correct isExecutable flag, so it does get removed.
    EXPECT_NO_THROW(node->removeSuccessor(childFooPtr, true));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
    EXPECT_TRUE(node->getSuccessorsEmpty());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
}

TEST(Unit_CFGNode, addPredecessor_self)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNode>();
    auto childPtr = child.get();

    EXPECT_NO_THROW(node->addChild(std::move(child)));

    EXPECT_NO_THROW(node->addPredecessor(childPtr));
    EXPECT_EQ(size_t{1}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->addPredecessor(childPtr, true));
    EXPECT_EQ(size_t{2}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->addPredecessor(childPtr, false));
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
}

TEST(Unit_CFGNode, addPredecessor_other)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto other = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNode>();
    auto childPtr = child.get();

    EXPECT_NO_THROW(other->addChild(std::move(child)));
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->addPredecessor(childPtr));
    EXPECT_EQ(size_t{1}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->addPredecessor(childPtr, true));
    EXPECT_EQ(size_t{2}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->addPredecessor(childPtr, false));
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
}

TEST(Unit_CFGNode, addPredecessor_throws)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNode>();
    auto childPtr = child.get();

    // The child has not been added to a child anywhere, so this should fail.
    EXPECT_THROW(node->addPredecessor(childPtr), std::bad_weak_ptr);
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
}

TEST(Unit_CFGNode, addPredecessor_unique)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNode>();

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(node->addPredecessor(std::move(child)));
    EXPECT_EQ(size_t{1}, node->getPredecessorSize());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
}

TEST(Unit_CFGNode, setPredecessor_0)
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

    EXPECT_NO_THROW(node->addChild(std::move(child0)));
    EXPECT_NO_THROW(node->addChild(std::move(child1)));
    EXPECT_NO_THROW(node->addChild(std::move(child2)));
    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, node->getPredecessorSize());

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(node->addPredecessor(child0Ptr));
    EXPECT_NO_THROW(node->addPredecessor(child1Ptr));
    EXPECT_NO_THROW(node->addPredecessor(child2Ptr));
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->setPredecessor(size_t(0), childFooPtr, true));
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
}

TEST(Unit_CFGNode, setPredecessor_1)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child2 = std::make_unique<gtirb::CFGNode>();

    auto childFoo = std::make_unique<gtirb::CFGNode>();
    auto childFooPtr = childFoo.get();

    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, node->getPredecessorSize());

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(node->addPredecessor(std::move(child0)));
    EXPECT_NO_THROW(node->addPredecessor(std::move(child1)));
    EXPECT_NO_THROW(node->addPredecessor(std::move(child2)));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->setPredecessor(size_t(0), childFooPtr, true));

    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(0)).second);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_FALSE(node->getPredecessor(size_t(1)).second);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_FALSE(node->getPredecessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->setPredecessor(size_t(2), childFooPtr, true));

    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(0)).second);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_FALSE(node->getPredecessor(size_t(1)).second);
    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->setPredecessor(size_t(1), childFooPtr, true));

    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(0)).second);
    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(1)).second);
    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
}

TEST(Unit_CFGNode, setPredecessor_throws)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child2 = std::make_unique<gtirb::CFGNode>();

    auto childFoo = std::make_unique<gtirb::CFGNode>();
    auto childFooPtr = childFoo.get();

    EXPECT_NO_THROW(node->addChild(std::move(child0)));
    EXPECT_NO_THROW(node->addChild(std::move(child1)));
    EXPECT_NO_THROW(node->addChild(std::move(child2)));
    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());

    EXPECT_THROW(node->setPredecessor(size_t{64}, childFooPtr), std::out_of_range);
}

TEST(Unit_CFGNode, setPredecessor_move)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child2 = std::make_unique<gtirb::CFGNode>();
    auto childFoo = std::make_unique<gtirb::CFGNode>();

    EXPECT_NO_THROW(node->addChild(std::move(child0)));
    EXPECT_NO_THROW(node->addChild(std::move(child1)));
    EXPECT_NO_THROW(node->addChild(std::move(child2)));
    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
}

TEST(Unit_CFGNode, getPredecessor)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNode>();
    auto childPtr = child.get();

    EXPECT_NO_THROW(node->addChild(std::move(child)));
    EXPECT_TRUE(node->getPredecessorsEmpty());

    EXPECT_NO_THROW(node->addPredecessor(childPtr));
    EXPECT_FALSE(node->getPredecessorsEmpty());
    EXPECT_EQ(size_t{1}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->addPredecessor(childPtr, true));
    EXPECT_EQ(size_t{2}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->addPredecessor(childPtr, false));
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_EQ(size_t{0}, node->getSuccessorSize());

    auto successor0 = node->getPredecessor(size_t{0});
    EXPECT_EQ(childPtr, successor0.first);
    EXPECT_EQ(false, successor0.second);

    auto successor1 = node->getPredecessor(size_t{1});
    EXPECT_EQ(childPtr, successor1.first);
    EXPECT_EQ(true, successor1.second);
}

TEST(Unit_CFGNode, removePredecessor_index)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child2 = std::make_unique<gtirb::CFGNode>();

    auto childFoo = std::make_unique<gtirb::CFGNode>();
    auto childFooPtr = childFoo.get();

    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, node->getPredecessorSize());

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(node->addPredecessor(std::move(child0)));
    EXPECT_NO_THROW(node->addPredecessor(std::move(child1)));
    EXPECT_NO_THROW(node->addPredecessor(std::move(child2)));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->setPredecessor(size_t(0), childFooPtr, true));

    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(0)).second);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_FALSE(node->getPredecessor(size_t(1)).second);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_FALSE(node->getPredecessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->setPredecessor(size_t(2), childFooPtr, true));

    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(0)).second);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_FALSE(node->getPredecessor(size_t(1)).second);
    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->setPredecessor(size_t(1), childFooPtr, true));

    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(0)).second);
    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(1)).second);
    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());

    // Now remove Child 1 from the list of successors.
    // It will not be removed from the list of node children.
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());
    EXPECT_NO_THROW(node->removePredecessor(size_t{1}));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
    EXPECT_EQ(size_t{2}, node->getPredecessorSize());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
}

TEST(Unit_CFGNode, removePredecessor_index_empty)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    EXPECT_NO_THROW(node->removePredecessor(size_t{0}));
}

TEST(Unit_CFGNode, removePredecessor_ptr)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child2 = std::make_unique<gtirb::CFGNode>();

    auto childFoo = std::make_unique<gtirb::CFGNode>();
    auto childFooPtr = childFoo.get();

    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, node->getPredecessorSize());

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(node->addPredecessor(std::move(child0)));
    EXPECT_NO_THROW(node->addPredecessor(std::move(child1)));
    EXPECT_NO_THROW(node->addPredecessor(std::move(child2)));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->setPredecessor(size_t(0), childFooPtr, true));

    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(0)).second);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_FALSE(node->getPredecessor(size_t(1)).second);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_FALSE(node->getPredecessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->setPredecessor(size_t(2), childFooPtr, true));

    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(0)).second);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_FALSE(node->getPredecessor(size_t(1)).second);
    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->setPredecessor(size_t(1), childFooPtr, true));

    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(0)).second);
    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(1)).second);
    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());

    // Now remove Child 1 from the list of successors.
    // It will not be removed from the list of node children.
    EXPECT_NO_THROW(node->removePredecessor(childFooPtr, true));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
    EXPECT_TRUE(node->getPredecessorsEmpty());
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
}

TEST(Unit_CFGNode, removePredecessor_ptr_isExecutable)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child2 = std::make_unique<gtirb::CFGNode>();

    auto childFoo = std::make_unique<gtirb::CFGNode>();
    auto childFooPtr = childFoo.get();

    EXPECT_NO_THROW(node->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, node->getPredecessorSize());

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(node->addPredecessor(std::move(child0)));
    EXPECT_NO_THROW(node->addPredecessor(std::move(child1)));
    EXPECT_NO_THROW(node->addPredecessor(std::move(child2)));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->setPredecessor(size_t(0), childFooPtr, true));

    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(0)).second);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_FALSE(node->getPredecessor(size_t(1)).second);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_FALSE(node->getPredecessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->setPredecessor(size_t(2), childFooPtr, true));

    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(0)).second);
    EXPECT_NE(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_FALSE(node->getPredecessor(size_t(1)).second);
    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());

    EXPECT_NO_THROW(node->setPredecessor(size_t(1), childFooPtr, true));

    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(0)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(0)).second);
    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(1)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(1)).second);
    EXPECT_EQ(childFooPtr, node->getPredecessor(size_t(2)).first);
    EXPECT_TRUE(node->getPredecessor(size_t(2)).second);
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());

    // Now remove Child 1 from the list of successors...but we specify the WRONG isExecuable flag,
    // so nothing happens. It will not be removed from the list of node children.
    EXPECT_NO_THROW(node->removePredecessor(childFooPtr, false));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
    EXPECT_EQ(size_t{3}, node->getPredecessorSize());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());

    // This time, we set the correct isExecutable flag, so it does get removed.
    EXPECT_NO_THROW(node->removePredecessor(childFooPtr, true));
    EXPECT_EQ(size_t{4}, node->getChildrenSize());
    EXPECT_TRUE(node->getPredecessorsEmpty());
    EXPECT_EQ(size_t{0}, node->getPredecessorSize());
    EXPECT_EQ(size_t{0}, node->getSuccessorSize());
}

TEST(Unit_CFGNode, preventSelfReferencesForPredecessors)
{
    auto parent = std::make_unique<gtirb::CFGNode>();

    auto node = std::make_unique<gtirb::CFGNode>();
    auto nodePtr = node.get();

    EXPECT_NO_THROW(parent->addChild(std::move(node)));

    EXPECT_THROW(nodePtr->addPredecessor(nodePtr), gtirb::NodeStructureError);
}

TEST(Unit_CFGNode, preventSelfReferencesForSuccessors)
{
    auto parent = std::make_unique<gtirb::CFGNode>();

    auto node = std::make_unique<gtirb::CFGNode>();
    auto nodePtr = node.get();

    EXPECT_NO_THROW(parent->addChild(std::move(node)));

    EXPECT_THROW(nodePtr->addSuccessor(nodePtr), gtirb::NodeStructureError);
}

TEST(Unit_CFGNode, serialize)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    auto original = std::make_unique<gtirb::CFGNode>();
    auto child0 = std::make_unique<gtirb::CFGNode>();
    auto child1 = std::make_unique<gtirb::CFGNode>();
    auto child2 = std::make_unique<gtirb::CFGNode>();

    auto childFoo = std::make_unique<gtirb::CFGNode>();

    EXPECT_NO_THROW(original->addChild(std::move(childFoo)));

    EXPECT_EQ(size_t{0}, original->getPredecessorSize());

    // We will add it as a child first, then add it as a successor.
    EXPECT_NO_THROW(original->addPredecessor(std::move(child0)));
    EXPECT_NO_THROW(original->addPredecessor(std::move(child1)));
    EXPECT_NO_THROW(original->addPredecessor(std::move(child2)));
    EXPECT_EQ(size_t{4}, original->getChildrenSize());
    EXPECT_EQ(size_t{3}, original->getPredecessorSize());

    EXPECT_NO_THROW(original->setLocalProperty("Name", std::string("Value")));
    EXPECT_EQ(size_t{1}, original->getLocalPropertySize());
    EXPECT_EQ(std::string{"Value"}, boost::get<std::string>(original->getLocalProperty("Name")));

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
        auto serialized = std::make_unique<gtirb::CFGNode>();

        // Serialize In.
        std::ifstream ifs{tempPathString.c_str()};
        boost::archive::polymorphic_text_iarchive ia{ifs};

        EXPECT_NO_THROW(ia >> serialized);

        EXPECT_NO_THROW(ifs.close());

        EXPECT_EQ(size_t{4}, serialized->getChildrenSize());
        EXPECT_EQ(size_t{3}, serialized->getPredecessorSize());

        EXPECT_EQ(size_t{1}, serialized->getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"},
                  boost::get<std::string>(serialized->getLocalProperty("Name")));
    }
}
