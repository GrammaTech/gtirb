#include <gtirb/CFG.hpp>
#include <gtirb/CFGNode.hpp>
#include <gtirb/CFGNodeInfo.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/NodeValidators.hpp>
#include <gtirb/NodeUtilities.hpp>
#include <gtirb/RuntimeError.hpp>
#include <cassert>

using namespace gtirb;

CFGNode::CFGNode() : Node()
{
    this->addParentValidator([](const Node* const x) {
        const auto parentCFG = dynamic_cast<const gtirb::CFG* const>(x);
        const auto parentCFGNode = dynamic_cast<const gtirb::CFGNode* const>(x);
        return ((parentCFG != nullptr) || (parentCFGNode != nullptr));
    });
}

void CFGNode::setEA(EA x)
{
    this->ea = x;
}

EA CFGNode::getEA() const
{
    return this->ea;
}

void CFGNode::setKind(CFGNode::Kind x)
{
    this->kind = x;
}

CFGNode::Kind CFGNode::getKind() const
{
    return this->kind;
}

void CFGNode::addSuccessor(CFGNode* x, bool isExecutable)
{
    auto sharedNode = std::dynamic_pointer_cast<CFGNode>(x->shared_from_this());
    assert(sharedNode != nullptr);
    this->successors.push_back({sharedNode, isExecutable});
}

void CFGNode::addSuccessor(std::unique_ptr<CFGNode>&& x, bool isExecutable)
{
    auto xPtr = x.get();
    this->push_back(std::move(x));
    this->addSuccessor(xPtr, isExecutable);
}

void CFGNode::setSuccessor(size_t index, CFGNode* x, bool isExecutable)
{
    auto sharedNode = std::dynamic_pointer_cast<CFGNode>(x->shared_from_this());

    if(index < this->successors.size())
    {
        this->successors[index] = {sharedNode, isExecutable};
    }
    else
    {
        throw std::out_of_range("Attempt to set a successor CFGNode by an index outside the range of successors.");
    }
}

void CFGNode::setSuccessor(size_t index, std::unique_ptr<CFGNode>&& x, bool isExecutable)
{
    auto xPtr = x.get();
    this->push_back(std::move(x));
    this->setSuccessor(index, xPtr, isExecutable);
}

std::pair<CFGNode*, bool> CFGNode::getSuccessor(size_t x) const
{
    auto s = this->successors[x];
    return {s.first.lock().get(), s.second};
}

bool CFGNode::getSuccessorsEmpty() const
{
    return this->successors.empty();
}

size_t CFGNode::getSuccessorSize() const
{
    return this->successors.size();
}

void CFGNode::removeSuccessor(size_t x)
{
    auto idx = std::begin(this->successors);
    std::advance(idx, x);
    this->successors.erase(idx);
}

void CFGNode::removeSuccessor(const CFGNode* const x)
{
    this->successors.erase(
        std::remove_if(std::begin(this->successors), std::end(this->successors),
                       [x](const auto& s) { return (s.first.lock().get() == x); }),
        std::end(this->successors));
}

void CFGNode::removeSuccessor(const CFGNode* const x, bool isExecutable)
{
    this->successors.erase(std::remove_if(std::begin(this->successors), std::end(this->successors),
                                          [x, isExecutable](const auto& s) {
                                              return (s.first.lock().get() == x)
                                                     && (s.second == isExecutable);
                                          }),
                           std::end(this->successors));
}

void CFGNode::addPredecessor(CFGNode* x, bool isExecutable)
{
    auto sharedNode = std::dynamic_pointer_cast<CFGNode>(x->shared_from_this());
    assert(sharedNode != nullptr);
    this->predecessors.push_back({sharedNode, isExecutable});
}

void CFGNode::addPredecessor(std::unique_ptr<CFGNode>&& x, bool isExecutable)
{
    auto xPtr = x.get();
    this->push_back(std::move(x));
    this->addPredecessor(xPtr, isExecutable);
}

void CFGNode::setPredecessor(size_t index, CFGNode* x, bool isExecutable)
{
    auto sharedNode = std::dynamic_pointer_cast<CFGNode>(x->shared_from_this());
    this->predecessors[index] = {sharedNode, isExecutable};
}

void CFGNode::setPredecessor(size_t index, std::unique_ptr<CFGNode>&& x, bool isExecutable)
{
    auto xPtr = x.get();
    this->push_back(std::move(x));
    this->setPredecessor(index, xPtr, isExecutable);
}

std::pair<CFGNode*, bool> CFGNode::getPredecessor(size_t x) const
{
    auto s = this->predecessors[x];
    return {s.first.lock().get(), s.second};
}

bool CFGNode::getPredecessorsEmpty() const
{
    return this->predecessors.empty();
}

size_t CFGNode::getPredecessorSize() const
{
    return this->predecessors.size();
}

void CFGNode::removePredecessor(size_t x)
{
    auto idx = std::begin(this->predecessors);
    std::advance(idx, x);
    this->predecessors.erase(idx);
}

void CFGNode::removePredecessor(const CFGNode* const x)
{
    this->predecessors.erase(
        std::remove_if(std::begin(this->predecessors), std::end(this->predecessors),
                       [x](const auto& s) { return (s.first.lock().get() == x); }),
        std::end(this->predecessors));
}

void CFGNode::removePredecessor(const CFGNode* const x, bool isExecutable)
{
    this->predecessors.erase(std::remove_if(std::begin(this->predecessors), std::end(this->predecessors),
                                          [x, isExecutable](const auto& s) {
                                              return (s.first.lock().get() == x)
                                                     && (s.second == isExecutable);
                                          }),
                           std::end(this->predecessors));
}

CFGNodeInfo* CFGNode::getCFGNodeInfo() const
{
    const auto children = GetChildrenOfType<CFGNodeInfo>(this);

    if(children.empty() == false)
    {
        return children[0];
    }

    return nullptr;
}
