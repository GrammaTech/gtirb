#include <gsl/gsl>
#include <gtirb/CFG.hpp>
#include <gtirb/CFGNode.hpp>
#include <gtirb/CFGNodeInfo.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/RuntimeError.hpp>
#include <iostream>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::CFGNode);

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

void CFGNode::addChild(std::unique_ptr<gtirb::CFGNode>&& x)
{
    this->children.push_back(std::move(x));
}

void CFGNode::addSuccessor(CFGNode* x, bool isExecutable)
{
    this->add(this->successors, x, isExecutable);
}

void CFGNode::addSuccessor(std::unique_ptr<CFGNode>&& x, bool isExecutable)
{
    this->add(this->successors, std::move(x), isExecutable);
}

void CFGNode::setSuccessor(size_t index, CFGNode* x, bool isExecutable)
{
    this->set(this->successors, index, x, isExecutable);
}

void CFGNode::setSuccessor(size_t index, std::unique_ptr<CFGNode>&& x, bool isExecutable)
{
    this->set(this->successors, index, std::move(x), isExecutable);
}

std::pair<CFGNode*, bool> CFGNode::getSuccessor(size_t x) const
{
    return this->get(this->successors, x);
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
    this->remove(this->successors, x);
}

void CFGNode::removeSuccessor(const CFGNode* const x)
{
    this->remove(this->successors, x);
}

void CFGNode::removeSuccessor(const CFGNode* const x, bool isExecutable)
{
    this->remove(this->successors, x, isExecutable);
}

void CFGNode::addPredecessor(CFGNode* x, bool isExecutable)
{
    this->add(this->predecessors, x, isExecutable);
}

void CFGNode::addPredecessor(std::unique_ptr<CFGNode>&& x, bool isExecutable)
{
    this->add(this->predecessors, std::move(x), isExecutable);
}

void CFGNode::setPredecessor(size_t index, CFGNode* x, bool isExecutable)
{
    this->set(this->predecessors, index, x, isExecutable);
}

void CFGNode::setPredecessor(size_t index, std::unique_ptr<CFGNode>&& x, bool isExecutable)
{
    this->set(this->predecessors, index, std::move(x), isExecutable);
}

std::pair<CFGNode*, bool> CFGNode::getPredecessor(size_t x) const
{
    return this->get(this->predecessors, x);
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
    this->remove(this->predecessors, x);
}

void CFGNode::removePredecessor(const CFGNode* const x)
{
    this->remove(this->predecessors, x);
}

void CFGNode::removePredecessor(const CFGNode* const x, bool isExecutable)
{
    this->remove(this->predecessors, x, isExecutable);
}

void CFGNode::setCFGNodeInfo(std::unique_ptr<CFGNodeInfo>&& x)
{
    this->info = std::move(x);
}

CFGNodeInfo* CFGNode::getCFGNodeInfo() const
{
    return this->info.get();
}

void CFGNode::setLoadedInstructionBytes(uint8_t* x)
{
    this->loadedInstructionBytes = x;
}

uint8_t* CFGNode::getLoadedInstructionBytes() const
{
    return this->loadedInstructionBytes;
}

void CFGNode::add(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec, CFGNode* x,
                  bool isExecutable)
{
    if(x != this)
    {
        auto sharedNode = std::dynamic_pointer_cast<CFGNode>(x->shared_from_this());
        Expects(sharedNode != nullptr);
        vec.push_back({sharedNode, isExecutable});
    }
    else
    {
        throw gtirb::NodeStructureError(
            "Attempt to add a CFGNode to itself as a successor/predecessor.");
    }
}

void CFGNode::add(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec,
                  std::unique_ptr<CFGNode>&& x, bool isExecutable)
{
    auto xPtr = x.get();
    this->children.push_back(std::move(x));
    this->add(vec, xPtr, isExecutable);
}

void CFGNode::set(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec, size_t index,
                  CFGNode* x, bool isExecutable)
{
    if(x != this)
    {
        auto sharedNode = std::dynamic_pointer_cast<CFGNode>(x->shared_from_this());

        if(index < vec.size())
        {
            vec[index] = {sharedNode, isExecutable};
        }
        else if(index == vec.size())
        {
            vec.push_back({sharedNode, isExecutable});
        }
        else
        {
            throw std::out_of_range(
                "Attempt to set a CFGNode by an index outside the range of "
                "successors/predecessors.");
        }
    }
    else
    {
        throw gtirb::NodeStructureError(
            "Attempt to add a CFGNode to itself as a successor/predecessor.");
    }
}

void CFGNode::set(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec, size_t index,
                  std::unique_ptr<CFGNode>&& x, bool isExecutable)
{
    auto xPtr = x.get();
    this->children.push_back(std::move(x));
    this->set(vec, index, xPtr, isExecutable);
}

std::pair<CFGNode*, bool> CFGNode::get(
    const std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec, size_t x) const
{
    auto s = vec[x];
    return {s.first.lock().get(), s.second};
}

void CFGNode::remove(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec, size_t x)
{
    auto idx = std::begin(vec);
    std::advance(idx, x);

    if(idx != std::end(vec))
    {
        vec.erase(idx);
    }
}

void CFGNode::remove(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec,
                     const CFGNode* const x)
{
    vec.erase(std::remove_if(std::begin(vec), std::end(vec),
                             [x](const auto& s) { return (s.first.lock().get() == x); }),
              std::end(vec));
}

void CFGNode::remove(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec,
                     const CFGNode* const x, bool isExecutable)
{
    vec.erase(std::remove_if(std::begin(vec), std::end(vec),
                             [x, isExecutable](const auto& s) {
                                 return (s.first.lock().get() == x) && (s.second == isExecutable);
                             }),
              std::end(vec));
}

size_t CFGNode::getChildrenSize() const
{
    return this->children.size();
}
