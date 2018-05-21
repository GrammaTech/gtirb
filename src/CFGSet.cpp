#include <gtirb/CFG.hpp>
#include <gtirb/CFGSet.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeValidators.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::CFGSet);

CFGSet::CFGSet() : Node()
{
    this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>);
    this->addParentValidator(NodeValidatorHasNoSiblingsOfType<gtirb::CFGSet>);
}

CFG* CFGSet::getCFG(gtirb::EA x) const
{
    const auto allChildren = GetChildrenOfType<CFG>(this);
    const auto found = std::find_if(std::begin(allChildren), std::end(allChildren),
                                    [x](CFG* s) { return s->getEA() == x; });

    if(found != std::end(allChildren))
    {
        return *found;
    }

    return nullptr;
}

CFG* CFGSet::getCFG(const std::string& x) const
{
    const auto allChildren = GetChildrenOfType<CFG>(this);
    const auto found = std::find_if(std::begin(allChildren), std::end(allChildren),
                                    [x](CFG* s) { return s->getProcedureName() == x; });

    if(found != std::end(allChildren))
    {
        return *found;
    }

    return nullptr;
}

CFG* CFGSet::getOrCreateCFG(gtirb::EA x)
{
    auto child = this->getCFG(x);

    if(child == nullptr)
    {
        auto newChild = std::make_unique<CFG>();
        newChild->setEA(x);
        child = newChild.get();
        this->push_back(std::move(newChild));
    }

    return child;
}
