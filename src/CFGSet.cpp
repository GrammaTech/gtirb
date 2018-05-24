#include <gtirb/CFG.hpp>
#include <gtirb/CFGNode.hpp>
#include <gtirb/CFGNodeInfo.hpp>
#include <gtirb/CFGSet.hpp>
#include <gtirb/Module.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::CFGSet);

CFG* CFGSet::getCFG(gtirb::EA x) const
{
    auto found = std::find_if(contents.begin(), contents.end(),
                              [x](const auto& s) { return s->getEA() == x; });

    if(found != contents.end())
    {
        return found->get();
    }

    return nullptr;
}

CFG* CFGSet::getCFG(const std::string& x) const
{
    auto found = std::find_if(contents.begin(), contents.end(),
                              [x](const auto& s) { return s->getProcedureName() == x; });

    if(found != contents.end())
    {
        return found->get();
    }

    return nullptr;
}

CFG* CFGSet::getOrCreateCFG(gtirb::EA x)
{
    auto child = this->getCFG(x);

    if(child)
    {
        return child;
    }

    auto newChild = std::make_shared<CFG>();
    newChild->setEA(x);
    child = newChild.get();
    contents.push_back(std::move(newChild));

    return this->getCFG(x);
}

template <class Archive>
void CFGSet::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar& boost::serialization::base_object<Node>(*this);
    ar & this->contents;
}

const std::vector<std::shared_ptr<CFG>>& CFGSet::getCFGs() const
{
    return this->contents;
}
