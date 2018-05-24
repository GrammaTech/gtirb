#include <gtirb/IR.hpp>
#include <gtirb/Module.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::IR);

IR::IR() : Node()
{
}

Module* IR::getMainModule() const
{
    return this->mainModule.lock().get();
}

Module* IR::getOrCreateMainModule()
{
    auto main = this->mainModule.lock().get();

    if(main == nullptr)
    {
        // Create a new Main Module then get its pointer value to return.
        auto mm = std::make_shared<Module>();
        this->mainModule = mm;
        main = mm.get();
        this->modules.push_back(std::move(mm));
    }

    return main;
}

std::vector<Module*> IR::getModulesWithPreferredEA(EA x) const
{
    std::vector<Module*> results;

    for(const auto& m : this->modules)
    {
        if(m->getPreferredEA() == x)
        {
            results.push_back(m.get());
        }
    }

    return results;
}

std::vector<Module*> IR::getModulesContainingEA(EA x) const
{
    std::vector<Module*> results;

    for(const auto& m : this->modules)
    {
        auto minmax = m->getEAMinMax();
        if((x >= minmax.first) && (x < minmax.second))
        {
            results.push_back(m.get());
        }
    }

    return results;
}

void IR::addModule(std::unique_ptr<gtirb::Module>&& x)
{
    Expects(x != nullptr);
    this->modules.push_back(std::move(x));
}
