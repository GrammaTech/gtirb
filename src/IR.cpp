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
        auto mm = std::make_unique<Module>();
        main = mm.get();
        this->push_back(std::move(mm));
        this->mainModule = std::dynamic_pointer_cast<gtirb::Module>(main->shared_from_this());
    }

    return main;
}

std::vector<gtirb::Module*> IR::getModulesWithPreferredEA(EA x) const
{
    std::vector<gtirb::Module*> modules;
    std::vector<gtirb::Node*> nodes;

    std::copy_if(this->begin(), this->end(), std::back_inserter(nodes), [x](auto m) {
        const auto module = dynamic_cast<Module*>(m);
        return (module != nullptr) && (module->getPreferredEA() == x);
    });

    if(nodes.empty() == false)
    {
        for(auto n : nodes)
        {
            modules.push_back(dynamic_cast<Module*>(n));
        }
    }

    return modules;
}

std::vector<gtirb::Module*> IR::getModulesContainingEA(EA x) const
{
    std::vector<gtirb::Module*> modules;
    std::vector<gtirb::Node*> nodes;

    std::copy_if(this->begin(), this->end(), std::back_inserter(nodes), [x](auto m) {
        const auto module = dynamic_cast<Module*>(m);

        if(module != nullptr)
        {
            auto minmax = module->getEAMinMax();
            return (x >= minmax.first) && (x < minmax.second);
        }

        return false;
    });

    if(nodes.empty() == false)
    {
        for(auto n : nodes)
        {
            modules.push_back(dynamic_cast<Module*>(n));
        }
    }

    return modules;
}
