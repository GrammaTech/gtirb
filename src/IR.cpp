#include <gtirb/IR.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Table.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::IR);

IR::IR() : Node()
{
    // Create a main module
    auto mm = std::make_shared<Module>();
    this->mainModule = mm;
    this->modules.push_back(std::move(mm));
}

Module& IR::getMainModule()
{
    return *this->mainModule.lock().get();
}

const Module& IR::getMainModule() const
{
    return *this->mainModule.lock().get();
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

Table& IR::addTable(std::string name, std::unique_ptr<gtirb::Table>&& x)
{
    return *(this->tables[std::move(name)] = std::move(x)).get();
}

gtirb::Table* const IR::getTable(const std::string& x) const
{
    const auto found = this->tables.find(x);
    if(found != std::end(this->tables))
    {
        return (*found).second.get();
    }

    return nullptr;
}

bool IR::removeTable(const std::string& x)
{
    const auto found = this->tables.find(x);

    if(found != std::end(this->tables))
    {
        this->tables.erase(found);
        return true;
    }

    return false;
}

size_t IR::getTableSize() const
{
    return this->tables.size();
}

bool IR::getTablesEmpty() const
{
    return this->tables.empty();
}

void IR::clearTables()
{
    this->tables.clear();
}

template <class Archive>
void IR::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar& boost::serialization::base_object<Node>(*this);
    ar& modules;
    ar & this->mainModule;
    ar & this->tables;
}
