#include <proto/IR.pb.h>
#include <gtirb/IR.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Serialization.hpp>
#include <gtirb/Table.hpp>

using namespace gtirb;

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

Table& IR::addTable(std::string name, std::unique_ptr<Table>&& x)
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

void IR::toProtobuf(MessageType* message) const
{
    nodeUUIDToBytes(this, *message->mutable_uuid());

    auto* modules = message->mutable_modules();
    modules->Clear();
    modules->Reserve(this->modules.size());
    std::for_each(this->modules.begin(), this->modules.end(),
                  [modules](const auto& n) { n->toProtobuf(modules->Add()); });

    nodeUUIDToBytes(this->mainModule.lock().get(), *message->mutable_main_module_id());
    containerToProtobuf(this->tables, message->mutable_tables());
}

void IR::fromProtobuf(const MessageType& message)
{
    setNodeUUIDFromBytes(this, message.uuid());

    const auto& messages = message.modules();
    this->modules.clear();
    this->modules.reserve(messages.size());
    std::for_each(messages.begin(), messages.end(), [this](const auto& message) {
        auto val = std::make_shared<Module>();
        val->fromProtobuf(message);
        this->modules.push_back(std::move(val));
    });

    UUID mainId = uuidFromBytes(message.main_module_id());
    auto foundMain = std::find_if(this->modules.begin(), this->modules.end(),
                                  [mainId](const auto& m) { return m->getUUID() == mainId; });
    assert(foundMain != this->modules.end());
    this->mainModule = *foundMain;
    containerFromProtobuf(this->tables, message.tables());
}

void IR::save(std::ostream& out) const
{
    MessageType message;
    this->toProtobuf(&message);
    message.SerializeToOstream(&out);
}

void IR::load(std::istream& in)
{
    MessageType message;
    message.ParseFromIstream(&in);
    this->fromProtobuf(message);
}
