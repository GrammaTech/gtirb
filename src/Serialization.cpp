#include <gtirb/Node.hpp>
#include <gtirb/Serialization.hpp>

namespace gtirb
{
    UUID uuidFromBytes(const std::string& bytes)
    {
        UUID id;
        Expects(bytes.size() == sizeof(id.data));
        std::copy(bytes.begin(), bytes.end(), std::begin(id.data));
        return id;
    }

    void uuidToBytes(UUID uuid, std::string& bytes)
    {
        bytes.clear();
        bytes.reserve(sizeof(uuid.data));
        std::copy(std::begin(uuid.data), std::end(uuid.data), std::back_inserter(bytes));
    }

    void nodeUUIDToBytes(const Node* node, std::string& bytes)
    {
        uuidToBytes(node->getUUID(), bytes);
    }

    void setNodeUUIDFromBytes(Node* node, const std::string& bytes)
    {
        node->setUUID(uuidFromBytes(bytes));
    }

    uint64_t toProtobuf(const EA val)
    {
        return val.get();
    }

    std::string toProtobuf(const std::string& val)
    {
        return val;
    }

    int64_t toProtobuf(const int64_t& val)
    {
        return val;
    }

    uint64_t toProtobuf(const uint64_t& val)
    {
        return val;
    }

    void fromProtobuf(EA& result, const uint64_t& message)
    {
        result = EA(message);
    }
}
