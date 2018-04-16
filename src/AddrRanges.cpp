#include <gtirb/AddrRanges.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/RuntimeError.hpp>

using namespace gtirb;

AddrRanges::AddrRanges() : Node()
{
    this->addParentValidator([](const Node* const x) {
        // We can only be a child to a gtirb::Module.
        const auto parent = dynamic_cast<const gtirb::Module* const>(x);
        return parent != nullptr;
    });
}

void AddrRanges::addRange(std::pair<gtirb::EA, gtirb::EA> x)
{
    if(x.first <= x.second)
    {
        this->rangeVector.push_back(std::move(x));
    }
    else
    {
        throw gtirb::RuntimeError(
            "Address range pairs must have the first value less than the second value.", __FILE__,
            __LINE__);
    }
}

std::vector<std::pair<gtirb::EA, gtirb::EA>>& AddrRanges::getRangeVector()
{
    return this->rangeVector;
}

const std::vector<std::pair<gtirb::EA, gtirb::EA>>& AddrRanges::getRangeVector() const
{
    return this->rangeVector;
}

size_t AddrRanges::getBytesCoveredByRanges() const
{
    size_t totalBytes{0};

    for(auto& i : this->rangeVector)
    {
        // A check to run in debugging.
        assert(i.first <= i.second);

        totalBytes += i.second - i.first;
    }

    return totalBytes;
}
