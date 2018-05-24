#include <gsl/gsl>
#include <gtirb/AddrRanges.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/RuntimeError.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::AddrRanges);

bool AddrRanges::addRange(std::pair<gtirb::EA, gtirb::EA> x)
{
    if(x.first <= x.second)
    {
        if(this->ranges.empty() == true)
        {
            this->ranges.insert(x);
            return true;
        }

        auto next = this->ranges.upper_bound(x.first);

        if(next != std::begin(this->ranges))
        {
            auto prev = next;
            --prev;

            if(prev->second >= x.first)
            {
                // prev  next next2 ...
                // case 1:  11
                // case 2:  2222
                // case 3:  333333
                // case 4:  44444444444444

                // Case 1:
                if(prev->second >= x.second)
                {
                    return false;
                }

                // Cases 2-4:
                while(next != std::end(this->ranges) && next->first <= x.second)
                {
                    if(x.second < next->second)
                    {
                        x.second = next->second;
                    }

                    const auto tmp = next;
                    ++next;

                    this->ranges.erase(tmp);
                }

                prev->second = x.second;
                return true;
            }
        }

        // next  next2 ...
        // case 5: 55
        // case 6: 6666
        // case 7: 77777777
        // case 8: 888888888888

        while(next != this->ranges.end() && next->first <= x.second)
        {
            if(x.second < next->second)
            {
                x.second = next->second;
            }

            const auto tmp = next;
            ++next;

            this->ranges.erase(tmp);
        }

        return this->ranges.insert(x).second;
    }
    else
    {
        throw gtirb::RuntimeError(
            "Address range pairs must have the first value less than the second value.", __FILE__,
            __LINE__);
    }
}

bool AddrRanges::addRange(const AddrRanges& x)
{
    bool success = true;

    for(const auto& range : x.data())
    {
        success &= this->addRange(range);
    }

    return success;
}

bool AddrRanges::subtractRange(std::pair<gtirb::EA, gtirb::EA> x)
{
    bool success = false;

    if((x.second > x.first) && (this->ranges.empty() == false))
    {
        auto it = this->ranges.lower_bound(x.second);

        if(it == std::begin(this->ranges))
        {
            return false;
        }

        --it;

        while(true)
        {
            Expects(it->first < x.second);

            if(it->first < x.first)
            {
                // cases 1,2,3
                if(it->second <= x.first)
                {
                    return success;
                }

                if(it->second <= x.second)
                {
                    // case 2: truncate
                    it->second = x.first;
                }
                else
                {
                    // if (it->second > x.second): // case 3: split
                    const auto newLowerBound = x.second;
                    const auto newUpperBound = it->second;

                    it->second = x.first;
                    this->ranges.insert({newLowerBound, newUpperBound});
                }

                return true;
            }
            else
            {
                // if (it->first >= x.first) // cases 4,5,6,7
                // Remember iterator in tmp, decrement it for next round if needed.
                auto tmp = it;
                auto cont = false;

                if((it->first > x.first) && (it != std::begin(this->ranges)))
                {
                    --it;
                    cont = true;
                }

                // Adjust w.r.t. tmp iterator.
                if(tmp->second <= x.second)
                {
                    // cases 4,6
                    this->ranges.erase(tmp);
                }
                else
                {
                    // if (tmp->second > x.second): // case 5,7
                    const auto newLowerBound = x.second;
                    const auto newUpperBound = tmp->second;

                    this->ranges.erase(tmp);
                    this->ranges.insert({newLowerBound, newUpperBound});
                }

                // Break or continue
                if(!cont)
                {
                    return true;
                }

                success = true;
                continue;
            }
        }
    }

    return success;
}

bool AddrRanges::subtractRange(const AddrRanges& x)
{
    bool success = true;

    for(const auto& range : x.data())
    {
        success &= this->subtractRange(range);
    }

    return success;
}

size_t AddrRanges::getBytesCoveredByRanges() const
{
    size_t rv{0};

    for(const auto& range : ranges)
    {
        rv += range.second - range.first;
    }

    return rv;
}

bool AddrRanges::getContains(gtirb::EA x) const
{
    if(this->ranges.empty() == false)
    {
        auto it = this->ranges.upper_bound(x);

        if(it == std::begin(this->ranges))
        {
            return false;
        }

        --it;
        return (x < it->second);
    }

    return false;
}

void AddrRanges::clearRanges()
{
    this->ranges.clear();
}

void AddrRanges::swap(gtirb::AddrRanges& x)
{
    this->ranges.swap(x.ranges);
}

std::map<gtirb::EA, gtirb::EA>::const_iterator AddrRanges::getFirstIntersecting(gtirb::EA lb,
                                                                                gtirb::EA ub) const
{
    const auto it1 = this->ranges.upper_bound(lb);

    if(it1 != std::begin(this->ranges))
    {
        auto it0 = it1;
        --it0;

        if(it0->second > lb)
        {
            return it0;
        }
    }

    if(it1 != std::end(this->ranges) && it1->first < ub)
    {
        return it1;
    }

    return std::end(this->ranges);
}

std::map<gtirb::EA, gtirb::EA>::const_iterator AddrRanges::getRangeContaining(gtirb::EA x) const
{
    auto it = this->ranges.upper_bound(x);

    if(it == std::begin(this->ranges))
    {
        return std::end(this->ranges);
    }

    --it;

    if(it->second > x)
    {
        return it;
    }

    return std::end(this->ranges);
}

void AddrRanges::swapAndClear(std::map<gtirb::EA, gtirb::EA>& x)
{
    this->ranges.swap(x);
    this->ranges.clear();
}

std::map<gtirb::EA, gtirb::EA>& AddrRanges::data()
{
    return this->ranges;
}

const std::map<gtirb::EA, gtirb::EA>& AddrRanges::data() const
{
    return this->ranges;
}
