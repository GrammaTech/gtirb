#include <gsl/gsl>
#include <gtirb/CFG.hpp>
#include <gtirb/CFGNode.hpp>
#include <gtirb/CFGNodeInfoCall.hpp>
#include <gtirb/CFGSet.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Utilities.hpp>
#include <iostream>

using namespace gtirb;

std::vector<uint16_t> gtirb::utilities::ByteArray8To16(const std::vector<uint8_t>& x, bool swap)
{
    std::vector<uint16_t> vec;

    union Conversion {
        uint8_t Bytes[sizeof(uint16_t)];
        uint16_t Word;
    };

    if(x.size() % sizeof(uint16_t) != 0)
    {
        throw std::range_error(
            "The input vector held an incorrect number of bytes and could not be converted.");
    }

    for(int i = 0; i < static_cast<int>(x.size()); i += static_cast<int>(sizeof(uint16_t)))
    {
        Conversion cvt{0};

        for(size_t j = 0; j < sizeof(uint16_t); ++j)
        {
            cvt.Bytes[j] = x[i + j];
        }

        if(swap == true)
        {
            cvt.Word = gtirb::utilities::SwapEndian(cvt.Word);
        }

        vec.push_back(cvt.Word);
    }

    return vec;
}

std::vector<uint32_t> gtirb::utilities::ByteArray8To32(const std::vector<uint8_t>& x, bool swap)
{
    std::vector<uint32_t> vec;

    union Conversion {
        uint8_t Bytes[sizeof(uint32_t)];
        uint32_t Word;
    };

    if(x.size() % sizeof(uint32_t) != 0)
    {
        throw std::range_error(
            "The input vector held an incorrect number of bytes and could not be converted.");
    }

    for(int i = 0; i < static_cast<int>(x.size()); i += static_cast<int>(sizeof(uint32_t)))
    {
        Conversion cvt{0};

        for(size_t j = 0; j < sizeof(uint32_t); ++j)
        {
            cvt.Bytes[j] = x[i + j];
        }

        if(swap == true)
        {
            cvt.Word = gtirb::utilities::SwapEndian(cvt.Word);
        }

        vec.push_back(cvt.Word);
    }

    return vec;
}

std::vector<uint64_t> gtirb::utilities::ByteArray8To64(const std::vector<uint8_t>& x, bool swap)
{
    std::vector<uint64_t> vec;

    union Conversion {
        uint8_t Bytes[sizeof(uint64_t)];
        uint64_t Word;
    };

    if(x.size() % sizeof(uint64_t) != 0)
    {
        throw std::range_error(
            "The input vector held an incorrect number of bytes and could not be converted.");
    }

    for(int i = 0; i < static_cast<int>(x.size()); i += static_cast<int>(sizeof(uint64_t)))
    {
        Conversion cvt{0};

        for(size_t j = 0; j < sizeof(uint64_t); ++j)
        {
            cvt.Bytes[j] = x[i + j];
        }

        if(swap == true)
        {
            cvt.Word = gtirb::utilities::SwapEndian(cvt.Word);
        }

        vec.push_back(cvt.Word);
    }

    return vec;
}

///
/// Helper: Given that cfg is a thunk, discover its forward.
/// This version walks the CFG (compare get_thunk_targets_via_asts).
///
const auto X86GetThunkTarget = [](const Module* const /*module*/, const CFG* const cfg) {
    // We find the thunk's target by exploring at the nCFGnode level.
    // We assume that cfg contains a single relevant call or indrect node,
    // and pick up the target from there.
    // Note that this will pick up the names of imported callees (see
    // update_imported_callees).
    // (An alternative approach that examines the ASTs would fail to pick
    // this up).

    for(auto& node : cfg->getNodes())
    {
        const auto kind = node->getKind();

        if((kind == CFGNode::Kind::Call) || (kind == CFGNode::Kind::Indirect))
        {
            auto call = dynamic_cast<CFGNodeInfoCall*>(node->getCFGNodeInfo());

            if(call != nullptr)
            {
                /// \todo How is CFGNodeInfoCall->getProcedureName() wired in?  Do we have to
                /// set it or can we compute it? auto callee = call->getProcedureName();
                auto ea = call->getImportTableEntryEA();
                // return {ea, callee};
                return std::pair<gtirb::EA, gtirb::Symbol*>{ea, nullptr};
            }
        }
    }

    return std::pair<gtirb::EA, gtirb::Symbol*>{gtirb::EA{}, nullptr};
};

std::set<CFG*> gtirb::utilities::CollectThunks(const Module* const module)
{
    std::set<CFG*> thunks;

    if(module != nullptr)
    {
        auto cfgSet = module->getCFGSet();

        if(cfgSet != nullptr)
        {
            // Function signature declaration.
            std::function<std::pair<EA, Symbol*>(const Module* const m, const CFG* const cfg)>
                getThunkTargetFunc = [](const Module* const /*m*/, const CFG* const /*cfg*/) {
                    return std::pair<EA, Symbol*>{gtirb::EA{}, nullptr};
                };

            switch(module->getISAID())
            {
                case gtirb::ISAID::IA32:
                case gtirb::ISAID::X64:
                    getThunkTargetFunc = X86GetThunkTarget;
                    break;
                case gtirb::ISAID::ARM:
                    /// \todo getThunkTargetFunc = &s_arm_get_thunk_target;
                    break;
                case gtirb::ISAID::PPC32:
                    /// \todo getThunkTargetFunc = &s_ppc_get_thunk_target;
                    break;
                default:
                    throw std::out_of_range("The ISA ID was invalid.");
            }

            // Collect thunk targets.
            for(auto cfg : cfgSet->getCFGs())
            {
                if(gtirb::utilities::IsAnyFlagSet(cfg->getFlags(),
                                                  CFG::Flags::IS_ITHUNK | CFG::Flags::IS_DTHUNK))
                {
                    const auto indirectTarget = getThunkTargetFunc(module, cfg.get());

                    if(indirectTarget.second != nullptr)
                    {
                        // Simple sanity: this assert has been useful for noticing when we
                        // screw up thunk renaming, for example.
                        Expects(cfg->getProcedureName() != indirectTarget.second->getName());
                        thunks.insert(cfg.get());
                    }
                }
            }
        }
    }

    return thunks;
}
