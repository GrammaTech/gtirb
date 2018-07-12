#pragma once

#include <cstdint>
#include <gtirb/Node.hpp>
#include <memory>
#include <vector>

namespace gtirb
{
    class CFGNode;

    ///
    /// \class CFG
    /// \author John E. Farrier
    ///
    /// A child of a CFGSet.
    ///
    /// \todo Remove.  Eventually we will only have the ICFG which
    /// will be a graph of basic blocks and edges between basic
    /// blocks.
    ///
    class GTIRB_GTIRB_EXPORT_API CFG : public Node
    {
    public:
        enum Flags
        {
            // CFG is a clone, or has cloned instructions.
            IS_CLONE = 0x00000001,
            // CFG is a runtime library model
            IS_RTL_MODEL = 0x00000002,
            // CFG is a vararg function (not used yet!)
            IS_VARARG = 0x00000004,
            // CFG is an indirect thunk
            IS_ITHUNK = 0x00000008,
            // CFG is a direct thunk
            IS_DTHUNK = 0x00000010,
            // CFG is an entry function (either program entry,
            // or the entry of a DLL pulled in by the program).
            IS_ENTRY = 0x00000020,
            // CFG is synthesized (not in the original program)
            IS_SYNTHESIZED = 0x00000040,
            // CFG is a summarized (reduced) library function.
            IS_SUMMARY = 0x00000080,
            // CFG sets up a stack frame.
            HAS_FRAME = 0x00000100,
            // CFG is a printf/scanf stub
            IS_STUB = 0x00000200,
            // CFG has been setup
            IR_SETUP_DONE = 0x00000400,
            // CFG must be re-initialized
            MUST_REINIT = 0x00000800,
            // CFG is exported by the module in which it's defined
            // (e.g. via __declspec(dllexport))
            IS_EXPORTED = 0x00001000,
            // CFG is the "Supermain" procedure
            IS_SUPERMAIN = 0x00002000,
            // This CFG has SP-delta inconsistencies
            BAD_SP_DELTAS = 0x00004000,
            // This CFG is orphan
            IS_ORPHAN = 0x00008000,
            // This CFG may be unsound or non-conforming; i.e., there may be possible
            // control flow that is unrepresented (or under-represented, e.g., due to
            // faulty disassembly, or otherwise doesn't fit the "usual" expected
            // call-return model.)
            // Currently set for:
            //  - multi-entry constructs, where an instruction in one procedure flows into
            //    another;
            //  - unresolved indirect calls.
            UNSOUND_REP = 0x00010000,
            // This cfg has exception handlers. These appear in the cfg as nodes which
            // have no executable predecessors (as we have no way to represent these
            // edges in our cfgs)
            HAS_EXN_HANDLERS = 0x00020000,

            // Calling convention mask
            CC_MASK = 0xFF000000,
            // Calling convention is cdecl
            CC_CDECL = 0x01000000,
            // Calling convention is stdcall
            CC_STDCALL = 0x02000000,
            // Calling convention is fastcall
            CC_FASTCALL = 0x04000000,
            // Calling convention is Pascal
            CC_PASCAL = 0x08000000,
            // Calling convention is thiscall
            CC_THISCALL = 0x10000000,
            // Calling convention is cdecl+ellipsis
            CC_ELLIPSIS = 0x20000000,
            // Calling convention is unknown/manual/special
            CC_SPECIAL = 0x40000000
        };

        ///
        /// Default Constructor.
        ///
        CFG() = default;

        ///
        /// Defaulted trivial destructor.
        ///
        ~CFG() override;

        ///
        ///
        ///
        void setEA(EA x);

        ///
        ///
        ///
        EA getEA() const;

        ///
        ///
        ///
        void setProcedureName(std::string x);

        ///
        ///
        ///
        std::string getProcedureName() const;

        ///
        ///
        ///
        void setFlags(uint64_t x);

        ///
        ///
        ///
        uint64_t getFlags() const;

        ///
        /// CFG nodes owned by this CFG.
        ///
        const std::vector<std::shared_ptr<CFGNode>>& getNodes() const;

    private:
        std::string procedureName{};
        EA ea{};
        uint64_t flags{0};
        std::vector<std::shared_ptr<CFGNode>> nodes;
    };
}
