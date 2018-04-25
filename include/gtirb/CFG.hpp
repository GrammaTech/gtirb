#pragma once

#include <cstdint>
#include <gtirb/Node.hpp>

namespace gtirb
{
    class EA;

    ///
    /// \class CFG
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API CFG : public Node
    {
    public:
        ///
        /// Default Constructor.
        ///
        CFG();

        ///
        /// Defaulted trivial destructor.
        ///
        virtual ~CFG() = default;

        //Procedure* getOrCreateProcedure();
        
        //CFGAttribute* getOrCreateCFGAttribute();

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int)
        {
            ar& boost::serialization::base_object<Node>(*this);
        }

    private:
        //std::weak_ptr<CFGNode> first;
        //std::weak_ptr<CFGNode> last;
        //SRPSet_T srpSet;
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::CFG);
