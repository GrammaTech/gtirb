#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <set>

namespace gtirb
{
    ///
    /// \class Instruction
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API Instruction : public Node
    {
    public:
        ///
        /// Default Constructor.
        ///
        Instruction();

        ///
        /// Defaulted trivial destructor.
        ///
        virtual ~Instruction() = default;

        void setEA(gtirb::EA x);
        gtirb::EA getEA() const;

        ///
        /// From WALA.
        /// https://github.com/wala/WALA/wiki/Intermediate-Representation-(IR)
        ///
        void setIsFallthrough(bool x);
        bool getIsFallthrough() const;

        ///
        /// From WALA.
        /// Can this instruction thrown an exception?
        /// https://github.com/wala/WALA/wiki/Intermediate-Representation-(IR)
        ///
        void setIsPEI(bool x);
        bool getIsPEI() const;

        void setNumberOfUses(int64_t x);
        int64_t getNumberOfUses() const;

        template <class Archive>
        void serialize(Archive& ar, const unsigned int)
        {
            ar& boost::serialization::base_object<Node>(*this);
            ar& ea;
            ar& numberOfUses;
            ar& isFallthrough;
            ar& isPEI;
        }

    private:
        gtirb::EA ea;
        int64_t numberOfUses{0};
        bool isFallthrough{false};
        bool isPEI{false};
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::Instruction);
