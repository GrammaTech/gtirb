#pragma once

#include <gtirb/Export.hpp>
#include <gtirb/Node.hpp>
#include <vector>

namespace gtirb
{
    class Instruction;

    ///
    /// \class Block
    /// \author Nathan Weston
    ///
    /// A basic block. This will eventually be part of the ICFG.
    ///
    class GTIRB_GTIRB_EXPORT_API Block : public Node
    {
    public:
        // Default constructor required by boost::serialize
        Block();

        ///
        /// Construct an empty block
        ///
        Block(EA startingAddress, EA endingAddress);

        ///
        /// Construct a block with some instructions.
        /// Instructions are copied and added as children.
        ///
        Block(EA startingAddress, EA endingAddress, std::vector<Instruction>& instructions);

        ///
        /// Defaulted trivial destructor.
        ///
        ~Block() override = default;

        EA getStartingAddress() const;
        EA getEndingAddress() const;

        std::vector<const Instruction*> getInstructions() const;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar& boost::serialization::base_object<Node>(*this);
            ar & this->startingAddress;
            ar & this->endingAddress;
        }

    private:
        EA startingAddress{};
        EA endingAddress{};
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::Block);
