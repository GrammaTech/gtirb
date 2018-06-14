#include <boost/serialization/optional.hpp>
#include <gtirb/Block.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/RuntimeError.hpp>

using namespace gtirb;
class Block;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Instruction);

Instruction::Instruction(EA ea) : Node(), ea(ea)
{
}

void Instruction::setEA(gtirb::EA x)
{
    this->ea = x;
}

gtirb::EA Instruction::getEA() const
{
    return this->ea;
}

void Instruction::setIsFallthrough(bool x)
{
    this->isFallthrough = x;
}

bool Instruction::getIsFallthrough() const
{
    return this->isFallthrough;
}

void Instruction::setIsPEI(bool x)
{
    this->isPEI = x;
}

bool Instruction::getIsPEI() const
{
    return this->isPEI;
}

void Instruction::setNumberOfUses(int64_t x)
{
    this->numberOfUses = x;
}

int64_t Instruction::getNumberOfUses() const
{
    return this->numberOfUses;
}

std::vector<Instruction::SymbolicOperand>& Instruction::getSymbolicOperands()
{
    return this->symbolicOperands;
}

const std::vector<Instruction::SymbolicOperand>& Instruction::getSymbolicOperands() const
{
    return this->symbolicOperands;
}

template <class Archive>
void Instruction::MovedLabel::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar & this->offset1;
    ar & this->offset2;
}

template <class Archive>
void Instruction::SymbolicOperand::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar & this->kind;
    ar & this->pltReferenceName;
    ar & this->directCallDestination;
    ar & this->movedLabel;
}

template <class Archive>
void Instruction::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar& boost::serialization::base_object<Node>(*this);
    ar& ea;
    ar& numberOfUses;
    ar& isFallthrough;
    ar& isPEI;
    ar & this->symbolicOperands;
}
