#pragma once

#include <boost/filesystem.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Enums.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/NodeReference.hpp>
#include <gtirb/ProcedureSet.hpp>
#include <gtirb/SymbolSet.hpp>
#include <gtirb/SymbolicOperandSet.hpp>

namespace proto
{
    class Module;
}

namespace gtirb
{
    class AddrRanges;
    class Data;
    class ImageByteMap;
    class Block;
    using BlockSet = std::vector<Block>;
    class Data;
    using DataSet = std::vector<Data>;
    class Relocation;
    using RelocationSet = std::vector<Relocation>;
    class Section;
    using SectionSet = std::vector<Section>;

    ///
    /// \class Module
    /// \author John E. Farrier
    ///
    /// \todo Replace boost::filesystem with std::filesystem.
    ///
    class GTIRB_GTIRB_EXPORT_API Module : public Node
    {
    public:
        ///
        /// Default constructor.
        ///
        Module();

        ///
        /// Trivial virtual destructor.
        ///
        ~Module() override;

        ///
        /// Set the location of the corresponding binary on disk.
        ///
        /// \param  x   A path to the corresponding binary on disk.
        ///
        void setBinaryPath(boost::filesystem::path x);

        ///
        /// Get the location of the corresponding binary on disk.
        ///
        /// \return   The path to the corresponding binary on disk.
        ///
        boost::filesystem::path getBinaryPath() const;

        ///
        /// Sets the format of the binary pointed to by getBinaryPath().
        ///
        /// \param  x   The gtirb::FileFormat enumeration corresponding to the binary associated
        /// with this Module.
        ///
        void setFileFormat(gtirb::FileFormat x);

        ///
        /// Gets the format of the binary pointed to by getBinaryPath().
        ///
        /// \return     The gtirb::FileFormat enumeration corresponding to the binary associated
        /// with this Module.
        ///
        gtirb::FileFormat getFileFormat() const;

        ///
        ///
        ///
        void setRebaseDelta(int64_t x);

        ///
        ///
        ///
        int64_t getRebaseDelta() const;

        ///
        /// If an invalid pair is passed in, the min and max will be set to an invalid state
        /// (gtirb::constants::BadAddress).
        ///
        /// \param      x   The minimum and maximum effective address (EA) for this Module.
        /// \return     False if the pair's first is > the pair's second.
        ///
        bool setEAMinMax(std::pair<gtirb::EA, gtirb::EA> x);

        ///
        /// Gets the minimum and maximum effective address (EA) for this Module.
        ///
        /// Check return values for gtirb::constants::BadAddress.
        ///
        /// \return     The minimum and maximum effective address (EA) for this Module.
        ///
        std::pair<gtirb::EA, gtirb::EA> getEAMinMax() const;

        ///
        ///
        ///
        void setPreferredEA(gtirb::EA x);

        ///
        ///
        ///
        void setISAID(gtirb::ISAID x);

        ///
        ///
        ///
        gtirb::ISAID getISAID() const;

        ///
        ///
        ///
        gtirb::EA getPreferredEA() const;

        ///
        /// A Module can have exactly one AddrRanges child.
        ///
        gtirb::AddrRanges& getAddrRanges();
        const gtirb::AddrRanges& getAddrRanges() const;

        ///
        /// A Module can have exactly one ImageByteMap child.
        ///
        gtirb::ImageByteMap& getImageByteMap();
        const gtirb::ImageByteMap& getImageByteMap() const;

        ///
        /// A Module can have exactly one ProcedureSet child.
        ///
        gtirb::ProcedureSet& getProcedureSet();
        const gtirb::ProcedureSet& getProcedureSet() const;

        ///
        /// A Module can have exactly one SymbolSet child.
        ///
        gtirb::SymbolSet& getSymbolSet();
        const gtirb::SymbolSet& getSymbolSet() const;

        bool getIsSetupComplete() const;
        bool getIsReadOnly() const;

        void setName(std::string x);
        std::string getName() const;

        void setDecodeMode(uint64_t x);
        uint64_t getDecodeMode() const;

        // Note: all of these will probably move and/or get a different interface.
        const std::vector<Block>& getBlocks() const;
        void setBlocks(std::vector<Block> x);
        const std::vector<Relocation>& getRelocations() const;
        void setRelocations(std::vector<Relocation> x);
        const std::vector<Data>& getData() const;
        std::vector<Data>& getData();
        std::vector<Section>& getSections();
        const std::vector<Section>& getSections() const;
        SymbolicOperandSet& getSymbolicOperands();
        const SymbolicOperandSet& getSymbolicOperands() const;

        using MessageType = proto::Module;
        void toProtobuf(MessageType* message) const;
        void fromProtobuf(const MessageType& message);

    protected:
        ///
        /// Sets the internal "isSetupComplete" flag to true.
        /// Once this is set to "true", it cannot be set back to false.
        ///
        void setIsSetupComplete();

        ///
        /// Sets the state of the section's "isReadOnly" flag.
        ///
        void setIsReadOnly(bool x);

    private:
        boost::filesystem::path binaryPath{};
        std::pair<gtirb::EA, gtirb::EA> eaMinMax{};
        gtirb::EA preferredEA{};
        int64_t rebaseDelta{0};
        gtirb::FileFormat fileFormat{};
        gtirb::ISAID isaID{};
        bool isSetupComplete{false};
        bool isReadOnly{false};
        std::string name{};
        uint64_t decodeMode{0};
        std::unique_ptr<AddrRanges> addrRanges;
        std::unique_ptr<ImageByteMap> imageByteMap;
        std::unique_ptr<ProcedureSet> procedureSet;
        std::unique_ptr<SymbolSet> symbolSet;
        std::unique_ptr<BlockSet> blocks;
        std::unique_ptr<RelocationSet> relocations;
        std::unique_ptr<DataSet> data;
        std::unique_ptr<SectionSet> sections;
        std::unique_ptr<SymbolicOperandSet> symbolicOperands;
    };
} // namespace gtirb
