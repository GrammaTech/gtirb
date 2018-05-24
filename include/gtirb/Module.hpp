#pragma once

#include <boost/filesystem.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Enums.hpp>
#include <gtirb/FilesystemSerialization.hpp>
#include <gtirb/Node.hpp>

namespace gtirb
{
    class AddrRanges;
    class CFGSet;
    class ImageByteMap;
    class ModuleAux;
    class ModuleCore;
    class ModuleSummary;
    class SymbolSet;
    class SectionTable;
    class ProcedureSet;

    ///
    /// \class Module
    /// \author John E. Farrier
    ///
    /// Upon construction, a Module will automatically build default ModuleSummary, ModuleCore, and
    /// ModuleAux children.
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
        ~Module() override = default;

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
        gtirb::AddrRanges* getAddrRanges();

        ///
        ///
        ///
        CFGSet* getCFGSet();
        const CFGSet* getCFGSet() const;

        ///
        /// A Module can have exactly one ImageByteMap child.
        ///
        gtirb::ImageByteMap* getImageByteMap();

        ///
        /// A Module can have exactly one ProcedureSet child.
        ///
        gtirb::ProcedureSet* getProcedureSet();

        ///
        /// A Module can have exactly one SymbolSet child.
        ///
        gtirb::SymbolSet* getSymbolSet();

        ///
        /// A Module can have exactly one section table.
        ///
        SectionTable& getOrCreateSectionTable();

        bool getIsSetupComplete() const;
        bool getIsReadOnly() const;

        void setName(std::string x);
        std::string getName() const;

        void setDecodeMode(uint64_t x);
        uint64_t getDecodeMode() const;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);

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
        std::shared_ptr<AddrRanges> addrRanges;
        std::shared_ptr<CFGSet> cfgSet;
        std::shared_ptr<ImageByteMap> imageByteMap;
        std::shared_ptr<ProcedureSet> procedureSet;
        std::shared_ptr<SymbolSet> symbolSet;
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::Module);
