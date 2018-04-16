#pragma once

#include <gtirb/Node.hpp>
#include <gtirb/Enums.hpp>
#include <gtirb/EA.hpp>
#include <boost/filesystem.hpp>

namespace gtirb
{
    class ModuleSummary;
    class ModuleCore;
    class ModuleAux;

    ///
    /// \class Module
    /// \author John E. Farrier
    ///
    /// Upon construction, a Module will automatically build default ModuleSummary, ModuleCore, and ModuleAux children.
    ///
    /// \todo Replace boost::filesystem with std::filesystem.
    ///
    class GTIRB_GTIRB_EXPORT_API Module : public Node
    {
    public:
        virtual ~Module() = default;

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
        /// \param  x   The gtirb::FileFormat enumeration corresponding to the binary associated with this Module.
        ///
        void setFileFormat(gtirb::FileFormat x);

        ///
        /// Gets the format of the binary pointed to by getBinaryPath().
        /// 
        /// \return     The gtirb::FileFormat enumeration corresponding to the binary associated with this Module.
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
        /// If an invalid pair is passed in, the min and max will be set to an invalid state (gtirb::constants::BadAddress).
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
        gtirb::EA getPreferredEA() const;

    private:
        boost::filesystem::path binaryPath{};
        std::pair<gtirb::EA, gtirb::EA> eaMinMax{};
        gtirb::EA preferredEA{};
        int64_t rebaseDelta{0};
        gtirb::FileFormat fileFormat{};
    };

    GTIRB_GTIRB_EXPORT_API ModuleSummary* GetOrCreateModuleSummary(Module* const x);
    GTIRB_GTIRB_EXPORT_API ModuleCore* GetOrCreateModuleCore(Module* const x);
    GTIRB_GTIRB_EXPORT_API ModuleAux* GetOrCreateModuleAux(Module* const x);
}
