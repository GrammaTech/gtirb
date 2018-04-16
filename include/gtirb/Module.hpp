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

        void setBinaryPath(boost::filesystem::path x);
        boost::filesystem::path getBinaryPath() const;

        void setFileFormat(gtirb::FileFormat x);
        gtirb::FileFormat getFileFormat() const;

        void setRebaseDelta(int64_t x);
        int64_t getRebaseDelta() const;

        ///
        /// If an invalid pair is passed in, the min and max will be set to an invalid state (gtirb::constants::BadAddress).
        ///
        /// \return     False if the pair's first is > the pair's second.
        ///
        bool setEAMinMax(std::pair<gtirb::EA, gtirb::EA> x);
        std::pair<gtirb::EA, gtirb::EA> getEAMinMax() const;

        void setPreferredEA(gtirb::EA x);
        gtirb::EA getPreferredEA() const;
    protected:

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
