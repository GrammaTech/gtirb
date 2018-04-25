#pragma once

#include <boost/filesystem.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Enums.hpp>
#include <gtirb/Node.hpp>

namespace gtirb
{
    ///
    /// \class ModuleSectionBase
    /// \author John E. Farrier
    ///
    /// A base class for ModuleSummary, ModuleCore, and ModuleAux.
    ///
    class GTIRB_GTIRB_EXPORT_API ModuleSectionBase : public Node
    {
    public:
        ModuleSectionBase();
        virtual ~ModuleSectionBase() = default;

        bool getIsSetupComplete() const;
        bool getIsReadOnly() const;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int)
        {
            ar& boost::serialization::base_object<Node>(*this);
            ar& isSetupComplete;
            ar& isReadOnly;
        }

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
        bool isSetupComplete{false};
        bool isReadOnly{false};
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::ModuleSectionBase);
