#pragma once

#include <gtirb/EA.hpp>

///
/// \define GTIRB_USE_VARIANT_IMPL_HEADER
///
/// The user can toggle between boost and std implementation of "variant". This is not a feature,
/// but a
/// work-around for older compiler support.  Once all compilers support std::variant, this should go
/// away. GTIRB_USE_VARIANT_IMPL_HEADER is defined by the
/// build system (CMake or SCons) to either be
/// <boost/variant.hpp> or <variant>.
///
#include GTIRB_USE_VARIANT_IMPL_HEADER

namespace gtirb
{
    ///
    /// \define GTIRB_USE_VARIANT_IMPL_NAMESPACE
    ///
    /// GTIRB_USE_VARIANT_IMPL_NAMESPACE is defined by the build system (CMake or Scons) to either
    /// be
    /// "boost" or "std".
    ///

    ///
    /// \typedef gtirb::variant
    ///
    /// The user can toggle between boost and std implementation of "variant".
    /// This is not a feature, but a work-around for older compiler support.
    /// Once all compilers support std::variant, this should go away.
    ///
    typedef GTIRB_USE_VARIANT_IMPL_NAMESPACE::variant<uint8_t, uint16_t, uint32_t, uint64_t, int8_t,
                                                      int16_t, int32_t, int64_t, double,
                                                      std::string, gtirb::EA>
        variant;
}
