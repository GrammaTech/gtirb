#pragma once

///
/// \define GTIRB_USE_ANY_IMPL_HEADER
///
/// The user can toggle between boost and std implementation of "any". This is not a feature, but a
/// work-around for older compiler support.  Once all compilers support std::any, this should go
/// away. GTIRB_USE_ANY_IMPL_HEADER is defined by the
/// build system (CMake or SCons) to either be
/// <boost/any.hpp> or <any>.
///
#include GTIRB_USE_ANY_IMPL_HEADER

namespace gtirb
{
    ///
    /// \define GTIRB_USE_ANY_IMPL_NAMESPACE
    ///
    /// GTIRB_USE_ANY_IMPL_NAMESPACE is defined by the build system (CMake or Scons) to either be
    /// "boost" or "std".
    ///

    ///
    /// \typedef gtirb::any
    ///
    /// The user can toggle between boost and std implementation of "any".
    /// This is not a feature, but a work-around for older compiler support.
    /// Once all compilers support std::any, this should go away.
    ///
    typedef GTIRB_USE_ANY_IMPL_NAMESPACE::any any;
}
