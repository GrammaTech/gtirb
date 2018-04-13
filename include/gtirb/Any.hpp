#pragma once

// The user can toggle between boost and std implementaiton of "any".
// This is not a feature, but a work-around for older compiler support.
// Once all compilers support std::any, this should go away.
#include GTIRB_USE_ANY_IMPL_HEADER

namespace gtirb
{
    // The user can toggle between boost and std implementaiton of "any".
    // This is not a feature, but a work-around for older compiler support.
    // Once all compilers support std::any, this should go away.
    typedef GTIRB_USE_ANY_IMPL_NAMESPACE::any any;
}
