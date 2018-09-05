#ifndef GTIRB_EXPORT_H
#define GTIRB_EXPORT_H

// DOXFIXME[What does this comment belong to? At the moment it's being
// attached to namespace gtirb which seems definitely wrong]
///
/// Defined by the build system (CMake or SCons).
/// This should only be defined by the build system which generates the GT-IRB
/// library.  Users of the library should NOT define this.
///

// DOXFIXME[What does this comment belong to? At the moment it's being
// attached to namespace gtirb which seems definitely wrong]
///
/// This controls the visibility of exported symbols (i.e. classes) in Windows
/// DLL's and Linux Shared Objects.
///

///
/// \namespace gtirb
///
/// The namespace for the GT-IRB library.
///

#ifdef _WIN32
#if defined GTIRB_gtirb_EXPORTS
#define GTIRB_EXPORT_API _declspec(dllexport)
#else
#define GTIRB_EXPORT_API _declspec(dllimport)
#endif
#else
#define GTIRB_EXPORT_API __attribute__((visibility("default")))
#endif

#endif // GTIRB_EXPORT_H
