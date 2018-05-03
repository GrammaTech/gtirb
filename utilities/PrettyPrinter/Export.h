#pragma once

///
/// \define GTIRB_PrettyPrinter_EXPORTS
///
/// Defined by the build system (CMake or SCons).
/// This should only be defined by the build system which generates the GT-IRB library.  Users of
/// the library should NOT define this.
///

///
/// \define GTIRB_PRETTYPRINTER_EXPORT_API
///
/// This controls the visibility of exported symbols (i.e. classes) in Windows DLL's and Linux
/// Shared Objects.
///

#ifdef WIN32
#if defined GTIRB_gtirbPrettyPrinter_EXPORTS
#define GTIRB_PRETTYPRINTER_EXPORT_API _declspec(dllexport)
#else
#define GTIRB_PRETTYPRINTER_EXPORT_API _declspec(dllimport)
#endif
#else
#define GTIRB_PRETTYPRINTER_EXPORT_API __attribute__((visibility("default")))
#endif
