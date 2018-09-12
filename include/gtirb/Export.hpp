#ifndef GTIRB_EXPORT_H
#define GTIRB_EXPORT_H

#ifndef __has_declspec_attribute
#define __has_declspec_attribute(x) 0
#endif

#ifndef __has_attribute
#define __has_attribute(x) 0
#endif

/// \def GTIRB_EXPORT_API
/// \brief This macro controls the visibility of exported symbols (i.e. classes)
/// in shared libraries. When producing the library, selects symbols to export,
/// and when consuming the library, selects symbols to import.
#if defined(_MSC_VER) || __has_declspec_attribute(dllexport)
// Defined by the build system (CMake or SCons). This should only be defined by
// the build system which generates the GT-IRB library. Users of the library
// should NOT define this.
#ifdef GTIRB_gtirb_EXPORTS
#define GTIRB_EXPORT_API _declspec(dllexport)
#else
#define GTIRB_EXPORT_API _declspec(dllimport)
#endif // GTIRB_gtirb_EXPORTS
#elif defined(__GNUC__) || __has_attribute(visibility)
#define GTIRB_EXPORT_API __attribute__((visibility("default")))
#else
#define GTIRB_EXPORT_API
#endif

#endif // GTIRB_EXPORT_H
