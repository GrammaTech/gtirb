#
# Cmake Configuration
#

# Need 3.9 to support CXX_STANDARD=17 and protobuf
CMAKE_MINIMUM_REQUIRED(VERSION 3.9.0)

# The version.txt file is the official record of the version number. We use the
# contents of that file to set the project version for use in other CMake files.
file(READ "${CMAKE_CURRENT_SOURCE_DIR}/version.txt" ver)

string(REGEX MATCH "VERSION_MAJOR ([0-9]*)" _ ${ver})
set(GTIRB_MAJOR_VERSION ${CMAKE_MATCH_1})

string(REGEX MATCH "VERSION_MINOR ([0-9]*)" _ ${ver})
set(GTIRB_MINOR_VERSION ${CMAKE_MATCH_1})

string(REGEX MATCH "VERSION_PATCH ([0-9]*)" _ ${ver})
set(GTIRB_PATCH_VERSION ${CMAKE_MATCH_1})

cmake_policy(SET CMP0048 NEW)
project(GTIRB VERSION
        "${GTIRB_MAJOR_VERSION}.${GTIRB_MINOR_VERSION}.${GTIRB_PATCH_VERSION}")

include(CheckFunctionExists)
include(CheckCXXSourceCompiles)
include(CheckIncludeFile)
include(Macros.cmake)
include(AlignOf.cmake)
include(CMakePackageConfigHelpers)

# ---------------------------------------------------------------------------
# Build options
# ---------------------------------------------------------------------------

option(GTIRB_ENABLE_TESTS "Enable building and running unit tests." ON)

# This just sets the builtin BUILD_SHARED_LIBS, but if defaults to ON
# instead of OFF.
option(GTIRB_BUILD_SHARED_LIBS "Build shared libraries." ON)
if(GTIRB_BUILD_SHARED_LIBS)
  set(BUILD_SHARED_LIBS ON)
else()
  set(BUILD_SHARED_LIBS OFF)
endif()
if(UNIX AND NOT BUILD_SHARED_LIBS)
    # Find only static libraries
    set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
    add_compile_options(-static)
endif()

enable_testing()

# Whether or not to run clang-tidy (if present)
option(GTIRB_RUN_CLANG_TIDY "Enable running of clang-tidy." ON)

# ---------------------------------------------------------------------------
# Global settings
# ---------------------------------------------------------------------------

set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/src)
if (WIN32)
  set(CMAKE_DEBUG_POSTFIX          "d" CACHE STRING "add a postfix, usually d on windows")
endif()
set(CMAKE_RELEASE_POSTFIX        ""  CACHE STRING "add a postfix, usually empty on windows")
set(CMAKE_RELWITHDEBINFO_POSTFIX ""  CACHE STRING "add a postfix, usually empty on windows")
set(CMAKE_MINSIZEREL_POSTFIX     ""  CACHE STRING "add a postfix, usually empty on windows")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_CXX_EXTENSIONS OFF)

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/include/gtirb/version.h.in"
  "${CMAKE_CURRENT_BINARY_DIR}/include/gtirb/version.h"
  @ONLY)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/include/gtirb/version.h"
        DESTINATION include/gtirb)

# Use C++17
set(CMAKE_CXX_STANDARD 17)
# Error if it's not available
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Specifically check for gcc-7 or later. gcc-5 is installed on many
# systems and will accept -std=c++17, but does not fully support the
# standard.
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS "7.0.0")
    message(FATAL_ERROR "gcc 7 or later is required to build gtirb")
  endif()
endif()

set(CMAKE_CXX_VISIBILITY_PRESET hidden)

# Base include path for the GTIRB library.
include_directories(${CMAKE_SOURCE_DIR}/include)
#
# Global Options (Compile / Link)
#
add_compile_options(-DBOOST_MULTI_INDEX_DISABLE_SERIALIZATION)
if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
        add_compile_options(-D_CRT_SECURE_NO_WARNINGS)
        add_compile_options(-D_MBCS)
        add_compile_options(-D_SCL_SECURE_NO_WARNINGS)
        add_compile_options(-D_WIN32)
        add_compile_options(-D_WINDOWS)
        add_compile_options(-D_WINSOCKAPI_)
        add_compile_options(-DMBCS)
        add_compile_options(-DNOMINMAX)
        add_compile_options(-EHsc)
        add_compile_options(-GR)
        add_compile_options(-MP)
        add_compile_options(-nologo)
        add_compile_options(-W4)
        add_compile_options(-WX)

        add_compile_options(-wd4251) # 'identifier' : class 'type' needs to have dll-interface to be used by clients of class 'type2'
        add_compile_options(-wd4275) # Non-dll interface base classes.
        add_compile_options(-wd4996) # VC8: Deprecated libc functions.
        add_compile_options(-wd4351) # This is a warning about a change in behavior from old versions of visual c++.  We want the new (standard-compliant) behavior, so we don't want the warning.  The warning is that using an array in a class initializer list will cause its elements to be default initialized.
        add_compile_options(-wd4146) # unary minus operator applied to unsigned type, result still unsigned
        add_compile_options(-wd4127) # conditional expression is constant (protobuf generates this diagnostic).

        # Per-configuration options
        add_compile_options($<$<CONFIG:Debug>:-D_DEBUG>)
        add_compile_options($<$<CONFIG:Debug>:-DDEBUG>)
        add_compile_options($<$<CONFIG:Debug>:-MDd>)
        add_compile_options($<$<CONFIG:Debug>:-Ob0>) # Disables inline expansion
        add_compile_options($<$<CONFIG:Debug>:-Od>) # Disables optimization, speeding compilation and simplifying debugging. https://msdn.microsoft.com/en-us/library/k1ack8f1.aspx
        add_compile_options($<$<CONFIG:Debug>:-RTC1>) # Enables run-time error checking.
        add_compile_options($<$<CONFIG:Debug>:-W4>) # Sets warning level.
        add_compile_options($<$<CONFIG:Debug>:-Zi>) # Generates complete debugging information.
        add_compile_options($<$<CONFIG:Debug>:-bigobj>) # enables big obj compilation option

        add_compile_options($<$<CONFIG:RelWithDebInfo>:-D_NDEBUG>)
        add_compile_options($<$<CONFIG:RelWithDebInfo>:-DNDEBUG>)
        add_compile_options($<$<CONFIG:RelWithDebInfo>:-MD>)
        add_compile_options($<$<CONFIG:RelWithDebInfo>:-O2>) # Creates fast code.
        add_compile_options($<$<CONFIG:RelWithDebInfo>:-Ob2>) # The default value. Allows expansion of functions marked as inline, __inline, or __forceinline, and any other function that the compiler chooses.
        add_compile_options($<$<CONFIG:RelWithDebInfo>:-Oi>) # Generates intrinsic functions.
        add_compile_options($<$<CONFIG:RelWithDebInfo>:-Ot>) # Favors fast code.
        add_compile_options($<$<CONFIG:RelWithDebInfo>:-W4>) # Sets warning level.
        add_compile_options($<$<CONFIG:RelWithDebInfo>:-Zi>) # Generates complete debugging information.
        add_compile_options($<$<CONFIG:RelWithDebInfo>:-bigobj>) # enables big obj compilation option

        add_compile_options($<$<CONFIG:Release>:-D_NDEBUG>)
        add_compile_options($<$<CONFIG:Release>:-DNDEBUG>)
        add_compile_options($<$<CONFIG:Release>:-MD>)
        add_compile_options($<$<CONFIG:Release>:-O2>) # Creates fast code.
        add_compile_options($<$<CONFIG:Release>:-Ob2>) # The default value. Allows expansion of functions marked as inline, __inline, or __forceinline, and any other function that the compiler chooses.
        add_compile_options($<$<CONFIG:Release>:-Oi>) # Generates intrinsic functions.
        add_compile_options($<$<CONFIG:Release>:-Ot>) # Favors fast code.
        add_compile_options($<$<CONFIG:Release>:-W4>) # Sets warning level.
        add_compile_options($<$<CONFIG:Release>:-bigobj>) # enables big obj compilation option

        # Shove in some linker flags to support using 64-bit memory.
        # 4099 -PDB 'filename' was not found with 'object/library' or at 'path'; linking object as if no debug info
        set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /LARGEADDRESSAWARE")

elseif((${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
       OR (${CMAKE_CXX_COMPILER_ID} STREQUAL Clang))
    add_compile_options(-Wall -Wextra -Wpointer-arith -Wshadow -Werror)
    add_compile_options(-fPIC)
endif()

# ---------------------------------------------------------------------------
# Boost
# ---------------------------------------------------------------------------
find_package(Boost 1.67 REQUIRED)

add_compile_options(-DBOOST_CONFIG_SUPPRESS_OUTDATED_MESSAGE)
add_compile_options(-DBOOST_SYSTEM_NO_DEPRECATED)
include_directories(SYSTEM ${Boost_INCLUDE_DIRS})

# ---------------------------------------------------------------------------
# Google Test Application
# ---------------------------------------------------------------------------
if(GTIRB_ENABLE_TESTS)
        # Pull in Google Test
        # https://github.com/google/googletest/tree/master/googletest#incorporating-into-an-existing-cmake-project

        # Download and unpack googletest at configure time
        configure_file(CMakeLists.googletest googletest-download/CMakeLists.txt)

        execute_process(COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}" .
                RESULT_VARIABLE result
                WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/googletest-download" )

        if(result)
                message(WARNING "CMake step for googletest failed: ${result}")
        endif()

        execute_process(COMMAND "${CMAKE_COMMAND}" --build .
                RESULT_VARIABLE result
                WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/googletest-download" )

        if(result)
                message(WARNING "Build step for googletest failed: ${result}")
        endif()

        # Prevent overriding the parent project's compiler/linker
        # settings on Windows
        set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

        # Add googletest directly to our build. This defines
        # the gtest and gtest_main targets.
        add_subdirectory("${CMAKE_BINARY_DIR}/googletest-src"
                                         "${CMAKE_BINARY_DIR}/googletest-build"
                                         EXCLUDE_FROM_ALL)

        include_directories("${gtest_SOURCE_DIR}/include")
endif()

# ---------------------------------------------------------------------------
# protobuf
# ---------------------------------------------------------------------------
find_package(Protobuf 3.0.0 REQUIRED)

if(NOT BUILD_SHARED_LIBS)
  set(Protobuf_USE_STATIC_LIBS ON)
endif()
include_directories(SYSTEM ${PROTOBUF_INCLUDE_DIRS})


# ---------------------------------------------------------------------------
# gtirb sources
# ---------------------------------------------------------------------------
add_subdirectory(src)
add_subdirectory(doc/examples)
add_subdirectory(python)


# ---------------------------------------------------------------------------
# Export config for use by other CMake projects
# ---------------------------------------------------------------------------

# --- For direct use from the build directory/cmake registry ---
# This exports the targets
export(TARGETS gtirb proto FILE "${CMAKE_CURRENT_BINARY_DIR}/gtirbTargets.cmake")
# This is the main config file that find_package will look for.
file(
  WRITE "${CMAKE_CURRENT_BINARY_DIR}/gtirbConfig.cmake"
        "
            include(\"\$\{CMAKE_CURRENT_LIST_DIR\}/gtirbTargets.cmake\")
            set_property(
                TARGET gtirb
                APPEND PROPERTY
                    INTERFACE_INCLUDE_DIRECTORIES \"${CMAKE_BINARY_DIR}/include\" ${PROTOBUF_INCLUDE_DIRS}
            )
        ")
# Add the build directory to the user CMake registry, so find_package
# can locate it automatically.
export(PACKAGE gtirb)


# --- For the installed copy ---
# Main config file for find_package, just includes the targets file.
file(
  WRITE "${CMAKE_CURRENT_BINARY_DIR}/export/gtirbConfig.cmake"
        "include(\"\$\{CMAKE_CURRENT_LIST_DIR\}/gtirbTargets.cmake\")"
    )
# In this mode, find_package also seems to require a version file
set(version_file "${CMAKE_CURRENT_BINARY_DIR}/gtirbConfig-version.cmake")
write_basic_package_version_file(${version_file}
                                 VERSION ${GTIRB_VERSION}
                                 COMPATIBILITY AnyNewerVersion)

# Copy the config files to the install location
install(FILES
  ${CMAKE_CURRENT_BINARY_DIR}/export/gtirbConfig.cmake
  ${version_file}
  DESTINATION lib/gtirb)
# This exports the targets to the install location.
install(EXPORT gtirbTargets DESTINATION lib/gtirb)
