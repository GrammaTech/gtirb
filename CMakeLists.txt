#
# Cmake Configuration
#

# Need 3.9 to support CXX_STANDARD=17 and protobuf
cmake_minimum_required(VERSION 3.9.0)

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
project(
  GTIRB
  VERSION "${GTIRB_MAJOR_VERSION}.${GTIRB_MINOR_VERSION}.${GTIRB_PATCH_VERSION}"
)

include(CheckFunctionExists)
include(CheckCXXSourceCompiles)
include(CheckIncludeFile)
include(Macros.cmake)
include(AlignOf.cmake)
include(CMakePackageConfigHelpers)

# ---------------------------------------------------------------------------
# Build options
# ---------------------------------------------------------------------------

set(GTIRB_PARALLEL_COMPILE_JOBS
    "0"
    CACHE
      STRING
      "Number of parallel compiler jobs. 0 means use all processors. Default is 0."
)

option(GTIRB_ENABLE_TESTS "Enable building and running unit tests." ON)

# This just sets the builtin BUILD_SHARED_LIBS, but if defaults to ON instead of
# OFF.
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

# These options control what APIs get built.
find_program(PYTHON "python3")
if(PYTHON)
  set(GTIRB_PY_API_DEFAULT ON)
else()
  set(GTIRB_PY_API_DEFAULT OFF)
endif()

option(GTIRB_CXX_API "Whether or not the C++ API is built." ON)
option(GTIRB_PY_API "Whether or not the Python API is built."
       ${GTIRB_PY_API_DEFAULT}
)

# ---------------------------------------------------------------------------
# Global settings
# ---------------------------------------------------------------------------

set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/src)
if(WIN32)
  set(CMAKE_DEBUG_POSTFIX
      "d"
      CACHE STRING "add a postfix, usually d on windows"
  )
endif()
set(CMAKE_RELEASE_POSTFIX
    ""
    CACHE STRING "add a postfix, usually empty on windows"
)
set(CMAKE_RELWITHDEBINFO_POSTFIX
    ""
    CACHE STRING "add a postfix, usually empty on windows"
)
set(CMAKE_MINSIZEREL_POSTFIX
    ""
    CACHE STRING "add a postfix, usually empty on windows"
)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

if(GTIRB_CXX_API)
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/include/gtirb/version.h.in"
    "${CMAKE_CURRENT_BINARY_DIR}/include/gtirb/version.h" @ONLY
  )
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/include/gtirb/version.h"
          DESTINATION include/gtirb
  )

  # Use C++17
  set(CMAKE_CXX_STANDARD 17)
  # Error if it's not available
  set(CMAKE_CXX_STANDARD_REQUIRED ON)

  # Specifically check for gcc-7 or later. gcc-5 is installed on many systems
  # and will accept -std=c++17, but does not fully support the standard.
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
  add_compile_definitions(BOOST_MULTI_INDEX_DISABLE_SERIALIZATION)

  # MSVC-specific Options
  if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
    if(NOT GTIRB_PARALLEL_COMPILE_JOBS STREQUAL "1")
      if(GTIRB_PARALLEL_COMPILE_JOBS STREQUAL "0")
        add_compile_options(-MP)
        message(STATUS "Parallel compilation enabled")
      else()
        add_compile_options(-MP${GTIRB_PARALLEL_COMPILE_JOBS})
        message(
          STATUS "Parallel compilation with ${GTIRB_PARALLEL_COMPILE_JOBS} jobs"
        )
      endif()
    else()
      message(STATUS "Parallel compilation disabled")
    endif()

    add_compile_definitions(_CRT_SECURE_NO_WARNINGS)
    add_compile_definitions(_SCL_SECURE_NO_WARNINGS)
    # We need to add both so that there is not a mismatch between Win32 SDK
    # headers (which use UNICODE) and C Standard Library headers (which use
    # _UNICODE).
    add_compile_definitions(_UNICODE)
    add_compile_definitions(UNICODE)
    add_compile_definitions(_WIN32)
    # Disable macro definitions for min and max that conflict with the STL.
    add_compile_definitions(NOMINMAX)
    # Enable RTTI. FIXME: stop using typeid so we can disable this and add -fno-
    # rtti to the Clang/GCC compiler options.
    add_compile_options(-GR)
    # Enabled a sensible warning level and treat all warnings as errors.
    add_compile_options(-W4)
    add_compile_options(-WX)

    add_compile_options(-sdl) # Enable extra security checks
    add_compile_options(-permissive-) # Disable permissive mode

    add_compile_options(-wd4996) # VC8: Deprecated libc functions.
    # This is a warning about a change in behavior from old versions of visual
    # c++.  We want the new (standard-compliant) behavior, so we don't want the
    # warning.  The warning is that using an array in a class initializer list
    # will cause its elements to be default initialized.
    add_compile_options(-wd4351)
    add_compile_options(-wd4146) # unary minus operator applied to unsigned
                                 # type, result still unsigned

    # Release target options
    add_compile_options($<$<CONFIG:Release>:-GL>) # Enable whole program
                                                  # optimization
    add_link_options($<$<CONFIG:Release>:-ltcg>) # Enable link-time code
                                                 # generation
  elseif((${CMAKE_CXX_COMPILER_ID} STREQUAL GNU) OR (${CMAKE_CXX_COMPILER_ID}
                                                     STREQUAL Clang)
  )
    add_compile_options(-Wall -Wextra -Wpointer-arith -Wshadow -Werror)
    add_compile_options(-fPIC -fno-exceptions)
  endif()
endif()

# ---------------------------------------------------------------------------
# Boost
# ---------------------------------------------------------------------------
if(GTIRB_CXX_API)
  find_package(Boost 1.67 REQUIRED)

  add_compile_definitions(BOOST_CONFIG_SUPPRESS_OUTDATED_MESSAGE)
  add_compile_definitions(BOOST_SYSTEM_NO_DEPRECATED)

  # Boost versions 1.70.0+ may use Boost's provided CMake support rather than
  # CMake's internal Boost support. The former uses "Boost::boost" and so on,
  # while the latter uses "Boost_BOOST" and so on. This normalizes the two cases
  # to use Boost_INCLUDE_DIRS and Boost_LIBRARIES.
  if(TARGET Boost::headers)
    get_target_property(
      Boost_INCLUDE_DIRS Boost::headers INTERFACE_INCLUDE_DIRECTORIES
    )
  endif()

  include_directories(SYSTEM ${Boost_INCLUDE_DIRS})
endif()

# ---------------------------------------------------------------------------
# Google Test Application
# ---------------------------------------------------------------------------
if(GTIRB_ENABLE_TESTS AND GTIRB_CXX_API)
  # Pull in Google Test
  # https://github.com/google/googletest/tree/master/googletest#incorporating-
  # into-an-existing-cmake-project

  # Download and unpack googletest at configure time
  configure_file(CMakeLists.googletest googletest-download/CMakeLists.txt)

  execute_process(
    COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}" .
    RESULT_VARIABLE result
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/googletest-download"
  )

  if(result)
    message(WARNING "CMake step for googletest failed: ${result}")
  endif()

  execute_process(
    COMMAND "${CMAKE_COMMAND}" --build .
    RESULT_VARIABLE result
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/googletest-download"
  )

  if(result)
    message(WARNING "Build step for googletest failed: ${result}")
  endif()

  # Prevent overriding the parent project's compiler/linker settings on Windows
  set(gtest_force_shared_crt
      ON
      CACHE BOOL "" FORCE
  )

  # Add googletest directly to our build. This defines the gtest and gtest_main
  # targets.
  add_subdirectory(
    "${CMAKE_BINARY_DIR}/googletest-src" "${CMAKE_BINARY_DIR}/googletest-build"
    EXCLUDE_FROM_ALL
  )

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

add_subdirectory(proto)

# ---------------------------------------------------------------------------
# gtirb sources
# ---------------------------------------------------------------------------
if(GTIRB_CXX_API)
  add_subdirectory(src)
endif()

if(GTIRB_PY_API)
  add_subdirectory(python)
endif()

add_subdirectory(doc)

# ---------------------------------------------------------------------------
# Export config for use by other CMake projects
# ---------------------------------------------------------------------------

if(GTIRB_CXX_API)
  # --- For direct use from the build directory/cmake registry ---
  # This exports the targets
  export(TARGETS gtirb proto
         FILE "${CMAKE_CURRENT_BINARY_DIR}/gtirbTargets.cmake"
  )
  # This is the main config file that find_package will look for.
  file(
    WRITE "${CMAKE_CURRENT_BINARY_DIR}/gtirbConfig.cmake"
    "
              include(\"\$\{CMAKE_CURRENT_LIST_DIR\}/gtirbTargets.cmake\")
              set_property(
                  TARGET gtirb
                  APPEND PROPERTY
                      INTERFACE_INCLUDE_DIRECTORIES \"${CMAKE_BINARY_DIR}/include\" \"${PROTOBUF_INCLUDE_DIRS}\"
              )
          "
  )
  # Add the build directory to the user CMake registry, so find_package can
  # locate it automatically.
  export(PACKAGE gtirb)

  # --- For the installed copy ---
  # Main config file for find_package, just includes the targets file.
  file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/export/gtirbConfig.cmake"
       "include(\"\$\{CMAKE_CURRENT_LIST_DIR\}/gtirbTargets.cmake\")"
  )
  # In this mode, find_package also seems to require a version file
  set(version_file "${CMAKE_CURRENT_BINARY_DIR}/gtirbConfig-version.cmake")
  write_basic_package_version_file(
    ${version_file}
    VERSION ${GTIRB_VERSION}
    COMPATIBILITY AnyNewerVersion
  )

  # Copy the config files to the install location
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/export/gtirbConfig.cmake
                ${version_file} DESTINATION lib/gtirb
  )
  # This exports the targets to the install location.
  install(EXPORT gtirbTargets DESTINATION lib/gtirb)
endif()
