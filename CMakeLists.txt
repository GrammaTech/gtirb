#
# Cmake Configuration
#

# Need 3.10 to support CXX_STANDARD=17 and protobuf::protoc
cmake_minimum_required(VERSION 3.10.0)

# The version.txt file is the official record of the version number. We use the
# contents of that file to set the project version for use in other CMake files.
file(READ "${CMAKE_CURRENT_SOURCE_DIR}/version.txt" ver)

string(REGEX MATCH "VERSION_MAJOR ([0-9]*)" _ ${ver})
set(GTIRB_MAJOR_VERSION ${CMAKE_MATCH_1})

string(REGEX MATCH "VERSION_MINOR ([0-9]*)" _ ${ver})
set(GTIRB_MINOR_VERSION ${CMAKE_MATCH_1})

string(REGEX MATCH "VERSION_PATCH ([0-9]*)" _ ${ver})
set(GTIRB_PATCH_VERSION ${CMAKE_MATCH_1})

string(REGEX MATCH "VERSION_PROTOBUF ([0-9]*)" _ ${ver})
set(GTIRB_PROTOBUF_VERSION ${CMAKE_MATCH_1})

cmake_policy(SET CMP0048 NEW)
project(
  GTIRB
  VERSION "${GTIRB_MAJOR_VERSION}.${GTIRB_MINOR_VERSION}.${GTIRB_PATCH_VERSION}"
)
set(PACKAGE_BRANCH master)

include(CheckFunctionExists)
include(CheckCXXSourceCompiles)
include(CheckIncludeFile)
include(Macros.cmake)
include(AlignOf.cmake)
include(CMakePackageConfigHelpers)

option(ENABLE_CONAN "Use Conan to inject dependencies" OFF)

if(ENABLE_CONAN)
  set(CONAN_SYSTEM_INCLUDES ON)
  include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
  conan_basic_setup()
endif()

# ---------------------------------------------------------------------------
# Build options
# ---------------------------------------------------------------------------

set(GTIRB_MSVC_PARALLEL_COMPILE_JOBS
    "0"
    CACHE
      STRING
      "Number of parallel compiler jobs used for Visual Studio compiles. 0 means use all processors. Default is 0."
)

option(GTIRB_ENABLE_TESTS "Enable building and running unit tests." ON)
option(GTIRB_ENABLE_MYPY "Enable checking python types with mypy." ON)

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

# Set ENABEL_CODE_COVERAGE to default off, unless you want to test c++ coverage
option(ENABLE_CODE_COVERAGE
       "Build with instrumentation for collecting code coverage" OFF
)

if(ENABLE_CODE_COVERAGE)
  if(${CMAKE_CXX_COMPILER_ID} STREQUAL GNU OR ${CMAKE_CXX_COMPILER_ID} STREQUAL
                                              Clang
  )
    add_compile_options(--coverage)
    link_libraries(--coverage)
  else()
    message(FATAL_ERROR "no support for code coverage on this target")
  endif()
endif()

# Whether or not to run clang-tidy (if present)
option(GTIRB_RUN_CLANG_TIDY "Enable running of clang-tidy." ON)

# Define the cache variables for the API options.
option(GTIRB_CXX_API "Whether or not the C++ API is built." ON)
option(GTIRB_PY_API "Whether or not the Python API is built." ON)
option(GTIRB_CL_API "Whether or not the Common Lisp API is built." ON)
option(GTIRB_JAVA_API "Whether or not the Java API is built." ON)

# Determine whether or not to strip debug symbols and set the build-id. This is
# only really needed when we are building ubuntu *-dbg packages
option(GTIRB_STRIP_DEBUG_SYMBOLS
       "Whether or not to strip debug symbols and set the build-id." OFF
)

option(
  GTIRB_RELEASE_VERSION
  "Whether or not to build package versions without dev/SNAPSHOT suffixes.  Applies to the python and java APIs."
  OFF
)

# Determine whether or not the APIs are REALLY built or not.
# === C++ ===
set(CXX_API ${GTIRB_CXX_API})

# === Python ===
set(PY_API ${GTIRB_PY_API})
if(GTIRB_PY_API)
  gtirb_find_python()

  if(PYTHON)
    set(PYTHON_MINIMUM_VERSION "3.5.2")
    execute_process(
      COMMAND "${PYTHON}" "--version" OUTPUT_VARIABLE PYTHON_VERSION
    )
    string(REPLACE "Python" "" PYTHON_VERSION "${PYTHON_VERSION}")

    if("${PYTHON_VERSION}" VERSION_LESS "${PYTHON_MINIMUM_VERSION}")
      message(
        WARNING
          "${PYTHON} --version is ${PYTHON_VERSION}, which is less then the minimum required, ${PYTHON_MINIMUM_VERSION}; disabling building of API."
      )
      set(PY_API OFF)
    endif()
  else()
    message(
      WARNING
        "Python interpreter not found; disabling building of Python API.
If this is in error, try giving -DPYTHON=... to CMake to specify what program to use."
    )
    set(PY_API OFF)
  endif()
endif()

# === Common Lisp ===
# TODO: test the CL API on other CL interpreters and search for those in
# addition to SBCL when looking for a default CL interpeter
set(CL_API ${GTIRB_CL_API})
if(GTIRB_CL_API)
  find_program(LISP "sbcl")
  set(QUICKLISP
      "$ENV{HOME}/quicklisp"
      CACHE STRING "The Quicklisp installation to use."
  )
  set(LISP_MINIMUM_VERSION "1.4.5")

  if(NOT LISP)
    message(
      WARNING
        "Lisp interpreter not found; disabling building of Lisp API.
If this is in error, try giving -DLISP=... to CMake to specify what program to use."
    )
    set(CL_API OFF)
  elseif(NOT EXISTS "${QUICKLISP}")
    message(
      WARNING
        "Quicklisp installation not found; disabling building of Lisp API.
If this is in error, try giving -DQUICKLISP=... to CMake to specify what directory to use."
    )
    set(CL_API OFF)
  else()
    execute_process(COMMAND "${LISP}" "--version" OUTPUT_VARIABLE LISP_VERSION)
    string(REPLACE "SBCL" "" LISP_VERSION "${LISP_VERSION}")
    string(REPLACE ".debian" "" LISP_VERSION "${LISP_VERSION}")

    if("${LISP_VERSION}" VERSION_LESS "${LISP_VERSION}")
      message(
        WARNING
          "${LISP} --version is ${LISP_VERSION}, which is less then the minimum required, ${LISP_MINIMUM_VERSION}; disabling building of API."
      )
      set(CL_API OFF)
    endif()
  endif()
endif()

# === Java ===
set(JAVA_API ${GTIRB_JAVA_API})
if(GTIRB_JAVA_API)
  find_package(Java 1.8.0 COMPONENTS Development)
  if(NOT JAVA_FOUND)
    message(WARNING "Java 8 compiler not found; disabling building of Java API.
If this is in error, try setting the environment variable $JAVA_HOME."
    )
    set(JAVA_API OFF)
  else()
    find_program(MVN mvn)
    if(NOT MVN)
      message(
        WARNING
          "Maven not found; disabling building of Java API. If this is in "
          "error, try setting -DMVN=<path-to-maven> on the CMake command-line"
      )
      set(JAVA_API OFF)
    endif()
  endif()
endif()

# Documentation options.
option(GTIRB_DOCUMENTATION "Whether or not documentation is built." ON)

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

if(CXX_API)
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

  #
  # Global Options (Compile / Link)
  #
  add_compile_options(-DBOOST_MULTI_INDEX_DISABLE_SERIALIZATION)

  # MSVC-specific Options
  if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
    if(NOT GTIRB_MSVC_PARALLEL_COMPILE_JOBS STREQUAL "1")
      if(GTIRB_MSVC_PARALLEL_COMPILE_JOBS STREQUAL "0")
        add_compile_options(-MP)
        message(STATUS "Parallel compilation enabled")
      else()
        add_compile_options(-MP${GTIRB_MSVC_PARALLEL_COMPILE_JOBS})
        message(
          STATUS
            "Parallel compilation with ${GTIRB_MSVC_PARALLEL_COMPILE_JOBS} jobs"
        )
      endif()
    else()
      message(STATUS "Parallel compilation disabled")
    endif()

    add_compile_options(-D_CRT_SECURE_NO_WARNINGS)
    add_compile_options(-D_MBCS)
    add_compile_options(-D_SCL_SECURE_NO_WARNINGS)
    # We need to add both so that there is not a mismatch between Win32 SDK
    # headers (which use UNICODE) and C Standard Library headers (which use
    # _UNICODE).
    add_compile_options(-D_UNICODE)
    add_compile_options(-DUNICODE)
    add_compile_options(-D_WIN32)
    # Disable macro definitions for min and max that conflict with the STL.
    add_compile_options(-DNOMINMAX)
    # Enable RTTI. FIXME: stop using typeid so we can disable this and add -fno-
    # rtti to the Clang/GCC compiler options.
    add_compile_options(-GR)
    # Enable exceptions, which are basically required because of our reliance on
    # boost.
    add_compile_options(-EHsc)
    # Enabled a sensible warning level and treat all warnings as errors.
    add_compile_options(-W4)
    add_compile_options(-WX)

    # Enable bigobj support, otherwise IR.cpp and Module.cpp will refuse to
    # compile due to execeeding the number of sections allowed in an object
    # file. FIXME: we should not have that many template instantiations.
    add_compile_options(-bigobj)

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

    # C4505: 'google::protobuf::internal::MapField<...>::ContainsMapKey':
    # unreferenced local function has been removed
    add_compile_options(-wd4505)

    # C4267: protobuf-generated headers, at least w/ protobuf 3.9.1, trigger
    # MSVC's "conversion from 'size_t' to 'int', possible loss of data" warning.
    add_compile_options(-wd4267)

    # Release target options
    add_compile_options($<$<CONFIG:Release>:-GL>) # Enable whole program
                                                  # optimization
    add_link_options($<$<CONFIG:Release>:-ltcg>) # Enable link-time code
                                                 # generation
  elseif((${CMAKE_CXX_COMPILER_ID} STREQUAL GNU) OR (${CMAKE_CXX_COMPILER_ID}
                                                     STREQUAL Clang)
  )
    add_compile_options(-Wall -Wextra -Wpointer-arith -Wshadow -Werror)
    add_compile_options(-fPIC)
  endif()
endif()

# ---------------------------------------------------------------------------
# Boost
# ---------------------------------------------------------------------------
if(CXX_API)
  find_package(Boost 1.67 REQUIRED)

  add_compile_options(-DBOOST_CONFIG_SUPPRESS_OUTDATED_MESSAGE)
  add_compile_options(-DBOOST_SYSTEM_NO_DEPRECATED)

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
if(GTIRB_ENABLE_TESTS AND CXX_API)
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
if(CL_API)
  find_package(Protobuf 3.7.0 REQUIRED)
else()
  find_package(Protobuf 3.0.0 REQUIRED)
endif()

if(Protobuf_VERSION VERSION_LESS 3.2)
  add_definitions(-DPROTOBUF_SET_BYTES_LIMIT)
endif()

if(NOT BUILD_SHARED_LIBS)
  set(Protobuf_USE_STATIC_LIBS ON)
endif()
include_directories(SYSTEM ${PROTOBUF_INCLUDE_DIRS})

add_subdirectory(proto)

# ---------------------------------------------------------------------------
# gtirb sources
# ---------------------------------------------------------------------------
if(CXX_API)
  add_subdirectory(src)
endif()

if(PY_API)
  add_subdirectory(python)
endif()

if(CL_API)
  add_subdirectory(cl)
endif()

if(JAVA_API)
  add_subdirectory(java)
endif()

if(GTIRB_DOCUMENTATION)
  add_subdirectory(doc)
endif()

# ---------------------------------------------------------------------------
# Export config for use by other CMake projects
# ---------------------------------------------------------------------------

if(CXX_API)
  # --- For direct use from the build directory/cmake registry ---
  # This exports the targets
  export(TARGETS gtirb gtirb_proto
         FILE "${CMAKE_CURRENT_BINARY_DIR}/gtirbTargets.cmake"
  )
  # This is the main config file that find_package will look for.
  configure_file(
    "${CMAKE_CURRENT_LIST_DIR}/gtirbConfig.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/gtirbConfig.cmake" @ONLY
  )
  # Add the build directory to the user CMake registry, so find_package can
  # locate it automatically.
  export(PACKAGE gtirb)

  # --- For the installed copy ---
  # Main config file for find_package, includes the targets file and defines the
  # check_gtirb_branch function.
  if(NOT DEFINED PACKAGE_BRANCH)
    set(PACKAGE_BRANCH "No package branch specified")
  endif()
  # FIXME: The installed version of gtirbConfig currently contains the
  # check_gtirb_branch function, which requires users to explicitly call it. We
  # ought to move this functionality to gtirbConfig-version, so that checking
  # the gtirb version also checks the branch, requiring users to opt-out of the
  # branch check, rather than opt-in by calling check_gtirb_branch. See: GitLab
  # issue #93
  configure_file(
    "${CMAKE_CURRENT_LIST_DIR}/gtirbConfig.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/export/gtirbConfig.cmake" @ONLY
  )

  # In this mode, find_package also seems to require a version file
  set(version_file "${CMAKE_CURRENT_BINARY_DIR}/gtirbConfig-version.cmake")
  write_basic_package_version_file(
    ${version_file}
    VERSION ${GTIRB_VERSION}
    COMPATIBILITY AnyNewerVersion
  )

  # Copy the config files to the install location
  install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/export/gtirbConfig.cmake ${version_file}
    DESTINATION lib/gtirb
    COMPONENT cmake_config
  )
  # This exports the targets to the install location.
  install(
    EXPORT gtirbTargets
    COMPONENT cmake_target
    DESTINATION lib/gtirb
  )
endif()

# ---------------------------------------------------------------------------
# Package policy enforcement
# ---------------------------------------------------------------------------

if(GTIRB_PACKAGE_POLICY)
  set(PACKAGE_POLICY ${GTIRB_PACKAGE_POLICY})
elseif(ENABLE_CONAN OR WIN32)
  set(PACKAGE_POLICY conan)
else()
  set(PACKAGE_POLICY unix)
endif()

if(PACKAGE_POLICY STREQUAL "unix")

  # Provides copyright file for Unix packages.
  install(
    FILES ${CMAKE_SOURCE_DIR}/LICENSE.txt
    COMPONENT license
    DESTINATION share/doc/gtirb
    RENAME copyright
  )

elseif(PACKAGE_POLICY STREQUAL "conan")

  # Provides LICENSE.txt for Conan packages
  install(
    FILES ${CMAKE_SOURCE_DIR}/LICENSE.txt
    COMPONENT license
    DESTINATION licenses
  )

endif()

# ---------------------------------------------------------------------------
# Package generation with cpack
# ---------------------------------------------------------------------------
set(CPACK_PROJECT_CONFIG_FILE ${CMAKE_CURRENT_SOURCE_DIR}/cpack-config.cmake)

set(CMAKE_PROJECT_HOMEPAGE_URL https://github.com/GrammaTech/gtirb)
set(CPACK_PACKAGE_VERSION_MAJOR ${GTIRB_MAJOR_VERSION})
set(CPACK_PACKAGE_VERSION_MINOR ${GTIRB_MINOR_VERSION})
set(CPACK_PACKAGE_VERSION_PATCH ${GTIRB_PATCH_VERSION})
set(CPACK_PACKAGE_VENDOR "GrammaTech Inc.")
set(CPACK_PACKAGE_CONTACT gtirb@grammatech.com)
set(CPACK_PACKAGE_DESCRIPTION_FILE ${CMAKE_CURRENT_SOURCE_DIR}/README.md)
set(CPACK_PACKAGE_RESOURCE_FILE_LICENSE ${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.md)
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY
    "The GrammaTech Intermediate Representation for Binaries (GTIRB) is a machine code analysis and rewriting data structure."
)
set(CPACK_DEBIAN_PACKAGE_SECTION devel)

string(REGEX MATCH "([^\.]+)\.([^\.]+)\.([^\.]+)" PROTOBUF_VERSION_MATCH
             ${Protobuf_VERSION}
)
set(PROTOBUF_MAJOR_VERSION ${CMAKE_MATCH_1})
set(PROTOBUF_MINOR_VERSION ${CMAKE_MATCH_2})
set(PROTOBUF_PATCH_VERSION ${CMAKE_MATCH_3})
math(EXPR NEXT_PROTOBUF_PATCH "${PROTOBUF_PATCH_VERSION}+1")
set(CPACK_PROTOBUF_VERSION_UPPER_BOUND
    "${PROTOBUF_MAJOR_VERSION}.${PROTOBUF_MINOR_VERSION}.${NEXT_PROTOBUF_PATCH}"
)
set(CPACK_PROTOBUF_VERSION_LOWER_BOUND "${Protobuf_VERSION}")
set(CPACK_GTIRB_VERSION "${GTIRB_VERSION}")
set(CPACK_SOURCE_DIR ${CMAKE_SOURCE_DIR})

include(CPack)

# ---------------------------------------------------------------------------
# Report APIs and features built
# ---------------------------------------------------------------------------

message("APIs to be built:")
message("    C++     ${CXX_API}")
message("    Python  ${PY_API}")
message("    Lisp    ${CL_API}")
message("    Java    ${JAVA_API}")
