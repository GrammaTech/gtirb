#
# Cmake Configuration
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.5.1)

PROJECT(GT-IRB)

include(CheckFunctionExists)
include(CheckCXXSourceCompiles)
include(CheckIncludeFile)
include(Macros.cmake)
include(AlignOf.cmake)

#
# Global Options (CMake)
#

set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/src)
set(CMAKE_DEBUG_POSTFIX          "d" CACHE STRING "add a postfix, usually d on windows")
set(CMAKE_RELEASE_POSTFIX        ""  CACHE STRING "add a postfix, usually empty on windows")
set(CMAKE_RELWITHDEBINFO_POSTFIX ""  CACHE STRING "add a postfix, usually empty on windows")
set(CMAKE_MINSIZEREL_POSTFIX     ""  CACHE STRING "add a postfix, usually empty on windows")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bin)
set(CMAKE_CXX_EXTENSIONS OFF)

#
# Grammatech Options (CMake)
#

option(GTIRB_ENABLE_TESTS "Enable building and running unit tests." ON)

set(GTIRB_ISO_CPP_VERSION "C++14" CACHE STRING "C++ ISO Standard to use.")
set_property(CACHE GTIRB_ISO_CPP_VERSION PROPERTY STRINGS C++17 C++14)

# Default to C++17, then override to C++14 if necessary.
if(${GTIRB_ISO_CPP_VERSION} STREQUAL "C++14")
	set(CMAKE_CXX_STANDARD 14)
else()
	set(CMAKE_CXX_STANDARD 17)
endif()

# Base include path for the GT-IRB library.
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/utilities)

#
# Global Options (Compile / Link)
#

add_compile_options(-DBOOST_CONFIG_SUPPRESS_OUTDATED_MESSAGE)

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

	# Microsoft VCPKG
	set(CMAKE_TOOLCHAIN_FILE "C:/vcpkg/scripts/buildsystems/vcpkg.cmake" CACHE PATH "")
    include(${CMAKE_TOOLCHAIN_FILE})
elseif((${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
       OR (${CMAKE_CXX_COMPILER_ID} STREQUAL Clang))
    add_compile_options(-Wall -Wpointer-arith -Wshadow -Werror)
    add_compile_options(-fPIC)
endif()

# ---------------------------------------------------------------------------
# Boost
# ---------------------------------------------------------------------------

add_compile_options(-DBOOST_SYSTEM_NO_DEPRECATED)
find_package(Boost 1.59 REQUIRED COMPONENTS serialization filesystem system program_options)
include_directories(${Boost_INCLUDE_DIRS})

# ---------------------------------------------------------------------------
# Microsoft GSL 
# ---------------------------------------------------------------------------
configure_file(CMakeLists.gsl gsl-download/CMakeLists.txt)

execute_process(COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}" . 
	RESULT_VARIABLE result
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/gsl-download" )

if(result)
	message(WARNING "CMake step for gsl failed: ${result}")
endif()

execute_process(COMMAND "${CMAKE_COMMAND}" --build .
	RESULT_VARIABLE result
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/gsl-download" )

if(result)
	message(WARNING "Build step for gsl failed: ${result}")
endif()

add_subdirectory("${CMAKE_BINARY_DIR}/gsl-src"
				 "${CMAKE_BINARY_DIR}/gsl-build"
				 EXCLUDE_FROM_ALL)

include_directories("${CMAKE_BINARY_DIR}/gsl-src/include")

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
find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIRS})

add_subdirectory(src)
