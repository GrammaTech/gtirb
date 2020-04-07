set(PROJECT_NAME gtirb)

add_subdirectory(gtirb/proto)

if(GTIRB_RUN_CLANG_TIDY)
  find_program(
    CLANG_TIDY_EXE
    NAMES "clang-tidy"
    DOC "Path to clang-tidy executable"
  )
  if(NOT CLANG_TIDY_EXE)
    message(STATUS "clang-tidy not found.")
  else()
    message(STATUS "clang-tidy found: ${CLANG_TIDY_EXE}")
  endif()
else()
  message(STATUS "clang-tidy disabled.")
endif()

if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
  # add_compile_options(-wd4251)  # Non-exportable template classes.
  add_compile_options(-DBOOST_UUID_RANDOM_PROVIDER_FORCE_WINCRYPT)
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
  # add_compile_options(-Wno-unused-function)
  add_compile_options(-mtune=generic)
  add_compile_options(-pthread)
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL Clang)
  # add_compile_options(-Wno-unused-function)
  add_compile_options(-mtune=generic)
  add_compile_options(-pthread)
endif()

# generate version.h from version.h.in
configure_file(
  "${CMAKE_SOURCE_DIR}/include/gtirb/version.h.in"
  "${CMAKE_BINARY_DIR}/include/gtirb/version.h" @ONLY
)

# specify source files
set(${PROJECT_NAME}_H
    "${CMAKE_SOURCE_DIR}/include/gtirb/Addr.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/Allocator.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/AuxData.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/AuxDataContainer.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/AuxDataSchema.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/ByteInterval.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/CFG.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/Casting.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/CfgNode.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/CodeBlock.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/Context.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/DataBlock.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/ErrorOr.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/Export.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/IR.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/Module.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/Node.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/Observer.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/Offset.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/ProxyBlock.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/Section.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/Symbol.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/SymbolicExpression.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/Utility.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/gtirb.hpp"
    "${CMAKE_SOURCE_DIR}/include/gtirb/version.h.in"
    "CFGSerialization.hpp"
    "Serialization.hpp"
    "SymbolicExpressionSerialization.hpp"
)

# specify source files
set(${PROJECT_NAME}_SRC
    AuxData.cpp
    AuxDataContainer.cpp
    ByteInterval.cpp
    CodeBlock.cpp
    Context.cpp
    CFG.cpp
    DataBlock.cpp
    IR.cpp
    Module.cpp
    Node.cpp
    Offset.cpp
    ProxyBlock.cpp
    Section.cpp
    Serialization.cpp
    Symbol.cpp
    SymbolicExpression.cpp
    Utility.cpp
)

file(GLOB ProtoFiles "${CMAKE_CURRENT_SOURCE_DIR}/gtirb/proto/*.proto")
set(${PROJECT_NAME}_PROTO ${ProtoFiles})
source_group("proto" FILES ${ProtoFiles})

if(UNIX AND NOT WIN32)
  set(SYSLIBS ${CMAKE_DL_LIBS})
else()
  set(SYSLIBS)
endif()

gtirb_add_library()
# FIXME We should be setting SOVERSION with an ABI version number once the ABI
# stabilizes. For now, since most patches affect the ABI anyway, we just use the
# GTIRB_VERSION as the SOVERSION.
set_target_properties(${PROJECT_NAME} PROPERTIES VERSION ${GTIRB_VERSION})
if(WIN32)
  # FIXME: We need to figure out how to handle cutting a release build. For the
  # moment, treat everything as a pre-release build.
  set(GTIRB_IS_PRERELEASE 1)
  configure_file(
    ${CMAKE_SOURCE_DIR}/resources/windows_version_resource.rc.in
    ${CMAKE_BINARY_DIR}/version.rc @ONLY
  )
  target_sources(${PROJECT_NAME} PRIVATE ${CMAKE_BINARY_DIR}/version.rc)
  source_group(Resources FILES ${CMAKE_BINARY_DIR}/version.rc)
endif()

# Find our headers
target_include_directories(
  ${PROJECT_NAME} PUBLIC $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
)
target_include_directories(
  ${PROJECT_NAME} PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>
)
target_include_directories(
  ${PROJECT_NAME} PRIVATE $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include/gtirb>
)
target_include_directories(
  ${PROJECT_NAME} PRIVATE $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/src/>
)

target_link_libraries(
  ${PROJECT_NAME}
  PUBLIC ${SYSLIBS}
         ${Boost_LIBRARIES}
         # Link in this static lib, but don't make it a transitive dependency of
         # TestGTIRB, etc
  PRIVATE proto
)
target_compile_definitions(
  ${PROJECT_NAME} PRIVATE GTIRB_${PROJECT_NAME}_EXPORTS
)
target_include_directories(${PROJECT_NAME} PUBLIC "${PROTOBUF_INCLUDE_DIRS}")

if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
  # These four warnings come from protobuf headers, disabling them this way
  # means that projects which link to gtirb via cmake won't have to deal with
  # them.
  target_compile_options(${PROJECT_NAME} PUBLIC -wd4100) # unreferenced formal
                                                         # parameter
  target_compile_options(
    ${PROJECT_NAME} PUBLIC -wd4127
  ) # conditional expression is constant
  target_compile_options(
    ${PROJECT_NAME} PUBLIC -wd4244
  ) # conversion from 'type1' to 'type2', possible loss of data
  target_compile_definitions(
    ${PROJECT_NAME} PUBLIC
    _SILENCE_CXX17_ITERATOR_BASE_CLASS_DEPRECATION_WARNING
  )

  target_compile_options(
    ${PROJECT_NAME} PUBLIC -wd4251
  ) # 'identifier' : class 'type' needs to have dll- interface to be used by
    # clients of class 'type2'
  target_compile_options(${PROJECT_NAME} PUBLIC -wd4275) # Non-dll interface
                                                         # base classes.
endif()

if(CLANG_TIDY_EXE)
  set_target_properties(
    ${PROJECT_NAME} PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}"
  )
endif()

if(GTIRB_ENABLE_TESTS)
  add_subdirectory(test)
endif()

install(
  TARGETS ${PROJECT_NAME}
  COMPONENT library
  EXPORT gtirbTargets
  INCLUDES
  DESTINATION include
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)
install(
  FILES ${${PROJECT_NAME}_H}
  COMPONENT headers
  DESTINATION include/gtirb
)
install(
  DIRECTORY "${CMAKE_BINARY_DIR}/include/gtirb/proto"
  COMPONENT headers
  DESTINATION include/gtirb
)
install(
  FILES "${CMAKE_BINARY_DIR}/include/gtirb/version.h"
  COMPONENT headers
  DESTINATION include/gtirb
)
