SET(PROJECT_NAME gtirb)

add_subdirectory(proto)

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

if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
        #add_compile_options(-wd4251)  # Non-exportable template classes.
        add_compile_options(-wd4100) # unreferenced formal parameter, for protobuf includes
    add_compile_options(-DBOOST_UUID_RANDOM_PROVIDER_FORCE_WINCRYPT)
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
        #add_compile_options(-Wno-unused-function)
        add_compile_options(-mtune=generic)
        add_compile_options(-pthread)
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL Clang)
        #add_compile_options(-Wno-unused-function)
        add_compile_options(-mtune=generic)
        add_compile_options(-pthread)
endif()

# Find our headers without gtirb/ prefix
include_directories("${CMAKE_SOURCE_DIR}/include/gtirb")

# Find protobuf generated headers in the build directory
include_directories("${CMAKE_BINARY_DIR}/src/")

set(PUBLIC_HEADERS
        ${CMAKE_SOURCE_DIR}/include/gtirb/Allocator.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/AuxData.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/Block.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/ByteMap.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/Casting.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/Context.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/CFG.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/DataObject.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/Addr.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/Export.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/ImageByteMap.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/IR.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/Module.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/Node.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/Section.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/Symbol.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/SymbolicExpression.hpp
        ${CMAKE_SOURCE_DIR}/include/gtirb/gtirb.hpp
        )

set(${PROJECT_NAME}_H
        ${PUBLIC_HEADERS}
        ../src/Serialization.hpp
)

set(${PROJECT_NAME}_SRC
        AuxData.cpp
        Block.cpp
        ByteMap.cpp
        Context.cpp
        CFG.cpp
        DataObject.cpp
        ImageByteMap.cpp
        IR.cpp
        Module.cpp
        Node.cpp
        Section.cpp
        Serialization.cpp
        Symbol.cpp
        SymbolicExpression.cpp
)

file(GLOB ProtoFiles "${CMAKE_CURRENT_SOURCE_DIR}/proto/*.proto")
set(${PROJECT_NAME}_PROTO ${ProtoFiles})
source_group("proto" FILES ${ProtoFiles})

IF(UNIX AND NOT WIN32)
        SET(SYSLIBS ${CMAKE_DL_LIBS})
ELSE()
        SET(SYSLIBS)
endif()

GTIRB_ADD_LIBRARY()

add_dependencies(${PROJECT_NAME} Protobuf)
target_link_libraries(
  ${PROJECT_NAME}
  PUBLIC
  ${SYSLIBS}
  ${Boost_LIBRARIES}
  ${PROTOBUF_LIBRARIES}
  # Link in this static lib, but don't make it a transitive
  # dependency of TestGTIRB, etc
  PRIVATE
  proto)
target_compile_definitions(${PROJECT_NAME} PRIVATE GTIRB_${PROJECT_NAME}_EXPORTS)
# Force GSL to use std::byte instead of its own byte type.
# This should be auto-detected but it doesn't seem to work reliably.
target_compile_definitions(${PROJECT_NAME} PUBLIC GSL_USE_STD_BYTE)

# Copy gtirb headers into build directory
add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
  COMMAND "${CMAKE_COMMAND}" -E remove_directory "${CMAKE_BINARY_DIR}/include/gtirb")
add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
    COMMAND "${CMAKE_COMMAND}" -E make_directory "${CMAKE_BINARY_DIR}/include/gtirb")
add_custom_command(
        TARGET ${PROJECT_NAME} POST_BUILD
        COMMAND "${CMAKE_COMMAND}" -E copy
                ${PUBLIC_HEADERS}
                "${CMAKE_BINARY_DIR}/include/gtirb")

if(CLANG_TIDY_EXE)
  set_target_properties(
    ${PROJECT_NAME} PROPERTIES
    CXX_CLANG_TIDY "${CLANG_TIDY_EXE}"
  )
endif()

if(GTIRB_ENABLE_TESTS)
        add_subdirectory(test)
endif()

install(TARGETS ${PROJECT_NAME} EXPORT gtirbTargets
  INCLUDES DESTINATION include
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib)
install(FILES ${PUBLIC_HEADERS} DESTINATION include/gtirb)
install(DIRECTORY "${CMAKE_BINARY_DIR}/include/proto" DESTINATION include)
