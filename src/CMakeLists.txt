set(PROJECT_NAME gtirb)

add_subdirectory(proto)

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

# Find our headers without gtirb/ prefix
include_directories("${CMAKE_BINARY_DIR}/include/gtirb")

# Find protobuf generated headers in the build directory
include_directories("${CMAKE_BINARY_DIR}/src/")

set(PUBLIC_HEADERS
    ${CMAKE_SOURCE_DIR}/include/gtirb/Allocator.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/AuxData.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/AuxDataContainer.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/ByteInterval.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/ByteVector.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/CodeBlock.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/Casting.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/Context.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/CFG.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/CfgNode.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/DataBlock.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/Addr.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/Export.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/IR.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/Module.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/Node.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/ProxyBlock.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/Section.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/Serialization.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/Symbol.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/SymbolicExpression.hpp
    ${CMAKE_SOURCE_DIR}/include/gtirb/gtirb.hpp
)

set(${PROJECT_NAME}_H ${PUBLIC_HEADERS})

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
    ProxyBlock.cpp
    Section.cpp
    Serialization.cpp
    Symbol.cpp
    SymbolicExpression.cpp
)

file(GLOB ProtoFiles "${CMAKE_CURRENT_SOURCE_DIR}/proto/*.proto")
set(${PROJECT_NAME}_PROTO ${ProtoFiles})
source_group("proto" FILES ${ProtoFiles})

if(UNIX AND NOT WIN32)
  set(SYSLIBS ${CMAKE_DL_LIBS})
else()
  set(SYSLIBS)
endif()

gtirb_add_library()

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

add_custom_command(
  TARGET ${PROJECT_NAME}
  POST_BUILD
  COMMAND "${CMAKE_COMMAND}" -E copy ${PUBLIC_HEADERS}
          "${CMAKE_BINARY_DIR}/include/gtirb"
)

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
  EXPORT gtirbTargets
  INCLUDES
  DESTINATION include
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)
install(FILES ${PUBLIC_HEADERS} DESTINATION include/gtirb)
install(DIRECTORY "${CMAKE_BINARY_DIR}/include/proto" DESTINATION include)
