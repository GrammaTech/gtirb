set(PROJECT_NAME TestGTIRB)

enable_testing()
include_directories(${GTEST_INCLUDE_DIRS})
include_directories(${CMAKE_CURRENT_BINARY_DIR})

add_compile_options(-DDEBUG)

# Required warning suppression (TODO: Remove!)
if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
  # add_compile_options(-wd4251)  # Non-exportable template classes.
  add_compile_options(-wd4389) # signed/unsigned mismatch (in gtest headers)
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
  add_compile_options(-fpermissive)
  add_compile_options(-mtune=generic)
  add_compile_options(-pthread)
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL Clang)
  add_compile_options(-fpermissive)
  add_compile_options(-mtune=generic)
  add_compile_options(-pthread)
endif()

# Check for access to resource.h
include(CheckIncludeFiles)
check_include_files(sys/resource.h HAVE_RESOURCE_H)
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/config-test.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/config-test.h
)

set(${PROJECT_NAME}_H AuxDataContainerSchema.hpp Main.test.hpp
                      PrepDeathTest.hpp
)

set(${PROJECT_NAME}_SRC
    Addr.test.cpp
    Allocator.test.cpp
    AuxData.test.cpp
    AuxDataContainer.test.cpp
    ByteInterval.test.cpp
    CFG.test.cpp
    CodeBlock.test.cpp
    DataBlock.test.cpp
    IR.test.cpp
    Main.test.cpp
    MergeSortedIterator.test.cpp
    Module.test.cpp
    Node.test.cpp
    Offset.test.cpp
    ProxyBlock.test.cpp
    Section.test.cpp
    Symbol.test.cpp
    SymbolicExpression.test.cpp
    TypedNodeTest.cpp
)

if(UNIX AND NOT WIN32)
  set(SYSLIBS dl)
else()
  set(SYSLIBS)
endif()

add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_H} ${${PROJECT_NAME}_SRC})
set_target_properties(${PROJECT_NAME} PROPERTIES FOLDER "gtirb/test")

target_link_libraries(
  ${PROJECT_NAME} ${SYSLIBS} ${Boost_LIBRARIES} gtest gtest_main gtirb
)

set(PROJECT_NAME PrepTestGTIRB)

set(${PROJECT_NAME}_H AuxDataContainerSchema.hpp)

set(${PROJECT_NAME}_SRC PrepTestGTIRB.cpp)

add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_H} ${${PROJECT_NAME}_SRC})
set_target_properties(${PROJECT_NAME} PROPERTIES FOLDER "gtirb/test")

target_link_libraries(${PROJECT_NAME} ${SYSLIBS} ${Boost_LIBRARIES} gtirb)

# Add tests to make test
add_test(
  NAME testgtirbc++
  COMMAND ${CMAKE_COMMAND} -DCMD1=$<TARGET_FILE:PrepTestGTIRB>
          -DCMD2=$<TARGET_FILE:TestGTIRB> -P
          ${CMAKE_CURRENT_SOURCE_DIR}/runtests.cmake
)

set(PROJECT_NAME testgtirb_schemac++)

set(${PROJECT_NAME}_H AuxDataContainerSchema.hpp PrepDeathTest.hpp)

set(${PROJECT_NAME}_SRC AuxDataSchemaRegistration.test.cpp)

# Add tests to make test
gtirb_add_executable_gtest()

target_link_libraries(
  ${PROJECT_NAME} ${SYSLIBS} ${Boost_LIBRARIES} gtest gtest_main gtirb
)

add_subdirectory(testInputBinary)
