set(PROJECT_NAME TestGTIRB)

enable_testing()
include_directories(${GTEST_INCLUDE_DIRS})

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

set(${PROJECT_NAME}_H)

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

gtirb_add_executable_gtest()

target_link_libraries(
  ${PROJECT_NAME} ${SYSLIBS} ${Boost_LIBRARIES} gtest gtest_main gtirb
)

# Add tests to make test
add_test(NAME testgtirb COMMAND $<TARGET_FILE:${PROJECT_NAME}>)

add_subdirectory(testInputBinary)
