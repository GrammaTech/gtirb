SET(PROJECT_NAME TestGTIRB)

enable_testing()
include_directories(${GTEST_INCLUDE_DIRS})

add_compile_options(-DDEBUG)

# Required warning suppression (TODO: Remove!)
if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
	#add_compile_options(-wd4251)  # Non-exportable template classes.
	add_compile_options(-wd4389)   # signed/unsigned mismatch (in gtest headers)
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
	add_compile_options(-fpermissive)
	add_compile_options(-mtune=generic)
	add_compile_options(-pthread)
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL Clang)
	add_compile_options(-fpermissive)
	add_compile_options(-mtune=generic)
	add_compile_options(-pthread)
endif()

set(${PROJECT_NAME}_H
)

set(${PROJECT_NAME}_SRC
        Block.test.cpp
	ByteMap.test.cpp
	CFG.test.cpp
        DataObject.test.cpp
    Addr.test.cpp
	ImageByteMap.test.cpp
	IR.test.cpp
	Module.test.cpp
	Node.test.cpp
        NodeRef.test.cpp
        Section.test.cpp
	Symbol.test.cpp
        SymbolicExpression.test.cpp
        Table.test.cpp
	TypedNodeTest.cpp
)

IF(UNIX AND NOT WIN32)
	SET(SYSLIBS
		dl
	)
ELSE()
	SET(SYSLIBS
	)
endif()

GTIRB_ADD_EXECUTABLE_GTEST()

target_link_libraries(
	${PROJECT_NAME} 
	${SYSLIBS}
	${Boost_LIBRARIES}
	gtest 
	gtest_main
	gtirb
)

# Emulate autotools "make check"
add_custom_target(check DEPENDS ${PROJECT_NAME})
add_custom_command(TARGET check COMMAND POST_BUILD COMMAND $<TARGET_FILE:${PROJECT_NAME}>)

add_subdirectory(testInputBinary)
