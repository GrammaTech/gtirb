SET(PROJECT_NAME TestGTIRB)

enable_testing()
include_directories(${GTEST_INCLUDE_DIRS})

add_compile_options(-DDEBUG)

# Required warning suppression (TODO: Remove!)
if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
	#add_compile_options(-wd4251)  # Non-exportable template classes.
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
	add_compile_options(-fpermissive)
	add_compile_options(-mtune=generic)
	add_compile_options(-pthread)
elseif(${CMAKE_CXX_COMPILEUR_ID} STREQUAL Clang)
	add_compile_options(-fpermissive)
	add_compile_options(-mtune=generic)
	add_compile_options(-pthread)
endif()

set(${PROJECT_NAME}_H
)

set(${PROJECT_NAME}_SRC
	AddrRanges.test.cpp
	EA.test.cpp
	Exception.test.cpp
	Instruction.test.cpp
	IR.test.cpp
	LocalProperties.test.cpp
	Module.test.cpp
	ModuleAux.test.cpp
	ModuleCore.test.cpp
	ModuleSectionBase.test.cpp
	ModuleSummary.test.cpp
	Node.test.cpp
	NodeError.test.cpp
	NodeUtilities.test.cpp
	NodeValidators.test.cpp
	Procedure.test.cpp
	Symbol.test.cpp
	SymbolSet.test.cpp
	TableTemplate.test.cpp
	TypedExceptionTest.cpp
	TypedNodeTest.cpp
	Utilities.test.cpp
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
	GTIRB
)

add_test(NAME ${PROJECT_NAME} COMMAND ${PROJECT_NAME})
set_property(TARGET ${PROJECT_NAME} PROPERTY FOLDER "GTIRB/Test")
