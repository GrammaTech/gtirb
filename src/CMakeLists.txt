SET(PROJECT_NAME gtirb)

add_subdirectory(proto)

add_compile_options(-DGTIRB_${PROJECT_NAME}_EXPORTS)

if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
	#add_compile_options(-wd4251)  # Non-exportable template classes.
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
	#add_compile_options(-Wno-unused-function)
	add_compile_options(-mtune=generic)
	add_compile_options(-pthread)
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL Clang)
	#add_compile_options(-Wno-unused-function)
	add_compile_options(-mtune=generic)
	add_compile_options(-pthread)
endif()

include_directories("${CMAKE_SOURCE_DIR}/include/gtirb")

set(${PROJECT_NAME}_H
	../include/gtirb/Block.hpp
	../include/gtirb/ByteMap.hpp
	../include/gtirb/CFG.hpp
    ../include/gtirb/DataObject.hpp
	../include/gtirb/EA.hpp
	../include/gtirb/Export.hpp
	../include/gtirb/ImageByteMap.hpp
	../include/gtirb/Instruction.hpp
	../include/gtirb/IR.hpp
	../include/gtirb/Module.hpp
	../include/gtirb/Node.hpp
        ../include/gtirb/NodeRef.hpp
        ../include/gtirb/Section.hpp
	../include/gtirb/Symbol.hpp
        ../include/gtirb/SymbolSet.hpp
    ../include/gtirb/SymbolicExpression.hpp
    ../include/gtirb/SymbolicExpressionSet.hpp
    ../include/gtirb/Table.hpp
        ../src/Serialization.hpp
)

set(${PROJECT_NAME}_SRC
        Block.cpp
	ByteMap.cpp
	CFG.cpp
        DataObject.cpp
        EA.cpp
	ImageByteMap.cpp
	Instruction.cpp
	IR.cpp
	Module.cpp
	Node.cpp
        Section.cpp
        Serialization.cpp
	Symbol.cpp
        SymbolSet.cpp
        SymbolicExpression.cpp
        Table.cpp
)

IF(UNIX AND NOT WIN32)
	SET(SYSLIBS
		${CMAKE_DL_LIBS}
	)
ELSE()
	SET(SYSLIBS
	)
endif()

GTIRB_ADD_LIBRARY()

target_link_libraries(
	${PROJECT_NAME} 
	${SYSLIBS}
	${Boost_LIBRARIES}
        ${PROTOBUF_LIBRARIES}
        proto
	)

if(GTIRB_ENABLE_TESTS)
	add_subdirectory(test)
endif()
