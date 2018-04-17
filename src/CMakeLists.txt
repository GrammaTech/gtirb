SET(PROJECT_NAME GTIRB)

add_compile_options(-DGTIRB_${PROJECT_NAME}_EXPORTS)

if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
	#add_compile_options(-wd4251)  # Non-exportable template classes.
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
	#add_compile_options(-Wno-unused-function)
	add_compile_options(-mtune=generic)
	add_compile_options(-pthread)
elseif(${CMAKE_CXX_COMPILEUR_ID} STREQUAL Clang)
	#add_compile_options(-Wno-unused-function)
	add_compile_options(-mtune=generic)
	add_compile_options(-pthread)
endif()

set(${PROJECT_NAME}_H
	../include/gtirb/AddrRanges.hpp
	../include/gtirb/Any.hpp
	../include/gtirb/Constants.hpp
	../include/gtirb/EA.hpp
	../include/gtirb/Enums.hpp
	../include/gtirb/Exception.hpp
	../include/gtirb/Export.hpp
	../include/gtirb/Instruction.hpp
	../include/gtirb/LocalProperties.hpp
	../include/gtirb/LogicError.hpp
	../include/gtirb/Module.hpp
	../include/gtirb/ModuleAux.hpp
	../include/gtirb/ModuleCore.hpp
	../include/gtirb/ModuleSectionBase.hpp
	../include/gtirb/ModuleSummary.hpp
	../include/gtirb/Node.hpp
	../include/gtirb/NodeError.hpp
	../include/gtirb/NodeStructureError.hpp
	../include/gtirb/NodeUtilities.hpp
	../include/gtirb/NodeValidators.hpp
	../include/gtirb/Procedure.hpp
	../include/gtirb/RuntimeError.hpp
	../include/gtirb/Table.hpp
	../include/gtirb/TableTemplate.hpp
	../include/gtirb/Utilities.hpp
)

set(${PROJECT_NAME}_SRC
	AddrRanges.cpp
	EA.cpp
	Exception.cpp
	Instruction.cpp
	LocalProperties.cpp
	LogicError.cpp
	Module.cpp
	ModuleAux.cpp
	ModuleCore.cpp
	ModuleSectionBase.cpp
	ModuleSummary.cpp
	Node.cpp
	NodeError.cpp
	NodeStructureError.cpp
	Procedure.cpp
	RuntimeError.cpp
	Utilities.cpp
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
	)