SET(PROJECT_NAME GTIRB)

add_compile_options(-DGTIRB_${PROJECT_NAME}_EXPORTS)

# Required warning suppression (TODO: Remove!)
if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
	#add_compile_options(-wd4251)  # Non-exportable template classes.
elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
	#add_compile_options(-Wno-uninitialized)
	#add_compile_options(-Wno-unused-variable)
	add_compile_options(-Wno-unused-function)
	#add_compile_options(-Wno-maybe-uninitialized)
	#add_compile_options(-Wno-sign-compare)
	#add_compile_options(-Wno-unused-local-typedefs)
	#add_compile_options(-Wno-strict-aliasing)
	add_compile_options(-mtune=generic)
	add_compile_options(-pthread)
elseif(${CMAKE_CXX_COMPILEUR_ID} STREQUAL Clang)
	#add_compile_options(-Wno-unused-variable)
	add_compile_options(-Wno-unused-function)
	#add_compile_options(-Wno-uninitialized)
	#add_compile_options(-Wno-maybe-uninitialized)
	#add_compile_options(-Wno-sign-compare)
	#add_compile_options(-Wno-strict-aliasing)
	#add_compile_options(-Wno-unused-local-typedefs)
	add_compile_options(-mtune=generic)
	add_compile_options(-pthread)
endif()

set(${PROJECT_NAME}_H
	../include/gtirb/Exception.hpp
	../include/gtirb/Export.hpp
	../include/gtirb/LogicError.hpp
	../include/gtirb/NodeError.hpp
	../include/gtirb/NodeStructureError.hpp
	../include/gtirb/RuntimeError.hpp
)

set(${PROJECT_NAME}_SRC
	Exception.cpp
	NodeError.cpp
	NodeStructureError.cpp
	LogicError.cpp
	RuntimeError.cpp
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
	)