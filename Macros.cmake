macro(IMPL_GTIRB_ADD_LINKER_FLAG flag)
	if(NOT ${CMAKE_EXE_LINKER_FLAGS} MATCHES "(${flag}.*)")
		set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${flag}" CACHE STRING "Linker Flags" FORCE)
	endif()

	if(NOT ${CMAKE_SHARED_LINKER_FLAGS} MATCHES "(${flag}.*)")
		set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${flag}" CACHE STRING "Linker Flags" FORCE)
	endif()
endmacro()

macro(GTIRB_ADD_MSVC_LINKER_FLAG flag)
	if(MSVC)
		IMPL_GTIRB_ADD_LINKER_FLAG(${flag})
	endif()
endmacro()

macro(GTIRB_ADD_GCC_LINKER_FLAG flag)
	if(CMAKE_COMPILER_IS_GNUCXX)
		IMPL_GTIRB_ADD_LINKER_FLAG(${flag})
	endif()
endmacro()

macro(GTIRB_ADD_CLANG_LINKER_FLAG flag)
	if(${CMAKE_CXX_COMPILER_ID} STREQUAL Clang)
		IMPL_GTIRB_ADD_LINKER_FLAG(${flag})
	endif()
endmacro()

macro(GTIRB_ADD_LIBRARY)
	add_library(${PROJECT_NAME} SHARED
		${${PROJECT_NAME}_H}
		${${PROJECT_NAME}_SRC}
		${${PROJECT_NAME}_PROTO}
	)
	
	set_target_properties(${PROJECT_NAME} PROPERTIES FOLDER "gtirb")
endmacro()

macro(GTIRB_ADD_LIBRARY_STATIC)
	add_library(${PROJECT_NAME} STATIC 
		${${PROJECT_NAME}_H}
		${${PROJECT_NAME}_SRC}
	)
	
	set_target_properties(${PROJECT_NAME} PROPERTIES FOLDER "gtirb")
endmacro()

macro(GTIRB_ADD_LIBRARY_HEADERONLY)
	add_library(${PROJECT_NAME} INTERFACE)
	target_include_directories(${PROJECT_NAME} INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
endmacro()

macro(GTIRB_ADD_EXECUTABLE)
	add_executable(${PROJECT_NAME} 
		${${PROJECT_NAME}_H}
		${${PROJECT_NAME}_SRC}
	)
	set_target_properties(${PROJECT_NAME} PROPERTIES FOLDER "gtirb/applications")
endmacro()

macro(GTIRB_ADD_EXECUTABLE_GTEST)
	add_executable(${PROJECT_NAME} 
		${${PROJECT_NAME}_H}
		${${PROJECT_NAME}_SRC}
	)
	
	add_test(NAME ${PROJECT_NAME} COMMAND ${PROJECT_NAME})
	set_target_properties(${PROJECT_NAME} PROPERTIES FOLDER "gtirb/test")
endmacro()

MACRO(GTIRB_GET_ALL_SUBDIRS result curdir)
	FILE(GLOB children RELATIVE ${curdir} ${curdir}/*)
	SET(dirlist "")
	foreach(child ${children})
		IF(IS_DIRECTORY ${curdir}/${child})
			LIST(APPEND dirlist ${child})
		ENDIF()
	endforeach()
	SET(${result} ${dirlist})
ENDMACRO()

macro(GTIRB_ADD_ALL_SUBDIRS)
	GTIRB_GET_ALL_SUBDIRS(SUBDIRS ${CMAKE_CURRENT_SOURCE_DIR})
	foreach(subdir ${SUBDIRS})
		add_subdirectory(${subdir})
	endforeach()
endmacro()
