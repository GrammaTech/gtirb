macro(IMPL_GTIRB_ADD_LINKER_FLAG flag)
  if(NOT ${CMAKE_EXE_LINKER_FLAGS} MATCHES "(${flag}.*)")
    set(CMAKE_EXE_LINKER_FLAGS
        "${CMAKE_EXE_LINKER_FLAGS} ${flag}"
        CACHE STRING "Linker Flags" FORCE
    )
  endif()

  if(NOT ${CMAKE_SHARED_LINKER_FLAGS} MATCHES "(${flag}.*)")
    set(CMAKE_SHARED_LINKER_FLAGS
        "${CMAKE_SHARED_LINKER_FLAGS} ${flag}"
        CACHE STRING "Linker Flags" FORCE
    )
  endif()
endmacro()

macro(GTIRB_ADD_MSVC_LINKER_FLAG flag)
  if(MSVC)
    impl_gtirb_add_linker_flag(${flag})
  endif()
endmacro()

macro(GTIRB_ADD_GCC_LINKER_FLAG flag)
  if(CMAKE_COMPILER_IS_GNUCXX)
    impl_gtirb_add_linker_flag(${flag})
  endif()
endmacro()

macro(GTIRB_ADD_CLANG_LINKER_FLAG flag)
  if(${CMAKE_CXX_COMPILER_ID} STREQUAL Clang)
    impl_gtirb_add_linker_flag(${flag})
  endif()
endmacro()

macro(GTIRB_ADD_LIBRARY)
  add_library(
    ${PROJECT_NAME} ${${PROJECT_NAME}_H} ${${PROJECT_NAME}_SRC}
                    ${${PROJECT_NAME}_PROTO}
  )

  set_target_properties(${PROJECT_NAME} PROPERTIES FOLDER "gtirb")
endmacro()

macro(GTIRB_ADD_LIBRARY_STATIC)
  add_library(
    ${PROJECT_NAME} STATIC ${${PROJECT_NAME}_H} ${${PROJECT_NAME}_SRC}
  )

  set_target_properties(${PROJECT_NAME} PROPERTIES FOLDER "gtirb")
endmacro()

macro(GTIRB_ADD_LIBRARY_HEADERONLY)
  add_library(${PROJECT_NAME} INTERFACE)
  target_include_directories(
    ${PROJECT_NAME} INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}
  )
endmacro()

macro(GTIRB_ADD_EXECUTABLE)
  add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_H} ${${PROJECT_NAME}_SRC})
  set_target_properties(${PROJECT_NAME} PROPERTIES FOLDER "gtirb/applications")
endmacro()

macro(GTIRB_ADD_EXECUTABLE_GTEST)
  add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_H} ${${PROJECT_NAME}_SRC})

  add_test(NAME ${PROJECT_NAME} COMMAND ${PROJECT_NAME})
  set_target_properties(${PROJECT_NAME} PROPERTIES FOLDER "gtirb/test")
endmacro()

macro(GTIRB_GET_ALL_SUBDIRS result curdir)
  file(
    GLOB children
    RELATIVE ${curdir}
    ${curdir}/*
  )
  set(dirlist "")
  foreach(child ${children})
    if(IS_DIRECTORY ${curdir}/${child})
      list(APPEND dirlist ${child})
    endif()
  endforeach()
  set(${result} ${dirlist})
endmacro()

macro(GTIRB_ADD_ALL_SUBDIRS)
  gtirb_get_all_subdirs(SUBDIRS ${CMAKE_CURRENT_SOURCE_DIR})
  foreach(subdir ${SUBDIRS})
    add_subdirectory(${subdir})
  endforeach()
endmacro()

# Provide a vaguely consistent interface to find a Python 3 interpreter. Just
# use FindPython3 if it exists, but fall back to looking for the interpreter
# program if we have to.
if(${CMAKE_VERSION} VERSION_GREATER_EQUAL "3.12")
  macro(GTIRB_FIND_PYTHON)
    find_package(Python3 REQUIRED COMPONENTS Interpreter)
    if(Python3_EXECUTABLE)
      set(PYTHON ${Python3_EXECUTABLE})
    endif()
  endmacro()
else()
  macro(GTIRB_FIND_PYTHON)
    find_program(PYTHON NAMES python3 python py)
    if(PYTHON)
      execute_process(
        COMMAND "${PYTHON}" --version OUTPUT_VARIABLE Python3_VERSION
      )
      string(REPLACE "Python " "" Python3_VERSION "${Python3_VERSION}")
      if("${Python3_VERSION}" VERSION_LESS 3)
        unset(PYTHON)
        unset(Python3_VERSION)
      endif()
    endif()
  endmacro()
endif()
