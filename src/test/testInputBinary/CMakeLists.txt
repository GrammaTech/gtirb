# Generate several projects of increasing size and complexity for use in testing
# GTIRB.
macro(GTIRB_BUILD_GTBINARY_MACRO)
  set(PROJECT_NAME gtirbTestInputBinary${_GENERATION_CONSTANT})

  set(${PROJECT_NAME}_H)

  set(${PROJECT_NAME}_SRC TestInputBinary.cpp)

  if(UNIX AND NOT WIN32)
    set(SYSLIBS ${CMAKE_DL_LIBS})
  else()
    set(SYSLIBS)
  endif()

  if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
    add_compile_options(-EHsc)
  elseif((${CMAKE_CXX_COMPILER_ID} STREQUAL GNU) OR (${CMAKE_CXX_COMPILER_ID}
                                                     STREQUAL Clang)
  )
    add_compile_options(-fexceptions)
  endif()

  add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_H} ${${PROJECT_NAME}_SRC})

  set_property(TARGET ${PROJECT_NAME} PROPERTY FOLDER "gtirb/test")

  target_compile_definitions(
    ${PROJECT_NAME} PRIVATE -DGENERATION_CONSTANT=${_GENERATION_CONSTANT}
  )

  target_link_libraries(${PROJECT_NAME} ${SYSLIBS})
endmacro()

set(_GENERATION_CONSTANT 2)
gtirb_build_gtbinary_macro()

set(_GENERATION_CONSTANT 8)
gtirb_build_gtbinary_macro()

set(_GENERATION_CONSTANT 16)
gtirb_build_gtbinary_macro()

set(_GENERATION_CONSTANT 32)
gtirb_build_gtbinary_macro()

set(_GENERATION_CONSTANT 64)
gtirb_build_gtbinary_macro()

# set(_GENERATION_CONSTANT 128) GTIRB_BUILD_GTBINARY_MACRO()

# set(_GENERATION_CONSTANT 256) GTIRB_BUILD_GTBINARY_MACRO()

# set(_GENERATION_CONSTANT 512) GTIRB_BUILD_GTBINARY_MACRO()
