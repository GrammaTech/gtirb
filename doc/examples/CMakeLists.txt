include_directories("${CMAKE_SOURCE_DIR}/include")
# Find protobuf generated headers in the build directory
include_directories("${CMAKE_BINARY_DIR}/src/")
# Find our version.h header file
include_directories("${CMAKE_BINARY_DIR}/include")

# Suppress warning from protobuf headers.
if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
  add_compile_options(-wd4100)
endif()

add_executable(ex-data-symbols data-symbols.cpp)
target_link_libraries(ex-data-symbols gtirb)

add_executable(ex-cfg-paths cfg-paths.cpp)
target_link_libraries(ex-cfg-paths gtirb)

add_executable(ex-functions functions.cpp)
target_link_libraries(ex-functions gtirb proto)

find_library(CAPSTONE NAMES capstone)
if(CAPSTONE)
  add_executable(ex-jumps jumps.cpp)
  target_link_libraries(ex-jumps gtirb ${CAPSTONE})
endif()

macro(move_filename file_list newdir)
  foreach(src_file ${${file_list}})
    get_filename_component(src_file_name "${src_file}" NAME)
    list(REMOVE_ITEM ${file_list} "${src_file}")
    list(APPEND ${file_list} "${newdir}/${src_file_name}")
  endforeach()
endmacro()

set(EXAMPLE_FILES_IN
    "${CMAKE_CURRENT_SOURCE_DIR}/cfg-paths.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/cfg-paths.py"
    "${CMAKE_CURRENT_SOURCE_DIR}/cfg-paths.lisp"
    "${CMAKE_CURRENT_SOURCE_DIR}/cfgpaths.java"
    "${CMAKE_CURRENT_SOURCE_DIR}/data-symbols.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/data-symbols.py"
    "${CMAKE_CURRENT_SOURCE_DIR}/data-symbols.lisp"
    "${CMAKE_CURRENT_SOURCE_DIR}/datasymbols.java"
    "${CMAKE_CURRENT_SOURCE_DIR}/functions.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/jumps.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/show-cfg.py"
    "${CMAKE_CURRENT_SOURCE_DIR}/show-cfg.lisp"
)

set(EXAMPLE_FILES ${EXAMPLE_FILES_IN})
move_filename(EXAMPLE_FILES ${CMAKE_CURRENT_BINARY_DIR})

foreach(_inmd ${EXAMPLE_FILES_IN})
  get_filename_component(_outmd "${_inmd}" NAME)
  add_custom_command(
    OUTPUT ${_outmd}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${_inmd}
    COMMAND ${CMAKE_COMMAND} -E copy ${_inmd} "${_outmd}"
    COMMENT "copying ${_outmd}"
    VERBATIM
  )
endforeach()

add_custom_target(
  doc_examples
  DEPENDS ${EXAMPLE_FILES}
  COMMENT "copying examples to output"
  VERBATIM
)

add_dependencies(doc doc_examples)
