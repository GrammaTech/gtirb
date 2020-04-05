file(GLOB PROTO_IN_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.proto.in)
set(PROTO_IN_FILES
    ${PROTO_IN_FILES}
    PARENT_SCOPE
)

# gtirb_make_proto_files(var [DIRECTORY install-dir] [PROTO_PATH include-dir])
#
# Generate *.proto files from the *.proto.in definitions.
#
# :param var:          name of variable to contain paths to generated files
#
# :param install-dir:  directory to install generated files into. Defaults to
# ${CMAKE_CURRENT_BINARY_DIR}.
#
# :param include-dir:  path to use when including proto files from one another.
# Defaults to "".

function(GTIRB_MAKE_PROTO_FILES VAR)
  cmake_parse_arguments(GTIRB_MAKE_PROTO "" "DIRECTORY;PROTO_PATH" "" ${ARGN})

  if(NOT GTIRB_MAKE_PROTO_DIRECTORY)
    set(GTIRB_MAKE_PROTO_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
  endif()
  if(GTIRB_MAKE_PROTO_PROTO_PATH)
    set(GTIRB_PROTO_PATH ${GTIRB_MAKE_PROTO_PROTO_PATH})
  else()
    set(GTIRB_PROTO_PATH "")
  endif()

  message(STATUS "generating proto files in ${GTIRB_MAKE_PROTO_DIRECTORY}")
  set(PROTO_FILES)
  foreach(PROTO_IN_FILE ${PROTO_IN_FILES})
    get_filename_component(PROTO_FILE ${PROTO_IN_FILE} NAME_WE)
    set(PROTO_FILE "${GTIRB_MAKE_PROTO_DIRECTORY}/${PROTO_FILE}.proto")
    configure_file(${PROTO_IN_FILE} ${PROTO_FILE} @ONLY)
    list(APPEND PROTO_FILES ${PROTO_FILE})
  endforeach()
  set(${VAR}
      ${PROTO_FILES}
      PARENT_SCOPE
  )
endfunction()

# Most APIs use the default-generated files. Generate them once here so those
# APIs don't need to regenerate the files themselves.
gtirb_make_proto_files(GTIRB_PROTO_FILES)

# GTIRB_PROTO_FILES
#
# Names of the default *.proto files.

set(GTIRB_PROTO_FILES
    ${GTIRB_PROTO_FILES}
    PARENT_SCOPE
)

# GTIRB_PROTO_DIR
#
# Directory containing the default *.proto files.

set(GTIRB_PROTO_DIR
    ${CMAKE_CURRENT_BINARY_DIR}
    PARENT_SCOPE
)
