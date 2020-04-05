configure_file(setup.py.in setup.py @ONLY)

if(NOT PYTHON)
  message(
    FATAL_ERROR
      "Python 3 installation not found.
Try giving -DPYTHON=... to CMake to specify what Python to use,
or give -DGTIRB_PY_API=OFF to CMake to disable Python support."
  )
endif()

# ---------------------------------------------------------------------------
# Building the gtirb protobuf files into python
# ---------------------------------------------------------------------------

# Generate *.proto files in the currect submodule (DIRECTORY) and look for them
# there (PROTO_PATH).
gtirb_make_proto_files(
  GTIRB_PY_PROTO_FILES DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/gtirb/proto/
  PROTO_PATH gtirb/proto/
)

# Remove "gtirb/proto" from the include path
set(PROTOBUF_GENERATE_CPP_APPEND_PATH off)
# Add "." to the include path
set(Protobuf_IMPORT_DIRS "${CMAKE_CURRENT_BINARY_DIR}")
# Generate the python protobuf files
protobuf_generate_python(PROTO_PY_SOURCES ${GTIRB_PY_PROTO_FILES})

# existing Python files
file(GLOB PY_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/gtirb/*.py
     ${CMAKE_CURRENT_SOURCE_DIR}/tests/*.py
)

add_custom_target(pygtirb ALL DEPENDS ${PY_SOURCES} ${PROTO_PY_SOURCES})
add_custom_command(
  TARGET pygtirb
  COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_CURRENT_BINARY_DIR}/gtirb"
          "${CMAKE_CURRENT_BINARY_DIR}/tests"
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/gtirb"
          "${CMAKE_CURRENT_BINARY_DIR}/gtirb"
  COMMAND ${CMAKE_COMMAND} -E copy_directory
          "${CMAKE_CURRENT_SOURCE_DIR}/tests"
          "${CMAKE_CURRENT_BINARY_DIR}/tests"
)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/version.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/gtirb/version.py @ONLY
)

if(GTIRB_ENABLE_TESTS)
  add_test(
    NAME testgtirbpy
    COMMAND ${PYTHON} setup.py test
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/"
  )
endif()

# Move the `setup.py install` command into a separate script. This extra layer
# of indirection allows us to detect the value of ENV{DESTDIR} at installation
# time, rather than configure time, so that the installation will go to the
# correct directory.

configure_file(install.cmake.in install.cmake @ONLY)
install(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/install.cmake)
