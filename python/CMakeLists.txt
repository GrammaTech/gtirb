configure_file(setup.py.in setup.py @ONLY)

# ---------------------------------------------------------------------------
# Building the gtirb protobuf files into python
# ---------------------------------------------------------------------------

set(PROTO_PY_DIR ${CMAKE_CURRENT_BINARY_DIR}/gtirb/proto)

# Rewrite the proto definitions to use paths when including other definitions.
# We do this instead of using the one-line configure_file() command so that the
# proto/*.proto files in the source directory do not need to be rewritten for
# the CL API.

foreach(GTIRB_PROTO_FILE ${GTIRB_PROTO_FILES})
  get_filename_component(PROTO_BASE ${GTIRB_PROTO_FILE} NAME_WE)
  set(PROTO_PY_OUT "${PROTO_PY_DIR}/${PROTO_BASE}.proto")
  file(READ ${GTIRB_PROTO_FILE} PROTO_DEF_STR)
  string(REGEX REPLACE "import \"" "import \"gtirb/proto/" PROTO_DEF_STR
                       "${PROTO_DEF_STR}"
  )
  file(WRITE ${PROTO_PY_OUT} "${PROTO_DEF_STR}")

  set(PROTO_PY_SOURCE "${PROTO_PY_DIR}/${PROTO_BASE}_pb2.py")
  add_custom_command(
    OUTPUT "${PROTO_PY_SOURCE}"
    COMMAND protobuf::protoc --python_out ${CMAKE_CURRENT_BINARY_DIR} -I
            "${CMAKE_CURRENT_BINARY_DIR}" ${PROTO_PY_OUT}
    DEPENDS ${PROTO_PY_OUT} protobuf::protoc
    VERBATIM
  )
  list(APPEND PROTO_PY_SOURCES "${PROTO_PY_SOURCE}")
endforeach()

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
install(
  SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/install.cmake
  COMPONENT python
  EXCLUDE_FROM_ALL
)

# Convenience targets for installing python
add_custom_target(
  install-python
  DEPENDS pygtirb
  COMMAND "${CMAKE_COMMAND}" -DCMAKE_INSTALL_COMPONENT=python -P
          "${CMAKE_BINARY_DIR}/cmake_install.cmake"
)
add_custom_target(
  python-wheel
  DEPENDS pygtirb
  COMMAND "${PYTHON}" setup.py sdist bdist_wheel
)
