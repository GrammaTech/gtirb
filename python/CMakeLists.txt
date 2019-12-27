configure_file(setup.py.in setup.py @ONLY)

if(NOT PYTHON)
  message(
    FATAL_ERROR
      "Python 3 installation not found.
Try giving -DPYTHON=... to CMake to specify what Python to use,
or give -DGTIRB_PY_API=OFF to CMake to disable Python support."
  )
endif()

# protobuf generated files
protobuf_generate_python(PROTO_PY_SOURCES ${PROTO_FILES})

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

install(
  CODE "execute_process(COMMAND ${PYTHON} setup.py install WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )"
)
