if(GTIRB_ENABLE_MYPY)
  find_program(MYPY mypy)
  find_program(MYPY_PROTOBUF protoc-gen-mypy)
  if(MYPY AND MYPY_PROTOBUF)
    set(ENABLE_MYPY ON)
    set(MYPY_PROTOBUF_FLAGS --mypy_out=${CMAKE_CURRENT_BINARY_DIR})
    configure_file(mypy.ini.in mypy.ini @ONLY)
  else()
    set(ENABLE_MYPY OFF)
    if(NOT MYPY)
      message(
        WARNING "mypy not found. Type checking with mypy will be disabled."
      )
    endif()
    if(NOT MYPY_PROTOBUF)
      message(
        WARNING
          "protoc-gen-mypy not found. Type checking with mypy will be disabled."
      )
    endif()
  endif()
endif()

# Create a PEP 561 py.typed to let clients find the inline type annotations.
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/gtirb/py.typed "")

if("${Protobuf_VERSION}" VERSION_LESS "3.19.0")
  # Older protobuf compilers generate code incompatible with newer runtimes.
  set(GTIRB_PROTOBUF_CONSTRAINT "protobuf >= ${Protobuf_VERSION}, < 4.0")
else()
  set(GTIRB_PROTOBUF_CONSTRAINT "protobuf >= 4.0")
endif()

configure_file(pyproject.toml.in pyproject.toml @ONLY)
configure_file(README.md README COPYONLY)
configure_file(requirements-dev.txt requirements-dev.txt COPYONLY)
configure_file(requirements-mypy.txt requirements-mypy.txt COPYONLY)
configure_file(tox.ini tox.ini COPYONLY)
# Copy the license from the top-level of the source repo.
configure_file(${CMAKE_SOURCE_DIR}/LICENSE.txt LICENSE COPYONLY)

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
    COMMAND
      protobuf::protoc --python_out ${CMAKE_CURRENT_BINARY_DIR}
      ${MYPY_PROTOBUF_FLAGS} -I "${CMAKE_CURRENT_BINARY_DIR}" ${PROTO_PY_OUT}
    DEPENDS ${PROTO_PY_OUT} protobuf::protoc
    VERBATIM
  )
  list(APPEND PROTO_PY_SOURCES "${PROTO_PY_SOURCE}")
endforeach()
add_custom_target(py-proto DEPENDS ${PROTO_PY_SOURCES})

# existing Python files
file(GLOB PY_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/gtirb/*.py
     ${CMAKE_CURRENT_SOURCE_DIR}/tests/*.py
)

add_custom_target(pygtirb ALL DEPENDS ${PY_SOURCES} py-proto)
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

if(GTIRB_RELEASE_VERSION)
  set(GTIRB_PYTHON_DEV_SUFFIX "")
else()
  set(GTIRB_PYTHON_DEV_SUFFIX ".dev")
endif()

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/version.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/gtirb/version.py @ONLY
)

if(GTIRB_ENABLE_TESTS)
  find_program(TOX tox)
  if(TOX)
    if(ENABLE_MYPY)
      # Only run mypy if we actually generated stubs for the protobuf
      # definitions.
      add_test(
        NAME mypy
        COMMAND tox run -e mypy
        WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/"
      )
    endif()

    if(ENABLE_CODE_COVERAGE)
      set(GTIRB_TOX_FLAGS)
    else()
      set(GTIRB_TOX_FLAGS "--")
    endif()
    execute_process(
      COMMAND ${TOX} -l
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      OUTPUT_VARIABLE TOX_ENVIRONMENTS
    )
    string(REGEX MATCHALL "py3[0-9a-z-]*" TOX_ENVIRONMENTS ${TOX_ENVIRONMENTS})
    foreach(TOX_ENVIRONMENT ${TOX_ENVIRONMENTS})
      add_test(
        NAME ${TOX_ENVIRONMENT}
        COMMAND ${TOX} run -e ${TOX_ENVIRONMENT} ${GTIRB_TOX_FLAGS}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      )
    endforeach()
  else()
    message(SEND_ERROR "Python tests require tox to be installed")
  endif()
endif()

# Convenience targets for installing python
add_custom_target(
  python-wheel
  DEPENDS pygtirb
  COMMAND "${PYTHON}" -m pip wheel --no-deps "${CMAKE_CURRENT_BINARY_DIR}"
)
