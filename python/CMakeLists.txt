find_program(PYTHON "python3")

if (PYTHON)
  file(GLOB DEPS ${CMAKE_CURRENT_SOURCE_DIR}/gtirb/*.py)

  add_custom_target(pygtirb ALL DEPENDS _force_python_protos)
  add_custom_command(TARGET pygtirb
    COMMAND cp -R "${CMAKE_CURRENT_SOURCE_DIR}/setup.py" "${CMAKE_CURRENT_SOURCE_DIR}/gtirb" "${CMAKE_CURRENT_SOURCE_DIR}/tests" "${CMAKE_CURRENT_BINARY_DIR}/")
  
  add_custom_target(pycheck
    TARGET check
    COMMAND ${PYTHON} setup.py test
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/"
    DEPENDS pygtirb)
  
  #install(CODE "execute_process(COMMAND ${PYTHON} ${SETUP_PY} install)")
endif()
