find_program(PYTHON "python")

if (PYTHON)

  #set(SETUP_PY_IN "${CMAKE_CURRENT_SOURCE_DIR}/setup.py")
  #set(SETUP_PY    "${CMAKE_CURRENT_BINARY_DIR}/setup.py")
  #file(GLOB DEPS ${CMAKE_CURRENT_BINARY_DIR}/gtirb/*.py)
  #set(OUTPUT      "${CMAKE_CURRENT_BINARY_DIR}/build")

  file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/gtirb"
    DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/")
  
  #configure_file(${SETUP_PY_IN} ${SETUP_PY})
  
  #add_custom_command(OUTPUT ${OUTPUT}
  #  COMMAND ${PYTHON}
  #  ARGS setup.py build
  #  DEPENDS ${DEPS})
  #  DEPENDS ${OUTPUT})
  add_custom_target(pygtirb ALL DEPENDS _force_python_protos)

  add_custom_command(TARGET pygtirb
    COMMAND cp ${PROTO_PY_SOURCES} "${CMAKE_CURRENT_BINARY_DIR}/gtirb")

  #install(CODE "execute_process(COMMAND ${PYTHON} ${SETUP_PY} install)")
endif()
