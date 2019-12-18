if(PYTHON)
  find_program(SPHINX_APIDOC sphinx-apidoc)

  if(SPHINX_APIDOC)
    add_custom_target(
      sphinx
      DEPENDS pygtirb
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMAND
        ${SPHINX_APIDOC} ${CMAKE_BINARY_DIR}/python/gtirb --output-dir
        ${CMAKE_CURRENT_BINARY_DIR} --force --full --separate
      COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/conf.py
              ${CMAKE_CURRENT_BINARY_DIR}/
      COMMAND make html
      COMMENT "Generating API documentation with Sphinx"
      VERBATIM
    )
    add_dependencies(doc sphinx)
  else()
    message(
      WARNING
        "sphinx-apidoc was not found. HTML documentation for the Python API will not be generated."
    )
  endif()
endif()
