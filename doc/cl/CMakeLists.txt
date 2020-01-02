set(SDT_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/write-documentation.lisp)
set(HTML_INDEX ${CMAKE_CURRENT_BINARY_DIR}/index.html)

file(GLOB DOCUMENTABLE_FILES ${CMAKE_SOURCE_DIR}/cl/*.lisp
     ${CMAKE_SOURCE_DIR}/cl/*.asd ${CMAKE_SOURCE_DIR}/cl/README.md
)

add_custom_command(
  OUTPUT ${HTML_INDEX}
  DEPENDS ${SDT_SCRIPT} ${DOCUMENTABLE_FILES}
  COMMAND ${LISP} --script ${SDT_SCRIPT} ${DOCUMENTABLE_FILES} ${HTML_INDEX}
)
add_custom_target(sdt ALL DEPENDS ${HTML_INDEX})
add_dependencies(doc sdt)
