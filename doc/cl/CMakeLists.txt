set(SDT_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/write-documentation.lisp)
set(README ${CMAKE_SOURCE_DIR}/cl/README.md)
set(HTML_INDEX ${CMAKE_BINARY_DIR}/doc/html/cl/index.html)

file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/doc/html/cl")
file(GLOB CL_SOURCES ${CMAKE_SOURCE_DIR}/cl/*.lisp ${CMAKE_SOURCE_DIR}/cl/*.asd)

add_custom_command(
  OUTPUT ${HTML_INDEX}
  DEPENDS ${SDT_SCRIPT} ${README} ${CL_SOURCES}
  COMMAND
    ${LISP} --noinform --dynamic-space-size 16384 --no-userinit --no-sysinit
    --disable-debugger --load ${QUICKLISP}/setup.lisp --eval
    "(asdf:initialize-source-registry `(:source-registry (:tree \"${CMAKE_SOURCE_DIR}/cl\") :inherit-configuration))"
    --script ${SDT_SCRIPT} gtirb ${README} ${HTML_INDEX}
  VERBATIM
  COMMENT "Generating API documentation with SDT"
)
add_custom_target(sdt ALL DEPENDS ${HTML_INDEX})
add_dependencies(doc sdt)
