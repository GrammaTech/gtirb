configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/index.html
    ${CMAKE_CURRENT_BINARY_DIR}/index.html
    @ONLY
)

add_custom_target(doc DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/index.html)

add_subdirectory(examples)
add_subdirectory(doxy)
add_subdirectory(python)
