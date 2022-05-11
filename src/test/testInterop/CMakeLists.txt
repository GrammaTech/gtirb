set(PROJECT_NAME testInterop)

add_executable(test_floats test_floats.cpp)
add_executable(test_variants test_variants.cpp)
set_target_properties(test_floats PROPERTIES FOLDER "gtirb/test/testInterop")
set_target_properties(test_variants PROPERTIES FOLDER "gtirb/test/testInterop")

target_link_libraries(test_floats gtirb)
target_link_libraries(test_variants gtirb)

set(${PROJECT_NAME}_SCRIPTS test_floats.py test_variants.py)
set(test_floats ${CMAKE_BINARY_DIR}/bin/test_floats)

foreach(script_name ${${PROJECT_NAME}_SCRIPTS})
  configure_file(
    ${script_name} ${CMAKE_CURRENT_BINARY_DIR}/${script_name} @ONLY
  )
endforeach()
