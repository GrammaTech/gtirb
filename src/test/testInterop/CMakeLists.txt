set(PROJECT_NAME test_floats)

set(${PROJECT_NAME}_H)
set(${PROJECT_NAME}_SRC test_floats.cpp)

add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_H} ${${PROJECT_NAME}_SRC})
set_target_properties(${PROJECT_NAME} PROPERTIES FOLDER "gtirb/test")

target_link_libraries(${PROJECT_NAME} gtirb)

set(${PROJECT_NAME}_SCRIPTS test_floats.py)
set(test_floats ${CMAKE_BINARY_DIR}/bin/test_floats)

foreach(script_name ${${PROJECT_NAME}_SCRIPTS})
  configure_file(
    ${script_name} ${CMAKE_CURRENT_BINARY_DIR}/${script_name} @ONLY
  )
endforeach()
