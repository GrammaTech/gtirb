set(PROJECT_NAME testAuxDataInterop)

add_test(
  NAME interop_test
  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test-interop.sh
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
