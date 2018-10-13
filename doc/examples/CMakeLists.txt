# Find protobuf generated headers in the build directory
include_directories("${CMAKE_BINARY_DIR}/src/")

add_executable(ex-data-symbols data-symbols.cpp)
target_link_libraries(ex-data-symbols gtirb)

add_executable(ex-cfg-paths cfg-paths.cpp)
target_link_libraries(ex-cfg-paths gtirb)

add_executable(ex-functions functions.cpp)
target_link_libraries(ex-functions gtirb)

find_library(CAPSTONE NAMES capstone)
if(CAPSTONE)
  add_executable(ex-jumps jumps.cpp)
  target_link_libraries(ex-jumps gtirb ${CAPSTONE})
endif()
