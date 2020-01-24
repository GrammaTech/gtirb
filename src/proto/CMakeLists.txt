if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
  add_compile_options(-wd4146) # unary minus operator applied to unsigned type,
                               # result still unsigned
  add_compile_options(-wd4125) # decimal digit terminates octal escape sequence
  add_compile_options(-wd4456) # declaration of 'decl' hides previous local
                               # declaration
  add_compile_options(-wd4100) # unreferenced formal parameter
  add_compile_options(-wd4267) # conversion from 'type1' to 'type2', possible
                               # loss of data
  add_compile_options(-wd4244) # possible loss of data in conversion
  add_compile_options(-wd4127) # conditional expression is constant
  add_compile_options(
    -wd4251
  ) # 'identifier' : class 'type' needs to have dll- interface to be used by
    # clients of class 'type2'
  add_compile_options(-wd4275) # Non-dll interface base classes.
  add_compile_options("/FI${CMAKE_SOURCE_DIR}/include/gtirb/Export.hpp")
else()
  add_compile_options(-Wno-shadow)
  add_compile_options(-Wno-unused-parameter)
  add_compile_options(-include "${CMAKE_SOURCE_DIR}/include/gtirb/Export.hpp")
endif()

protobuf_generate_cpp(
  PROTO_CPP_SOURCES PROTO_CPP_HEADERS EXPORT_MACRO "GTIRB_EXPORT_API"
  ${PROTO_FILES}
)

add_library(proto STATIC ${PROTO_CPP_SOURCES} ${PROTO_CPP_HEADERS})

target_link_libraries(proto ${Boost_LIBRARIES} ${Protobuf_LIBRARIES})
target_compile_definitions(proto PRIVATE GTIRB_${PROJECT_NAME}_EXPORTS)
include_directories(SYSTEM "${PROTOBUF_INCLUDE_DIRS}")

install(
  TARGETS proto
  EXPORT gtirbTargets
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)

add_custom_command(
  TARGET proto
  POST_BUILD
  COMMAND "${CMAKE_COMMAND}" -E make_directory
          "${CMAKE_BINARY_DIR}/include/proto"
)
add_custom_command(
  TARGET proto
  POST_BUILD
  COMMAND "${CMAKE_COMMAND}" -E copy ${PROTO_CPP_HEADERS}
          "${CMAKE_BINARY_DIR}/include/proto"
)
