file(GLOB ProtoFiles "${CMAKE_CURRENT_SOURCE_DIR}/*.proto")
if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
    add_compile_options(-wd4146) # unary minus operator applied to unsigned type, result still unsigned
    add_compile_options(-wd4125) # decimal digit terminates octal escape sequence
    add_compile_options(-wd4456) # declaration of 'decl' hides previous local declaration
    add_compile_options(-wd4100) # unreferenced formal parameter
    add_compile_options("/FIgtirb/Export.hpp")
else()
    add_compile_options(-Wno-shadow)
    add_compile_options(-Wno-unused-parameter)
    add_compile_options(-include "${CMAKE_SOURCE_DIR}/include/gtirb/Export.hpp")
endif()
PROTOBUF_GENERATE_CPP(ProtoSources ProtoHeaders
                      EXPORT_MACRO "GTIRB_EXPORT_API"
                      ${ProtoFiles})

PROTOBUF_GENERATE_PYTHON(ProtoSourcesPy ${ProtoFiles})

set(PROTO_PY_SOURCES ${ProtoSourcesPy} CACHE INTERNAL "")
add_custom_target(_force_python_protos ALL DEPENDS ${ProtoSourcesPy})

add_library(proto STATIC ${ProtoSources} ${ProtoHeaders})

target_link_libraries(proto ${Boost_LIBRARIES} ${Protobuf_LIBRARIES})
target_compile_definitions(proto PRIVATE GTIRB_${PROJECT_NAME}_EXPORTS)
include_directories(SYSTEM "${PROTOBUF_INCLUDE_DIRS}")

install(TARGETS proto EXPORT gtirbTargets
  ARCHIVE DESTINATION lib LIBRARY DESTINATION lib)

add_custom_command(TARGET proto POST_BUILD
    COMMAND "${CMAKE_COMMAND}" -E make_directory "${CMAKE_BINARY_DIR}/include/proto")
add_custom_command(
        TARGET proto POST_BUILD
        COMMAND "${CMAKE_COMMAND}" -E copy
                ${ProtoHeaders}
                "${CMAKE_BINARY_DIR}/include/proto")
