file(GLOB ProtoFiles "${CMAKE_CURRENT_SOURCE_DIR}/*.proto")
if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
    add_compile_options(-wd4146) # unary minus operator applied to unsigned type, result still unsigned
    add_compile_options(-wd4125) # decimal digit terminates octal escape sequence
    add_compile_options(-wd4456) # declaration of 'decl' hides previous local declaration
    add_compile_options(-wd4100) # unreferenced formal parameter
else()
    add_compile_options(-Wno-shadow)
endif()
PROTOBUF_GENERATE_CPP(ProtoSources ProtoHeaders ${ProtoFiles})
add_library(proto STATIC ${ProtoSources} ${ProtoHeaders})
target_link_libraries(proto ${Boost_LIBRARIES} ${PROTOBUF_LIBRARY})

# Install includes
add_custom_command(TARGET proto POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_SOURCE_DIR}/include/proto)
add_custom_command(
        TARGET proto POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
                ${ProtoHeaders}
                ${CMAKE_SOURCE_DIR}/include/proto)
