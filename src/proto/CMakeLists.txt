file(GLOB ProtoFiles "${CMAKE_CURRENT_SOURCE_DIR}/*.proto")
PROTOBUF_GENERATE_CPP(ProtoSources ProtoHeaders ${ProtoFiles})
add_library(proto STATIC ${ProtoSources} ${ProtoHeaders})
target_link_libraries(proto ${Boost_LIBRARIES} ${PROTOBUF_LIBRARY})

# Install includes
add_custom_command(TARGET proto POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/../include/proto)
add_custom_command(
        TARGET proto POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
                ${ProtoHeaders}
                ${CMAKE_BINARY_DIR}/../include/proto)
