# GTIRB_PROTO_DIR
#
# Directory containing the default *.proto files.

set(GTIRB_PROTO_DIR
    ${CMAKE_CURRENT_SOURCE_DIR}
    PARENT_SCOPE
)

# GTIRB_PROTO_FILES
#
# List of *.proto definition files.

file(GLOB GTIRB_PROTO_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.proto)
set(GTIRB_PROTO_FILES
    ${GTIRB_PROTO_FILES}
    PARENT_SCOPE
)
