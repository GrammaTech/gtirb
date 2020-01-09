#
# Cmake configuration for java build
#
if(NOT Java_FOUND)
  message(
    FATAL_ERROR
      "Java installation not found.
You may set the variable JAVA_HOME to explicitly specify an installation.
If you do not have Java installed, use -DGTIRB_JAVA_API=OFF when invoking
CMake to disable the Java API build."
  )
endif()

include(UseJava)

# ---------------------------------------------------------------------------
# Getting dependencies (protobuf-java jar file)
# ---------------------------------------------------------------------------

add_custom_command(
  OUTPUT protobuf-java-3.0.0.jar
  COMMAND
    wget --directory-prefix=${CMAKE_CURRENT_BINARY_DIR}
    https://repo1.maven.org/maven2/com/google/protobuf/protobuf-java/3.0.0/protobuf-java-3.0.0.jar
)

# ---------------------------------------------------------------------------
# Building the gtirb protobuf files into java
# ---------------------------------------------------------------------------

set(GTIRB_PROTO_FILES
    AuxData
    ByteInterval
    CFG
    CodeBlock
    DataBlock
    IR
    Module
    Offset
    ProxyBlock
    Section
    SymbolicExpression
    Symbol
)

foreach(GTIRB_PROTO_FILE ${GTIRB_PROTO_FILES})
  add_custom_command(
    OUTPUT
      ${CMAKE_CURRENT_SOURCE_DIR}/com/grammatech/gtirb/proto/${GTIRB_PROTO_FILE}OuterClass.java
    COMMAND
      ${Protobuf_PROTOC_EXECUTABLE} --java_out=${CMAKE_CURRENT_SOURCE_DIR}
      --proto_path=${PROJECT_SOURCE_DIR}/proto
      ${PROJECT_SOURCE_DIR}/proto/${GTIRB_PROTO_FILE}.proto
  )
  list(
    APPEND
      GTIRB_PROTOBUF_JAVA
      ${CMAKE_CURRENT_SOURCE_DIR}/com/grammatech/gtirb/proto/${GTIRB_PROTO_FILE}OuterClass.java
  )
endforeach()

set(GTIRB_PROTOBUF_JARFILE
    gtirb_protobufs-${GTIRB_MAJOR_VERSION}.${GTIRB_MINOR_VERSION}.${GTIRB_PATCH_VERSION}
)
add_jar(${GTIRB_PROTOBUF_JARFILE} SOURCES ${GTIRB_PROTOBUF_JAVA} INCLUDE_JARS
        ${CMAKE_CURRENT_BINARY_DIR}/protobuf-java-3.0.0.jar
)

# ---------------------------------------------------------------------------
# Building the gtirb java API
# ---------------------------------------------------------------------------

set(GTIRB_API_JAVAS
    AuxData
    Block
    ByteInterval
    CodeBlock
    DataBlock
    IR
    # Kind
    Module
    Node
    ProxyBlock
    Section
    Serialization
    Symbol
    Util
)

foreach(GTIRB_API_JAVA ${GTIRB_API_JAVAS})
  list(
    APPEND
      GTIRB_API_SOURCES
      ${CMAKE_CURRENT_SOURCE_DIR}/com/grammatech/gtirb/${GTIRB_API_JAVA}.java
  )
endforeach()

set(GTIRB_API_JARFILE
    gtirb_api-${GTIRB_MAJOR_VERSION}.${GTIRB_MINOR_VERSION}.${GTIRB_PATCH_VERSION}
)
add_jar(${GTIRB_API_JARFILE} SOURCES ${GTIRB_API_SOURCES} INCLUDE_JARS
        ${GTIRB_PROTOBUF_JARFILE}
        ${CMAKE_CURRENT_BINARY_DIR}/protobuf-java-3.0.0.jar
)

# ---------------------------------------------------------------------------
# Building the test code
# ---------------------------------------------------------------------------

set(JAR_FILE_LIST
    ${GTIRB_PROTOBUF_JARFILE}.jar:${GTIRB_API_JARFILE}.jar:protobuf-java-3.0.0.jar
)

add_custom_command(
  TARGET ${GTIRB_API_JARFILE}
  POST_BUILD
  COMMAND ${Java_JAVAC_EXECUTABLE} -cp ${JAR_FILE_LIST} -d
          ${CMAKE_CURRENT_BINARY_DIR}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/testIrSanity.java
  COMMENT "Building testIrSanity"
)

# ---------------------------------------------------------------------------
# Test Cases
# ---------------------------------------------------------------------------

add_test(NAME JavaSanityTest
         COMMAND ${Java_JAVA_EXECUTABLE} -cp .:${JAR_FILE_LIST} testIrSanity
                 ${CMAKE_CURRENT_SOURCE_DIR}/tests/testIr.gtirb
)

set_tests_properties(
  JavaSanityTest PROPERTIES PASS_REGULAR_EXPRESSION "test OK."
)
