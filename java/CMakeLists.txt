#
# Cmake configuration for java build
#

# ---------------------------------------------------------------------------
# Get protobuf version to DL from maven repository. This value is used in
# pom.xml.in.
# ---------------------------------------------------------------------------

# Use only minor version, patch versions might not be available
string(REGEX MATCH "([^\.]+)\.([^\.]+)\.([^\.]+)" PROTOBUF_VERSION_MATCH
             ${Protobuf_VERSION}
)
set(PROTOBUF_MAJOR_VERSION ${CMAKE_MATCH_1})
set(PROTOBUF_MINOR_VERSION ${CMAKE_MATCH_2})
set(JAVA_PROTOBUF_VERSION
    "${PROTOBUF_MAJOR_VERSION}.${PROTOBUF_MINOR_VERSION}.0"
)

# ---------------------------------------------------------------------------
# Building the gtirb protobuf files into java
# ---------------------------------------------------------------------------
set(GTIRB_JAVA_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/src)

# Directory containing the *.class files generated from the *.proto files
set(GTIRB_PROTO_CLASS_DIR "${GTIRB_JAVA_SOURCE_DIR}/com/grammatech/gtirb/proto")

# Add commands to compile the *.proto files to *.class files
foreach(GTIRB_PROTO_FILE ${GTIRB_PROTO_FILES})
  get_filename_component(GTIRB_PROTO_BASENAME ${GTIRB_PROTO_FILE} NAME_WE)
  set(GTIRB_PROTO_CLASS_FILE
      ${GTIRB_PROTO_CLASS_DIR}/${GTIRB_PROTO_BASENAME}OuterClass.java
  )
  add_custom_command(
    OUTPUT ${GTIRB_PROTO_CLASS_FILE}
    COMMAND ${Protobuf_PROTOC_EXECUTABLE} --java_out=${GTIRB_JAVA_SOURCE_DIR}
            --proto_path=${GTIRB_PROTO_DIR} ${GTIRB_PROTO_FILE}
  )
  list(APPEND GTIRB_PROTOBUF_JAVA ${GTIRB_PROTO_CLASS_FILE})
endforeach()

# ---------------------------------------------------------------------------
# Building the gtirb java API
# ---------------------------------------------------------------------------
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/pom.xml.in ${CMAKE_CURRENT_BINARY_DIR}/pom.xml
  @ONLY
)

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/com/grammatech/gtirb/
  DESTINATION ${GTIRB_JAVA_SOURCE_DIR}/com/grammatech/gtirb
  FILES_MATCHING
  PATTERN "*.java"
)

# ---------------------------------------------------------------------------
# Creating a version file to report version information
# ---------------------------------------------------------------------------
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/Version.java.in
  ${GTIRB_JAVA_SOURCE_DIR}/com/grammatech/gtirb/Version.java @ONLY
)

set(GTIRB_API_JAVAS
    Alignment
    AuxDataContainer
    AuxData
    AuxDataSerialization
    Block
    ByteBlock
    ByteInterval
    CFG
    CodeBlock
    Codec
    Comments
    DataBlock
    DecodeException
    Edge
    EncodeException
    FiveTuple
    FunctionBlocks
    FunctionEntries
    FunctionNames
    IntegerCodec
    IR
    MappingCodec
    Module
    Node
    OffsetCodec
    Offset
    Padding
    ParsedProduct
    ProxyBlock
    Section
    SequenceCodec
    Serialization
    SetCodec
    StreamSerialization
    StringCodec
    SymAddrAddr
    SymAddrConst
    SymbolForwarding
    SymbolicExpression
    Symbol
    ThreeTuple
    TreeListItem
    TreeListUtils
    TupleCodec
    TwoTuple
    Types
    UnknownCodecException
    UnknownData
    Util
    UuidCodec
    Version
)

foreach(GTIRB_API_JAVA ${GTIRB_API_JAVAS})
  list(
    APPEND GTIRB_API_SOURCES
           ${GTIRB_JAVA_SOURCE_DIR}/com/grammatech/gtirb/${GTIRB_API_JAVA}.java
  )
endforeach()

set(GTIRB_JAVA_API_VERSION
    "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}.${PROJECT_VERSION_PATCH}"
)
set(TARGET_DIR "${CMAKE_CURRENT_BINARY_DIR}/target")
set(GTIRB_JAVA_APIDOCS_DIR
    "${TARGET_DIR}/apidocs"
    PARENT_SCOPE
)

set(GTIRB_JAVA_API_JARS
    "${TARGET_DIR}/gtirb_api-${GTIRB_JAVA_API_VERSION}.jar"
    "${TARGET_DIR}/gtirb_api-${GTIRB_JAVA_API_VERSION}-sources.jar"
    "${TARGET_DIR}/gtirb_api-${GTIRB_JAVA_API_VERSION}-javadoc.jar"
)

add_custom_command(
  OUTPUT ${GTIRB_JAVA_API_JARS} ${GTIRB_JAVA_APIDOCS_DIR}/index.html
  COMMAND ${MVN} package
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  DEPENDS ${GTIRB_API_SOURCES} ${GTIRB_PROTOBUF_JAVA}
)

add_custom_target(gtirb-java-api ALL DEPENDS ${GTIRB_JAVA_API_JARS})

# ---------------------------------------------------------------------------
# Building the test code
# ---------------------------------------------------------------------------

if(GTIRB_ENABLE_TESTS)
  if(WIN32)
    set(DIR_SEP "\;")
  else()
    set(DIR_SEP ":")
  endif()

  execute_process(
    COMMAND ${MVN} -q exec:exec -Dexec.classpathScope=compile
            -Dexec.executable=echo -Dexec.args=%classpath
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    OUTPUT_VARIABLE JAR_FILE_LIST
  )
  string(STRIP ${JAR_FILE_LIST} JAR_FILE_LIST)

  file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tests/
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests
  )

  add_custom_command(
    TARGET gtirb-java-api
    POST_BUILD
    COMMAND ${Java_JAVAC_EXECUTABLE} -cp ${JAR_FILE_LIST} -d
            ${CMAKE_CURRENT_BINARY_DIR}
            ${CMAKE_CURRENT_BINARY_DIR}/tests/testIrSanity.java
    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/testIrSanity.class
    COMMENT "Building testIrSanity"
  )

  add_custom_command(
    TARGET gtirb-java-api
    POST_BUILD
    COMMAND ${Java_JAVAC_EXECUTABLE} -cp ${JAR_FILE_LIST} -d
            ${CMAKE_CURRENT_BINARY_DIR}
            ${CMAKE_CURRENT_BINARY_DIR}/tests/testByteIntervals.java
    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/testByteIntervals.class
    COMMENT "Building testByteIntervals"
  )

  add_custom_command(
    TARGET gtirb-java-api
    POST_BUILD
    COMMAND ${Java_JAVAC_EXECUTABLE} -cp ${JAR_FILE_LIST} -d
            ${CMAKE_CURRENT_BINARY_DIR}
            ${CMAKE_CURRENT_BINARY_DIR}/tests/testAuxData.java
    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/testAuxData.class
    COMMENT "Building testAuxData"
  )

  add_test(NAME JavaSanityTest
           COMMAND ${Java_JAVA_EXECUTABLE} -cp ".${DIR_SEP}${JAR_FILE_LIST}"
                   testIrSanity ${CMAKE_CURRENT_BINARY_DIR}/tests/test3.gtirb
  )

  add_test(
    NAME JavaByteIntervalsTest
    COMMAND ${Java_JAVA_EXECUTABLE} -cp ".${DIR_SEP}${JAR_FILE_LIST}"
            testByteIntervals ${CMAKE_CURRENT_BINARY_DIR}/tests/test3.gtirb
  )

  add_test(NAME JavaAuxDataTest
           COMMAND ${Java_JAVA_EXECUTABLE} -cp ".${DIR_SEP}${JAR_FILE_LIST}"
                   testAuxData ${CMAKE_CURRENT_BINARY_DIR}/tests/test3.gtirb
  )

  set_tests_properties(
    JavaSanityTest
    PROPERTIES PASS_REGULAR_EXPRESSION "test OK." JavaByteIntervalsTest
    PROPERTIES PASS_REGULAR_EXPRESSION "test OK." JavaAuxDataTest
    PROPERTIES PASS_REGULAR_EXPRESSION "test OK."
  )

endif()
