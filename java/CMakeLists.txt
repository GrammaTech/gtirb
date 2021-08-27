#
# Cmake configuration for java build
#

include(UseJava)

# ---------------------------------------------------------------------------
# Getting dependencies (protobuf-java jar file)
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

file(
  DOWNLOAD
  https://repo1.maven.org/maven2/com/google/protobuf/protobuf-java/${JAVA_PROTOBUF_VERSION}/protobuf-java-${JAVA_PROTOBUF_VERSION}.jar
  ${CMAKE_CURRENT_BINARY_DIR}/protobuf-java-${JAVA_PROTOBUF_VERSION}.jar
)

# ---------------------------------------------------------------------------
# Building the gtirb protobuf files into java
# ---------------------------------------------------------------------------

# Directory containing the *.class files generated from the *.proto files
set(GTIRB_PROTO_CLASS_DIR
    "${CMAKE_CURRENT_BINARY_DIR}/com/grammatech/gtirb/proto"
)

# Add commands to compile the *.proto files to *.class files
foreach(GTIRB_PROTO_FILE ${GTIRB_PROTO_FILES})
  get_filename_component(GTIRB_PROTO_BASENAME ${GTIRB_PROTO_FILE} NAME_WE)
  set(GTIRB_PROTO_CLASS_FILE
      ${GTIRB_PROTO_CLASS_DIR}/${GTIRB_PROTO_BASENAME}OuterClass.java
  )
  add_custom_command(
    OUTPUT ${GTIRB_PROTO_CLASS_FILE}
    COMMAND ${Protobuf_PROTOC_EXECUTABLE}
            --java_out=${CMAKE_CURRENT_BINARY_DIR}
            --proto_path=${GTIRB_PROTO_DIR} ${GTIRB_PROTO_FILE}
  )
  list(APPEND GTIRB_PROTOBUF_JAVA ${GTIRB_PROTO_CLASS_FILE})
endforeach()

# ---------------------------------------------------------------------------
# Building the gtirb java API
# ---------------------------------------------------------------------------

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/com/grammatech/gtirb/
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/com/grammatech/gtirb
  FILES_MATCHING
  PATTERN "*.java"
)

# ---------------------------------------------------------------------------
# Creating a version file to report version information
# ---------------------------------------------------------------------------
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/Version.java.in
  ${CMAKE_CURRENT_BINARY_DIR}/com/grammatech/gtirb/Version.java @ONLY
)

set(GTIRB_API_JAVAS
    Alignment
    AuxDataContainer
    AuxData
    Block
    ByteBlock
    ByteInterval
    CFG
    CodeBlock
    Comments
    DataBlock
    Edge
    FiveTuple
    FunctionBlocks
    FunctionEntries
    FunctionNames
    IR
    Module
    Node
    Offset
    Padding
    ProxyBlock
    Section
    Serialization
    SymAddrAddr
    SymAddrConst
    SymbolForwarding
    SymbolicExpression
    Symbol
    ThreeTuple
    TreeListItem
    TreeListUtils
    TwoTuple
    Types
    Util
    Version
)

foreach(GTIRB_API_JAVA ${GTIRB_API_JAVAS})
  list(
    APPEND
      GTIRB_API_SOURCES
      ${CMAKE_CURRENT_BINARY_DIR}/com/grammatech/gtirb/${GTIRB_API_JAVA}.java
  )
endforeach()

set(GTIRB_API_JARFILE
    gtirb_api-${GTIRB_MAJOR_VERSION}.${GTIRB_MINOR_VERSION}.${GTIRB_PATCH_VERSION}
)
add_jar(${GTIRB_API_JARFILE} SOURCES ${GTIRB_API_SOURCES}
        ${GTIRB_PROTOBUF_JAVA} INCLUDE_JARS
        ${CMAKE_CURRENT_BINARY_DIR}/protobuf-java-${JAVA_PROTOBUF_VERSION}.jar
)

# ---------------------------------------------------------------------------
# Building the test code
# ---------------------------------------------------------------------------

if(GTIRB_ENABLE_TESTS)

  file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tests/
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests
  )

  if(WIN32)
    set(DIR_SEP "\;")
  else()
    set(DIR_SEP ":")
  endif()

  set(JAR_FILE_LIST
      "${GTIRB_API_JARFILE}.jar${DIR_SEP}protobuf-java-${JAVA_PROTOBUF_VERSION}.jar"
  )

  # Start work on a command to generate docs add_custom_command( OUTPUT docs
  # POST_BUILD COMMAND if [[ ! -d ${CMAKE_CURRENT_BINARY_DIR}/docs ]]; then
  # mkdir #{CMAKE_CURRENT_BINARY_DIR}/docs; fi COMMAND javadoc -d doc -cp
  # .:${JAR_FILE_LIST} com/grammatech/gtirb/*.java COMMENT "Creating docs" )

  add_custom_command(
    TARGET ${GTIRB_API_JARFILE}
    POST_BUILD
    COMMAND ${Java_JAVAC_EXECUTABLE} -cp ${JAR_FILE_LIST} -d
            ${CMAKE_CURRENT_BINARY_DIR}
            ${CMAKE_CURRENT_BINARY_DIR}/tests/testIrSanity.java
    COMMENT "Building testIrSanity"
  )

  add_custom_command(
    TARGET ${GTIRB_API_JARFILE}
    POST_BUILD
    COMMAND ${Java_JAVAC_EXECUTABLE} -cp ${JAR_FILE_LIST} -d
            ${CMAKE_CURRENT_BINARY_DIR}
            ${CMAKE_CURRENT_BINARY_DIR}/tests/testByteIntervals.java
    COMMENT "Building testByteIntervals"
  )

  add_test(NAME JavaSanityTest
           COMMAND ${Java_JAVA_EXECUTABLE} -cp ".${DIR_SEP}${JAR_FILE_LIST}"
                   testIrSanity ${CMAKE_CURRENT_BINARY_DIR}/tests/test2.gtirb
  )

  add_test(
    NAME JavaByteIntervalsTest
    COMMAND ${Java_JAVA_EXECUTABLE} -cp ".${DIR_SEP}${JAR_FILE_LIST}"
            testByteIntervals ${CMAKE_CURRENT_BINARY_DIR}/tests/test2.gtirb
  )

  set_tests_properties(
    JavaSanityTest
    PROPERTIES PASS_REGULAR_EXPRESSION "test OK." JavaByteIntervalsTest
    PROPERTIES PASS_REGULAR_EXPRESSION "test OK."
  )

endif()
