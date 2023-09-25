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
if(GTIRB_RELEASE_VERSION)
  set(GTIRB_JAVA_SNAPSHOT_SUFFIX "")
else()
  set(GTIRB_JAVA_SNAPSHOT_SUFFIX "-SNAPSHOT")
endif()

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
    auxdatacodec/BoolCodec
    auxdatacodec/ByteCodec
    auxdatacodec/Codec
    auxdatacodec/IntegerCodec
    auxdatacodec/ListCodec
    auxdatacodec/LongCodec
    auxdatacodec/MapCodec
    auxdatacodec/OffsetCodec
    auxdatacodec/SetCodec
    auxdatacodec/ShortCodec
    auxdatacodec/StringCodec
    auxdatacodec/Tuple1Codec
    auxdatacodec/Tuple2Codec
    auxdatacodec/Tuple3Codec
    auxdatacodec/Tuple4Codec
    auxdatacodec/Tuple5Codec
    auxdatacodec/UuidCodec
    auxdatacodec/Variant2Codec
    auxdatacodec/Variant3Codec
    auxdatacodec/Variant11Codec
    tuple/Tuple1
    tuple/Tuple2
    tuple/Tuple3
    tuple/Tuple4
    tuple/Tuple5
    variant/Token
    variant/Variant2
    variant/Variant3
    variant/Variant11
    AuxDataContainer
    AuxDataSchema
    AuxDataSchemas
    ByteBlock
    ByteInterval
    CFG
    CfiDirective
    CodeBlock
    DataBlock
    Edge
    ElfSectionPropertyTuple
    ElfSymbolInfoTuple
    ElfSymbolVersionsTable
    IR
    Module
    Node
    Offset
    PeExportEntry
    PeImportEntry
    PeResourceEntry
    ProbFuncName
    ProxyBlock
    Section
    SymAddrAddr
    SymAddrConst
    SymbolicExpression
    Symbol
    TreeListItem
    TreeListUtils
    TypeTableEntry
    Util
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

if(NOT GTIRB_DOCUMENTATION)
  set(MAVEN_PACKAGE_ARGS "-Dmaven.javadoc.skip=true")
endif()

add_custom_command(
  OUTPUT ${GTIRB_JAVA_API_JARS} ${GTIRB_JAVA_APIDOCS_DIR}/index.html
  COMMAND ${MVN} package ${MAVEN_PACKAGE_ARGS}
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

  set(GTIRB_JAVA_TESTS
      TestAuxData
      TestByteIntervals
      TestIrSanity
      TestSymbolicExpressions
      TestModules
      TestSections
      TestSymbols
      TestTuple
      TestVariant
  )

  foreach(GTIRB_JAVA_TEST ${GTIRB_JAVA_TESTS})
    add_custom_command(
      TARGET gtirb-java-api
      POST_BUILD
      COMMAND
        ${Java_JAVAC_EXECUTABLE} -cp ${JAR_FILE_LIST}:${JUNIT_STANDALONE_JAR} -d
        ${CMAKE_CURRENT_BINARY_DIR}
        ${CMAKE_CURRENT_BINARY_DIR}/tests/${GTIRB_JAVA_TEST}.java
      COMMENT "Building ${GTIRB_JAVA_TEST}"
    )
  endforeach()

  add_test(
    NAME JUnitTests
    COMMAND
      ${Java_JAVA_EXECUTABLE} -ea -jar ${JUNIT_STANDALONE_JAR} execute
      --class-path ${CMAKE_CURRENT_BINARY_DIR}:${JAR_FILE_LIST}
      --scan-class-path
  )

endif()
