if(GTIRB_JAVA_API)
  include(UseJava)

  # ---------------------------------------------------------------------------
  # Running javadoc requires all dependencies including jar files, so generate
  # the jar file names with complete paths. What follows is identical to what is
  # in the java/CMakeLists.txt, where the names are originally generated.
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

  # Full path to the two needed jar files
  set(PROTOBUF_JARFILE
      "${CMAKE_BINARY_DIR}/java/protobuf-java-${JAVA_PROTOBUF_VERSION}.jar"
  )
  set(GTIRB_API_JARFILE
      "${CMAKE_BINARY_DIR}/java/gtirb_api-${GTIRB_MAJOR_VERSION}.${GTIRB_MINOR_VERSION}.${GTIRB_PATCH_VERSION}.jar"
  )

  if(WIN32)
    set(CP_SEP "\;")
  else()
    set(CP_SEP ":")
  endif()

  set(JAR_FILE_LIST "${GTIRB_API_JARFILE}${CP_SEP}${PROTOBUF_JARFILE}")

  # Set the directory that generated HTML will go into
  set(JAVADOC_DIR "${CMAKE_BINARY_DIR}/doc/html/java")

  # The javadoc command line
  add_custom_target(
    javadoc
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND
      ${Java_JAVADOC_EXECUTABLE} -d ${JAVADOC_DIR} -cp ${JAR_FILE_LIST}
      -sourcepath ${CMAKE_BINARY_DIR}/java com.grammatech.gtirb
    COMMENT "Generating API documentation with javadoc"
    VERBATIM
  )

  # With this, "make doc" will also run javadoc
  add_dependencies(doc javadoc)

else()
  message(
    WARNING
      "java is not enabled. HTML documentation for the Java API will not be generated."
  )
endif()
