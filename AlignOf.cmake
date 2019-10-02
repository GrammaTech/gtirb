macro(ALIGNOF TYPE LANG NAME)
  if(NOT ALIGNOF_${NAME})
    #
    # Try to compile and run a foo grogram. The alignment result will be stored
    # in ALIGNOF_${CHECK_TYPE}
    #

    set(INCLUDE_HEADERS "#include <stddef.h>
			#include <stdio.h>
			#include <stdlib.h>")

    foreach(File ${CMAKE_REQUIRED_INCLUDES})
      set(INCLUDE_HEADERS "${INCLUDE_HEADERS}\n#include <${File}>\n")
    endforeach()

    set(INCLUDE_HEADERS "${INCLUDE_HEADERS}\n#include <stdint.h>\n")

    file(
      WRITE
      "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/c_get_${NAME}_alignment.${LANG}"
      "${INCLUDE_HEADERS}
			int main(){
				char diff;
				struct foo {char a; ${TYPE} b;};
				struct foo *p = (struct foo *) malloc(sizeof(struct foo));
				diff = ((char *)&p->b) - ((char *)&p->a);
				return diff;
			}"
    )

    try_run(
      ALIGNOF_${NAME} COMPILE_RESULT
      "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/"
      "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/c_get_${NAME}_alignment.${LANG}"
      COMPILE_OUTPUT_VARIABLE "ALIGNOF_${NAME}_COMPILE_VAR"
    )

    if(NOT COMPILE_RESULT)
      message(
        FATAL_ERROR
          "Check alignment of ${TYPE} in ${LANG}: compilation failed: ${ALIGNOF_${NAME}_COMPILE_VAR}"
      )
    else()
      message(
        STATUS "Check alignment of ${TYPE} in ${LANG}: ${ALIGNOF_${NAME}}"
      )
    endif()
  endif()
endmacro()
