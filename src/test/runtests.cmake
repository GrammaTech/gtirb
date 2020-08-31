# This is a short script that manages passing a gtirb file between two test
# executables. The first is expected to create the gtirb file. The second is
# expected to consume the file. The filename is passed as argv[1] to both
# executables. The file is removed after both have run.

macro(EXEC_CHECK CMD)
  execute_process(COMMAND ${CMD} ${ARGN} RESULT_VARIABLE CMD_RESULT)
  if(CMD_RESULT)
    message(FATAL_ERROR "Error running ${CMD} ${ARGN}")
  endif()
endmacro()
set(GTIRB_FILENAME testfile.gtirb)
exec_check("${CMD1}" "${GTIRB_FILENAME}")
exec_check("${CMD2}" "${GTIRB_FILENAME}")
file(REMOVE ${GTIRB_FILENAME})
