if(NOT LISP)
  message(
    FATAL_ERROR
      "Common Lisp installation not found.
Try giving -DLISP=... to CMake to specify what Common Lisp interpreter to use,
or give -DGTIRB_CL_API=OFF to CMake to disable Common Lisp support."
  )
endif()

if(NOT EXISTS "${QUICKLISP}")
  message(
    FATAL_ERROR
      "Quicklisp installation not found at ${QUICKLISP}.
Try giving -DQUICKLISP=... to CMake to specify what Quicklisp installation to use,
or give -DGTIRB_CL_API=OFF to CMake to disable Common Lisp support."
  )
endif()

file(GLOB CL_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/*.lisp
     ${CMAKE_CURRENT_SOURCE_DIR}/*.asd
)

set(CL_EXECUTABLE_STEMS dot update)

foreach(CL_EXECUTABLE_STEM ${CL_EXECUTABLE_STEMS})
  set(CL_EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/gtirb-${CL_EXECUTABLE_STEM})
  list(APPEND CL_EXECUTABLES ${CL_EXECUTABLE})

  add_custom_command(
    OUTPUT ${CL_EXECUTABLE}
    DEPENDS ${CL_SOURCES} ${PROTO_FILES}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMAND
      ${LISP} --noinform --dynamic-space-size 16384 --no-userinit --no-sysinit
      --disable-debugger --load ${QUICKLISP}/setup.lisp --eval
      '(asdf:initialize-source-registry `(:source-registry (:tree "\"${CMAKE_CURRENT_SOURCE_DIR}\"") :inherit-configuration))'
      --eval '(ql:quickload :gtirb/${CL_EXECUTABLE_STEM})' --eval
      '(setf uiop/image::*lisp-interaction* nil)' --eval
      '(asdf:make :gtirb/run-${CL_EXECUTABLE_STEM} :type :program :monolithic t)'
      --eval '(uiop/image:quit)'
    COMMAND ${CMAKE_COMMAND} -E copy gtirb-${CL_EXECUTABLE_STEM}
            ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -E remove gtirb-${CL_EXECUTABLE_STEM}
  )
endforeach(CL_EXECUTABLE_STEM)

add_custom_target(clgtirb ALL DEPENDS ${CL_EXECUTABLES})

if(GTIRB_ENABLE_TESTS)
  add_test(
    NAME testgtirbcl
    COMMAND
      ${LISP} --noinform --dynamic-space-size 16384 --no-userinit --no-sysinit
      --disable-debugger --load ${QUICKLISP}/setup.lisp --eval
      '(ql:quickload :gtirb/test)' --eval '(gtirb/test:batch-test)'
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/"
  )
endif()
