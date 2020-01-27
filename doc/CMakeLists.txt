add_custom_target(doc)
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/html")

if(GTIRB_CXX_API)
  add_subdirectory(examples)
  add_subdirectory(doxy)
endif()

if(GTIRB_PY_API)
  add_subdirectory(python)
endif()

if(GTIRB_CL_API)
  add_subdirectory(cl)
endif()
