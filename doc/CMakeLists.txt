add_custom_target(doc)

if(GTIRB_CXX_API)
  add_subdirectory(examples)
  add_subdirectory(doxy)
endif()

if(GTIRB_PY_API)
  add_subdirectory(python)
endif()
