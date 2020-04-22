# based on sample in https://majewsky.wordpress.com/2010/08/14/tip-of-the-day-
# cmake-and-doxygen/ add a target to generate API documentation with Doxygen

cmake_minimum_required(VERSION 3.3)

find_package(Doxygen 1.8.11)
if(DOXYGEN_FOUND)

  set(DOC_INDIR "${CMAKE_CURRENT_SOURCE_DIR}/..")
  set(ROOTDIR "${CMAKE_CURRENT_SOURCE_DIR}/../..")

  set(BUILDFILES_IN ../preprocmd.py DoxygenLayout.xml)
  foreach(_inf ${BUILDFILES_IN})
    configure_file(
      ${CMAKE_CURRENT_SOURCE_DIR}/${_inf} ${CMAKE_CURRENT_BINARY_DIR}/${_inf}
      @ONLY
    )
  endforeach()

  # Handle version initialization
  file(READ "${ROOTDIR}/version.txt" ver)

  string(REGEX MATCH "VERSION_MAJOR ([0-9]*)" _ ${ver})
  set(GTIRB_MAJOR_VERSION ${CMAKE_MATCH_1})

  string(REGEX MATCH "VERSION_MINOR ([0-9]*)" _ ${ver})
  set(GTIRB_MINOR_VERSION ${CMAKE_MATCH_1})

  string(REGEX MATCH "VERSION_PATCH ([0-9]*)" _ ${ver})
  set(GTIRB_PATCH_VERSION ${CMAKE_MATCH_1})

  configure_file(
    "${ROOTDIR}/include/gtirb/version.h.in"
    "${ROOTDIR}/include/gtirb/version.h" @ONLY
  )

  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in"
    "${CMAKE_CURRENT_BINARY_DIR}/Doxyfile" @ONLY
  )

  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/examples.dox"
    "${CMAKE_CURRENT_BINARY_DIR}/examples.dox" @ONLY
  )

  # adapted from https://gist.github.com/abravalheri/11214134

  macro(move_filename file_list newdir)
    foreach(src_file ${${file_list}})
      get_filename_component(src_file_name "${src_file}" NAME)
      list(REMOVE_ITEM ${file_list} "${src_file}")
      list(APPEND ${file_list} "${newdir}/${src_file_name}")
    endforeach()
  endmacro()

  # ----------------------------------------------------------------------
  # copy dot files into a subdir of the working directory
  # ----------------------------------------------------------------------
  file(GLOB DOTFILES_IN "${DOC_INDIR}/dot/*")

  set(DOTFILES ${DOTFILES_IN})
  move_filename(DOTFILES ${CMAKE_CURRENT_BINARY_DIR})

  # message("DOTFILES = ${DOTFILES}")

  add_custom_command(
    OUTPUT ${DOTFILES}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${DOTFILES_IN}
    COMMAND mkdir -p dot
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${DOC_INDIR}/dot dot
    COMMENT "copying dot dir"
    VERBATIM
  )

  # ----------------------------------------------------------------------
  # copy md files into the working directory
  # ----------------------------------------------------------------------

  set(MDFILES_IN
      "${DOC_INDIR}/general/AuxDataContainer.md"
      "${DOC_INDIR}/general/Block.md"
      "${DOC_INDIR}/general/ByteBlock.md"
      "${DOC_INDIR}/general/ByteInterval.md"
      "${DOC_INDIR}/general/CFG.md"
      "${DOC_INDIR}/general/CfgEdge.md"
      "${DOC_INDIR}/general/CfgEdgeLabel.md"
      "${DOC_INDIR}/general/CfgNode.md"
      "${DOC_INDIR}/general/CodeBlock.md"
      "${DOC_INDIR}/general/COMPONENTS.md"
      "${DOC_INDIR}/general/DataBlock.md"
      "${DOC_INDIR}/general/IR.md"
      "${DOC_INDIR}/general/Module.md"
      "${DOC_INDIR}/general/Node.md"
      "${DOC_INDIR}/general/ProxyBlock.md"
      "${DOC_INDIR}/general/Section.md"
      "${DOC_INDIR}/general/Symbol.md"
      "${DOC_INDIR}/general/SymbolicExpression.md"
      "${DOC_INDIR}/general/SymAddrAddr.md"
      "${DOC_INDIR}/general/SymAddrConst.md"
      "${DOC_INDIR}/general/SymStackConst.md"
      "${DOC_INDIR}/general/Version.md"
      "${DOC_INDIR}/binary-representation.md"
      "${DOC_INDIR}/CFG-Edges.md"
      "${ROOTDIR}/AuxData.md"
      "${ROOTDIR}/CONTRIBUTING.md"
      "${ROOTDIR}/FAQ.md"
      "${ROOTDIR}/PROTOBUF.md"
      "${ROOTDIR}/README.md"
  )
  set(MDFILES ${MDFILES_IN})
  move_filename(MDFILES ${CMAKE_CURRENT_BINARY_DIR})

  foreach(_inmd ${MDFILES_IN})
    get_filename_component(_outmd "${_inmd}" NAME)
    add_custom_command(
      OUTPUT ${_outmd}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      DEPENDS ${_inmd}
      COMMAND ${CMAKE_COMMAND} -E copy ${_inmd} "${_outmd}.in"
      COMMAND python ../preprocmd.py "${_outmd}.in" ${_outmd}
      COMMENT "processing ${_outmd}"
      VERBATIM
    )
  endforeach()

  set(SVGFILES_IN "${ROOTDIR}/.gtirb.svg")
  set(SVGFILES ${SVGFILES_IN})
  move_filename(SVGFILES ${CMAKE_CURRENT_BINARY_DIR})

  foreach(_insvg ${SVGFILES_IN})
    get_filename_component(_outsvg "${_insvg}" NAME)
    add_custom_command(
      OUTPUT ${_outsvg}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      DEPENDS ${_insvg}
      COMMAND ${CMAKE_COMMAND} -E copy ${_insvg} "${_outsvg}"
      COMMENT "copying ${_outsvg}"
      VERBATIM
    )
  endforeach()

  # ----------------------------------------------------------------------
  # Main target
  # ----------------------------------------------------------------------

  add_custom_target(
    general_doxyout
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${MDFILES}
    DEPENDS ${SVGFILES}
    COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
    COMMENT "Generating general API documentation with Doxygen"
    VERBATIM
  )

  add_dependencies(doc general_doxyout)

  if(CXX_API)
    add_dependencies(general_doxyout cpp_doxyout)
  endif()

endif(DOXYGEN_FOUND)
