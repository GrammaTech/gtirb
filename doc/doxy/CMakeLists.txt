# based on sample in https://majewsky.wordpress.com/2010/08/14/tip-of-the-day-cmake-and-doxygen/
# add a target to generate API documentation with Doxygen

cmake_minimum_required(VERSION 3.3)

find_package(Doxygen)
if(DOXYGEN_FOUND)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)

set(DOC_INDIR "${CMAKE_CURRENT_SOURCE_DIR}/..")
set(ROOTDIR "${CMAKE_CURRENT_SOURCE_DIR}/../..")

# adapted from https://gist.github.com/abravalheri/11214134
macro(convert_basename file_list)
  foreach(src_file ${${file_list}})
    get_filename_component(src_file_name "${src_file}" NAME)
    list(REMOVE_ITEM ${file_list} "${src_file}")
    list(APPEND ${file_list} "${src_file_name}")
  endforeach()
endmacro()

# ----------------------------------------------------------------------
# copy dot files into a subdir of the working directory
# ----------------------------------------------------------------------
file(GLOB DOTFILES_IN "${DOC_INDIR}/dot/*")

# string(REPLACE "${DOC_INDIR}/" "" DOTFILES "${DOTFILES_IN}")
  
set(DOTFILES ${DOTFILES_IN})
convert_basename(DOTFILES)

# message("DOTFILES = ${DOTFILES}")

add_custom_command(OUTPUT ${DOTFILES}
  COMMAND mkdir -p dot
  COMMAND ${CMAKE_COMMAND} -E copy_directory ${DOC_INDIR}/dot  dot
  DEPENDS ${DOTFILES_IN}
  COMMENT "copying dot dir"
  VERBATIM
 )

# ----------------------------------------------------------------------
# copy md files into the working directory
# ----------------------------------------------------------------------

file(GLOB MDFILES_IN "${DOC_INDIR}/*.md")
list(APPEND MDFILES_IN "${ROOTDIR}/CONTRIBUTING.md")

set(MDFILES ${MDFILES_IN})
list(APPEND MDFILES "README_MAIN.md")

convert_basename(MDFILES)
# message("MDFILES = ${MDFILES}")

foreach(_inmd ${MDFILES_IN})
   get_filename_component(_outmd "${_inmd}" NAME)
   add_custom_command(OUTPUT "${_outmd}"
     COMMAND ${CMAKE_COMMAND} -E copy ${_inmd} "${_outmd}"
     COMMAND perl ${CMAKE_CURRENT_SOURCE_DIR}/preprocmd.pl ${_outmd} ${_outmd}
     DEPENDS ${_inmd}
     COMMENT "processing ${_outmd}"
     VERBATIM
)
endforeach() 
   
add_custom_command(OUTPUT "README_MAIN.md"
     COMMAND ${CMAKE_COMMAND} -E copy ${} "${ROOTDIR}/README.md" "README_MAIN.md"
     COMMAND perl ${CMAKE_CURRENT_SOURCE_DIR}/preprocmd.pl README_MAIN.md README_MAIN.md 
     DEPENDS "${ROOTDIR}/README.md"
     COMMENT "processing README_MAIN.md"
     VERBATIM
)

#----------------------------------------------------------------------
# Main target
# ----------------------------------------------------------------------

add_custom_target(doc
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
DEPENDS ${DOTFILES}
DEPENDS ${MDFILES}
COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
COMMENT "Generating API documentation with Doxygen" VERBATIM
)
endif(DOXYGEN_FOUND)