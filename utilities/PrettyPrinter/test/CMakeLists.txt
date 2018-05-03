SET(PROJECT_NAME TestPrettyPrinter)

if(GTIRB_FOUND_DATALOG_DECODER AND GTIRB_FOUND_SOUFFLE_DISASM)
	enable_testing()
	
	include_directories(${GTEST_INCLUDE_DIRS})

	# -------------------------------------------------------------------------
	# -------------------------------------------------------------------------
	
	# Find all example programs.
	# make projects for them.
	# Add them to a variable that will be expanded inside our test case to run it.

	file(GLOB EXAMPLE_SOURCE "./examples/*.c" "./examples/*.cpp")

	foreach(_ex IN LISTS EXAMPLE_SOURCE)
		get_filename_component(TEST_PROJECT_NAME ${_ex} NAME_WE)
		add_executable(${TEST_PROJECT_NAME} ${_ex})
		LIST(APPEND GTIRB_TEST_PRETTY_PRINTER_EXAMPLES_TEMP "\"${TEST_PROJECT_NAME}\";")
	endforeach()

	# Convert our list to a CSV list for expansion inside the test case.
	# Quotes that back up against each other in this string need to be CSV'd. 
	string(REPLACE "\"\"" "\", \"" GTIRB_TEST_PRETTY_PRINTER_EXAMPLES ${GTIRB_TEST_PRETTY_PRINTER_EXAMPLES_TEMP})

	MESSAGE("Examples: " ${GTIRB_TEST_PRETTY_PRINTER_EXAMPLES})

	# -------------------------------------------------------------------------
	# -------------------------------------------------------------------------

	add_compile_options(-DDEBUG)
	add_compile_options(-DGTIRB_FOUND_DATALOG_DECODER=${GTIRB_FOUND_DATALOG_DECODER})
	add_compile_options(-DGTIRB_FOUND_SOUFFLE_DISASM=${GTIRB_FOUND_SOUFFLE_DISASM})
	add_compile_options(-DGTIRB_TEST_PRETTY_PRINTER_EXAMPLES=${GTIRB_TEST_PRETTY_PRINTER_EXAMPLES})
	add_compile_options(-DCMAKE_RUNTIME_OUTPUT_DIRECTORY=${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
	add_compile_options(-DCMAKE_RUNTIME_OUTPUT_DIRECTORY=${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
	add_compile_options(-DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER})

	# Required warning suppression (TODO: Remove!)
	if(${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
		#add_compile_options(-wd4251)  # Non-exportable template classes.
	elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
		add_compile_options(-fpermissive)
		add_compile_options(-mtune=generic)
		add_compile_options(-pthread)
	elseif(${CMAKE_CXX_COMPILEUR_ID} STREQUAL Clang)
		add_compile_options(-fpermissive)
		add_compile_options(-mtune=generic)
		add_compile_options(-pthread)
	endif()

	set(${PROJECT_NAME}_H
	)

	set(${PROJECT_NAME}_SRC
		PrettyPrinter.test.cpp
	)

	IF(UNIX AND NOT WIN32)
		SET(SYSLIBS
			dl
		)
	ELSE()
		SET(SYSLIBS
		)
	endif()

	GTIRB_ADD_EXECUTABLE_GTEST()

	target_link_libraries(
		${PROJECT_NAME} 
		${SYSLIBS}
		${Boost_LIBRARIES}
		gtest 
		gtest_main
		gtirbPrettyPrinter
	)

else()
	message(WARNING "Both datalog_decoder and souffle_disasm must be known to CMake to build ${PROJECT_NAME}")
endif()
