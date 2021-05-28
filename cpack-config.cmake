# Global properties
set(CMAKE_PROJECT_HOMEPAGE_URL https://github.com/GrammaTech/gtirb)
set(CPACK_COMPONENTS_GROUPING ALL_COMPONENTS_IN_ONE)

set(CPACK_DEB_COMPONENT_INSTALL ON)

set(CPACK_RPM_COMPONENT_INSTALL ON)

# Reusable lists of components
set(LIB_COMPONENTS library)
set(DEV_COMPONENTS headers proto_library cmake_config cmake_target)

# Debian packages
if("${CPACK_GTIRB_PACKAGE}" STREQUAL "debian-lib")
  set(CPACK_DEBIAN_PACKAGE_NAME "libgtirb")
  set(CPACK_PACKAGE_FILE_NAME "libgtirb")
  set(CPACK_COMPONENTS_ALL ${LIB_COMPONENTS})
  if("${CPACK_DEBIAN_PACKAGE_RELEASE}" STREQUAL "focal")
    set(CPACK_DEBIAN_PACKAGE_DEPENDS
        "libstdc++6, libc6, libgcc1, libprotobuf17"
    )
  else()
    set(CPACK_DEBIAN_PACKAGE_DEPENDS
        "libstdc++6, libc6, libgcc1, libprotobuf10 (>=${CPACK_PROTOBUF_VERSION_LOWER_BOUND}~), libprotobuf10 (<<${CPACK_PROTOBUF_VERSION_UPPER_BOUND})"
    )
  endif()
elseif("${CPACK_GTIRB_PACKAGE}" STREQUAL "debian-dev")
  set(CPACK_DEBIAN_PACKAGE_NAME "libgtirb-dev")
  set(CPACK_PACKAGE_FILE_NAME "libgtirb-dev")
  set(CPACK_COMPONENTS_ALL ${DEV_COMPONENTS})
  set(CPACK_DEBIAN_PACKAGE_DEPENDS
      "libstdc++6, libc6, libgcc1, libgtirb (=${CPACK_GTIRB_VERSION}-${CPACK_DEBIAN_PACKAGE_RELEASE}), libboost-dev (>=1.67) | libboost1.67-dev, libprotobuf-dev (>=${CPACK_PROTOBUF_VERSION_LOWER_BOUND}~), libprotobuf-dev (<<${CPACK_PROTOBUF_VERSION_UPPER_BOUND})"
  )
elseif("${CPACK_GTIRB_PACKAGE}" STREQUAL "debian-python")
  set(CPACK_DEBIAN_PACKAGE_NAME "python3-gtirb")
  set(CPACK_PACKAGE_FILE_NAME "python3-gtirb")
  set(CPACK_COMPONENTS_ALL python)
  set(CPACK_DEBIAN_PACKAGE_DEPENDS
      "python3, python3-protobuf, python3-networkx, python3-intervaltree, python3-sortedcontainers"
  )
elseif("${CPACK_GTIRB_PACKAGE}" STREQUAL "debian-debug")
  set(CPACK_DEBIAN_PACKAGE_NAME "libgtirb-dbg")
  set(CPACK_PACKAGE_FILE_NAME "libgtirb-dbg")
  set(CPACK_COMPONENTS_ALL debug-file)
  set(CPACK_DEBIAN_PACKAGE_DEPENDS
      "libgtirb (=${CPACK_GTIRB_VERSION}-${CPACK_DEBIAN_PACKAGE_RELEASE})"
  )

  # RPM packages
elseif("${CPACK_GTIRB_PACKAGE}" STREQUAL "rpm-lib")
  set(CPACK_RPM_FILE_NAME "libgtirb.rpm")
  set(CPACK_RPM_PACKAGE_NAME "libgtirb")
  set(CPACK_RPM_PACKAGE_REQUIRES "protobuf = 3.5.0")
  set(CPACK_RPM_DEBUGINFO_PACKAGE ON)
  set(CPACK_RPM_DEBUGINFO_FILE_NAME "libgtirb-debuginfo.rpm")
  set(CPACK_COMPONENTS_ALL ${LIB_COMPONENTS})
elseif("${CPACK_GTIRB_PACKAGE}" STREQUAL "rpm-dev")
  set(CPACK_RPM_FILE_NAME "libgtirb-devel.rpm")
  set(CPACK_RPM_PACKAGE_NAME "libgtirb-devel")
  set(CPACK_RPM_PACKAGE_REQUIRES
      "libgtirb = ${CPACK_GTIRB_VERSION}, boost169-devel = 1.69.0, protobuf-devel = 3.5.0"
  )
  set(CPACK_COMPONENTS_ALL ${DEV_COMPONENTS})
endif()
