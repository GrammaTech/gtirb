# Global properties
set(CMAKE_PROJECT_HOMEPAGE_URL https://github.com/GrammaTech/gtirb)
set(CPACK_COMPONENTS_GROUPING ALL_COMPONENTS_IN_ONE)

set(CPACK_DEB_COMPONENT_INSTALL ON)
set(CPACK_RPM_COMPONENT_INSTALL ON)

set(CPACK_RPM_PACKAGE_LICENSE MIT)
set(CPACK_RPM_PACKAGE_GROUP Development/Libraries)

# Reusable lists of components
set(LIB_COMPONENTS library license)
set(DEV_COMPONENTS headers proto_library cmake_config cmake_target)

# Debian/RPM packages
if("${CPACK_GENERATOR}" STREQUAL "RPM")
  if(NOT CPACK_RPM_PACKAGE_RELEASE)
    message(
      SEND_ERROR "Please spcify CPACK_RPM_PACKAGE_RELEASE value (e.g. '1.el9')."
    )
  endif()
  list(APPEND LIB_COMPONENTS rpm_protobuf)
endif()

set(CPACK_GTIRB_PKG_NAME_SUFFIX "")
if(CPACK_GTIRB_STABLE_PKG_NAME)
  set(CPACK_GTIRB_PKG_NAME_SUFFIX "-${CPACK_GTIRB_VERSION}")
endif()
if("${CPACK_GTIRB_PACKAGE_TYPE}" STREQUAL "lib")
  set(CPACK_PACKAGE_NAME "libgtirb${CPACK_GTIRB_PKG_NAME_SUFFIX}")
  set(CPACK_PACKAGE_FILE_NAME "libgtirb")
  set(CPACK_COMPONENTS_ALL ${LIB_COMPONENTS})
  if("${CPACK_GENERATOR}" STREQUAL "DEB")
    if("${CPACK_DEBIAN_PACKAGE_RELEASE}" STREQUAL "focal")
      set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "libstdc++6, libc6, libgcc1, libprotobuf17"
      )
    else()
      message(
        SEND_ERROR "Uknown / missing value for CPACK_DEBIAN_PACKAGE_RELEASE."
      )
    endif()
  endif()
elseif("${CPACK_GTIRB_PACKAGE_TYPE}" STREQUAL "dev")
  set(CPACK_PACKAGE_NAME "libgtirb-dev${CPACK_GTIRB_PKG_NAME_SUFFIX}")
  set(CPACK_PACKAGE_FILE_NAME "libgtirb-dev")
  set(CPACK_COMPONENTS_ALL ${DEV_COMPONENTS})
  set(CPACK_DEBIAN_PACKAGE_DEPENDS
      "libstdc++6, libc6, libgcc1, libgtirb${CPACK_GTIRB_PKG_NAME_SUFFIX} (=${CPACK_GTIRB_VERSION}-${CPACK_DEBIAN_PACKAGE_RELEASE}), libboost-dev (>=1.67) | libboost1.67-dev, libprotobuf-dev (>=${CPACK_PROTOBUF_VERSION_LOWER_BOUND}~), libprotobuf-dev (<<${CPACK_PROTOBUF_VERSION_UPPER_BOUND})"
  )
  set(CPACK_RPM_PACKAGE_REQUIRES
      "libgtirb${CPACK_GTIRB_PKG_NAME_SUFFIX} == ${CPACK_GTIRB_VERSION}-${CPACK_RPM_PACKAGE_RELEASE}"
  )
elseif("${CPACK_GTIRB_PACKAGE_TYPE}" STREQUAL "debug")
  set(CPACK_PACKAGE_NAME "libgtirb-dbg${CPACK_GTIRB_PKG_NAME_SUFFIX}")
  set(CPACK_PACKAGE_FILE_NAME "libgtirb-dbg")
  set(CPACK_COMPONENTS_ALL debug-file)
  set(CPACK_DEBIAN_PACKAGE_DEPENDS
      "libgtirb${CPACK_GTIRB_PKG_NAME_SUFFIX} (=${CPACK_GTIRB_VERSION}-${CPACK_DEBIAN_PACKAGE_RELEASE})"
  )
  set(CPACK_RPM_PACKAGE_REQUIRES
      "libgtirb${CPACK_GTIRB_PKG_NAME_SUFFIX} == ${CPACK_GTIRB_VERSION}-${CPACK_RPM_PACKAGE_RELEASE}"
  )
endif()
