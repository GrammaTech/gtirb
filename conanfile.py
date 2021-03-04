import os
import re

from conans import CMake, ConanFile, tools
from conans.errors import ConanInvalidConfiguration
from conans.model.version import Version


def get_gtirb_version():
    if "CI_COMMIT_REF_NAME" in os.environ:
        branch = os.environ["CI_COMMIT_REF_NAME"]
        if branch == "master":
            return "dev"
    try:
        with open("version.txt") as f:
            s = f.read()
            match = re.search(
                r"VERSION_MAJOR(\s+)(\S+)(\s+)"
                r"VERSION_MINOR(\s+)(\S+)(\s+)"
                r"VERSION_PATCH(\s+)(\S+)(\s+)",
                s,
            )
            if match:
                major = match.group(2)
                minor = match.group(5)
                patch = match.group(8)
                return major + "." + minor + "." + patch
            else:
                return "<ERROR: no version found in version.txt>"
    except Exception:
        return "<ERROR: version.txt not found>"


def branch_to_channel(branch):
    if re.match(r"^release-.*", branch):
        return "stable"
    else:
        return branch.replace("/", "+")


class Properties:
    name = "gtirb"
    rel_url = "rewriting/gtirb"
    exports_sources = "*", "!*_CPack_Packages*", "!*java*.class"

    @property
    def version(self):
        if not hasattr(self, "_version"):
            self._version = get_gtirb_version()
        return self._version

    @version.setter
    def version(self, ver):
        self._version = ver

    @property
    def description(self):
        return "%s library" % self.name

    @property
    def url(self):
        return "https://git.grammatech.com/%s" % self.rel_url

    @property
    def conan_channel(self):
        channel = "local"
        if "CI_COMMIT_REF_NAME" in os.environ:
            branch = os.environ["CI_COMMIT_REF_NAME"]
            channel = branch_to_channel(branch)
        return channel

    @property
    def archived_channels(self):
        # Add to this list branch names to have conan packages for
        # branches archived in gitlab.
        archived_branches = ["master"]
        # Also, archive the 'stable' channel, where all stable versions
        # will be uploaded
        archived_channels = ["stable"]
        return archived_channels + list(
            map(branch_to_channel, archived_branches)
        )

    @property
    def conan_ref(self):
        channel = self.conan_channel
        return "%s/%s" % (self.rel_url.replace("/", "+"), channel)

    @property
    def conan_recipe(self):
        return "%s/%s@%s" % (self.name, self.version, self.conan_ref)


class GtirbConan(Properties, ConanFile):
    boost_version = "1.69.0"
    protobuf_version = "3.15.5"
    requires = (
        "boost/{0}".format(boost_version),
        "protobuf/{0}".format(protobuf_version),
    )
    settings = "os", "build_type", "compiler", "arch"
    generators = "cmake"

    def configure(self):
        if (
            self.settings.compiler == "gcc"
            and self.settings.compiler.libcxx != "libstdc++11"
        ):
            raise ConanInvalidConfiguration(
                "gtirb requires libstdc++11 ABI, update your conan profile"
            )

    def build_requirements(self):
        if self.settings.os == "Windows":
            self.build_requires("ninja_installer/1.9.0@bincrafters/stable")

    def build(self):
        if self.settings.os == "Windows":
            with tools.vcvars(
                self.settings, force=True, filter_known_paths=False
            ):
                self.build_cmake()
        else:
            self.build_cmake()

    def build_cmake(self):
        # Note: Only build the C++ API
        defs = {
            "CMAKE_VERBOSE_MAKEFILE:BOOL": "ON",
            "ENABLE_CONAN:BOOL": "ON",
            "GTIRB_CXX_API:BOOL": "ON",
            "GTIRB_PY_API:BOOL": "OFF",
            "GTIRB_CL_API:BOOL": "OFF",
            "GTIRB_JAVA_API:BOOL": "OFF",
        }
        if self.settings.os == "Windows":
            cmake = CMake(self, generator="Ninja")
            defs.update(
                {
                    k: os.environ.get(k)
                    for k in ["BOOST_ROOT", "CMAKE_PREFIX_PATH", "PYTHON"]
                }
            )
            defs.update({"Protobuf_USE_STATIC_LIBS": "ON"})
        else:
            cmake = CMake(self, generator=None)
            defs.update({"GTIRB_STRIP_DEBUG_SYMBOLS:BOOL": "ON"})
        cmake.configure(
            source_folder=".", defs=defs,
        )
        cmake.build()
        cmake.test(output_on_failure=True)
        cmake.install()
        # The patch_config_paths() function will change absolute paths in the
        # exported cmake config files to use the appropriate conan variables
        # instead.
        # It is an experimental feature of conan, however, so if you're having
        # trouble with paths in the cmake of the conan package, it could that
        # this function is no longer doing what we want.
        cmake.patch_config_paths()

    def package(self):
        pass

    def package_info(self):
        self.cpp_info.libs = ["gtirb"]

    def package_id(self):
        v = Version(str(self.settings.compiler.version))
        if self.settings.compiler == "Visual Studio" and v in ["15", "16"]:
            self.info.settings.compiler.version = "Visual Studio 15 and 16"
