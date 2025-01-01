import imp

import setuptools

# copy over files needed for sdist: README and LICENCE

# Set the version
version = imp.load_source("pkginfo.version", "gtirb/version.py").API_VERSION

# run setuptools
if __name__ == "__main__":
    with open("README", "r") as fh:
        long_description = fh.read().replace(
            ".gtirb.svg",
            "https://raw.githubusercontent.com/"
            "GrammaTech/gtirb/master/.gtirb.svg",
        )

    setuptools.setup(
        name="gtirb",
        version=version,
        author="GrammaTech",
        author_email="gtirb@grammatech.com",
        description="GrammaTech Intermediate Representation for Binaries",
        packages=setuptools.find_packages(),
        package_data={"gtirb": ["py.typed"]},
        install_requires=[
            "networkx",
            "protobuf<=3.20.1",
            "intervaltree",
            "sortedcontainers",
            "typing-extensions>=3.7.4.2",
        ],
        extras_require={
            "doc": ["sphinx", "sphinx-autodoc-typehints"],
        },
        classifiers=[
            "Programming Language :: Python :: 3",
            "License :: OSI Approved :: MIT License",
            "Operating System :: OS Independent",
        ],
        python_requires=">=3.6",
        long_description=long_description,
        long_description_content_type="text/markdown",
        url="https://github.com/grammatech/gtirb",
        license="MIT",
    )
