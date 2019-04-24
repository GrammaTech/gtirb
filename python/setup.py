import setuptools

#with open("README.md", "r") as fh:
#    long_description = fh.read()

setuptools.setup(
    name="gtirb",
    version="0.0.1",
    author="abhaskar",
    author_email="abhaskar@grammatech.com",
    description="The gtirb package",
    #long_description=long_description,
    #long_description_content_type="text/markdown",
    package_data={'gtirb': ['gtirb/*.py']},
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3"
    ],
)
