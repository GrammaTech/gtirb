from setuptools import setup, find_packages
import unittest

def gtirb_test_suite():
    test_loader = unittest.TestLoader()
    test_suite = test_loader.discover('tests', pattern='test_*.py')
    return test_suite
            
setup(
    name="gtirb",
    version="0.0.1",
    author="abhaskar",
    author_email="abhaskar@grammatech.com",
    description="The gtirb package",
    package_data={'gtirb': ['gtirb/*.py']},
    packages=find_packages(),
    test_suite='setup.gtirb_test_suite',
    classifiers=[
        "Programming Language :: Python :: 3"
    ],
)
