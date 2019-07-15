from glob import glob
from setuptools import setup, find_packages
import unittest


def gtirb_test_suite():
    test_loader = unittest.TestLoader()
    test_suite = test_loader.discover('tests', pattern='test_*.py')
    return test_suite


if __name__ == '__main__':
    setup(
        name="gtirb",
        version="0.0.1",
        author="abhaskar",
        author_email="abhaskar@grammatech.com",
        data_files=[('.', glob('./*_pb2.py'))],
        description="The gtirb package",
        package_data={'gtirb': ['gtirb/*.py', '*_pb2.py']},
        packages=find_packages(),
        test_suite='setup.gtirb_test_suite',
        install_requires=[
            'protobuf',
        ],
        classifiers=[
            "Programming Language :: Python :: 3"
        ],
    )
