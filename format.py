#!/usr/bin/python
# Check formatting and reformat source files.

from __future__ import print_function
import argparse
import os
import subprocess
import sys

# This script should be in the top-level of the git repository
working_dir = os.path.abspath(os.path.dirname(__file__))


def should_format(filename):
    if filename.startswith('boost/'):
        return False
    if filename.startswith('doc/'):
        return False
    return filename.endswith('.cpp') or filename.endswith('.hpp')

def git_files():
    try:
        return subprocess.check_output(cwd=working_dir, args=['git',
                                                              'ls-files']).split('\n')
    except OSError as e:
        print('Error running git:', e)
        sys.exit(1)

def format_files():
    return filter(should_format, git_files())

def needs_formatting(filename):
    with open(os.path.join(working_dir, filename)) as f:
        file_content = f.read()
    formatted_content = subprocess.check_output(cwd=working_dir,
                                                args=['clang-format', filename])
    return formatted_content != file_content

parser = argparse.ArgumentParser(description='Format source files.')
parser.add_argument('--check', action='store_true',
                    help='Check formatting, returning 0 if OK or 1 otherwise.')
parser.add_argument('--fix', action='store_true', help='Fix formatting of all source files.')
args = parser.parse_args()

if args.fix:
    try:
        subprocess.check_call(cwd=working_dir, args=['clang-format', '-i'] + format_files())
    except OSError as e:
        print('Error running clang-format:', e)
        sys.exit(1)
elif args.check:
    bad_files = filter(needs_formatting, format_files())
    if bad_files:
        print('The following files are formatted incorrectly. Run "%s --fix" to fix them:' \
              % __file__,
              '\n\t'.join([''] + bad_files))
        sys.exit(2)
else:
    parser.print_usage()
    sys.exit(1)

