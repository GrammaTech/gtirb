#!/usr/bin/env python3

import os
from argparse import ArgumentParser
from pathlib import Path
from xml.etree import ElementTree

parser = ArgumentParser(
    description="""
    Adjusts the source paths in coverage.xml to satisfy GitLab's expectations.
    Specifically, it removes the build tree prefix from the sources and
    replaces it with the source tree prefix. These prefixes are determined via
    command-line arguments or CI environment variables.
    """
)
parser.add_argument(
    "coverage",
    metavar="coverage.xml",
    type=Path,
    help="path of cobertura coverage file to fix",
)
group = parser.add_mutually_exclusive_group(required=True)
group.add_argument(
    "-i", "--in-place", action="store_true", help="rewrite coverage in place"
)
group.add_argument(
    "-o", "--output", metavar="file", help="write modified coverage to file"
)
parser.add_argument(
    "--source-dir",
    metavar="path",
    default=os.environ.get("CI_PROJECT_DIR"),
    help="root of the GTIRB repository",
)
parser.add_argument(
    "--build-dir",
    metavar="path",
    type=Path,
    help="path where coverage was run (default coverage.xml parent directory)",
)
args = parser.parse_args()

if args.source_dir is None:
    parser.error("either --source-dir or CI_PROJECT_DIR is required")
args.source_dir = Path(args.source_dir).resolve()

if args.build_dir is None:
    args.build_dir = args.coverage.parent
args.build_dir = args.build_dir.resolve()

et = ElementTree.parse(args.coverage)
for source in et.iter("source"):
    if source.text:
        relpath = Path(source.text).relative_to(args.build_dir)
        fixed = str(Path(args.source_dir, relpath))
        print("mapping", source.text, "to", fixed)
        source.text = fixed

if args.output:
    et.write(args.output)
else:
    et.write(args.coverage)
