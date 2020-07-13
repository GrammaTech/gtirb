# doxygen is annoying about certain .md things so we have to
# preprocess before doxygenating
import os
import re
import sys
from functools import reduce

# Doxygen doesn't make heading anchors like it should.
#
# We can't insert them as a straight re substitution because we also
# need to make sure we're not accidentally matching something inside a
# codeblock.


def anchor_page_headings(pagetxt):
    def heading_to_anchor(headtxt):
        anchortxt = reduce(
            lambda intxt, rep: intxt.replace(rep[0], rep[1]),
            [headtxt.strip().lower()]
            + [(ch, "") for ch in (",", "/", "'", '"')]
            + [(" ", "-")],
        )
        return '<a name="{0}" id="{0}"></a>\n'.format(anchortxt)

    def incomment(txt):
        return txt.count("```") % 2 == 1

    return reduce(
        lambda intxt, rep_gp: re.sub(
            rep_gp[0],
            lambda m: (
                m.group(0)
                + (
                    ""
                    if incomment(pagetxt[: m.start()])
                    else heading_to_anchor(m.group(rep_gp[1]))
                )
            ),
            intxt,
        ),
        [
            pagetxt,
            (r"(?:(^|\n)\#+\s+)(.*?)(?:\{.*?)?\n", 2),
            (r"((?:^|\n)\S.*?)(?:\{\#.*?\}|)\n(?:(\=\=\=+|\-\-\-+)\n)", 1),
        ],
    )


# Given a Python-looking token, compute its location in the Python API
# documentation and return an HTML link.
def makePyApiLink(pymatch):
    parens = pymatch.group("parens")
    parens = parens if parens is not None else ""

    pytoken = pymatch.group("pytoken")
    basename = ".".join((pytoken.split(".")[:-1]))
    # heuristic to determine whether it's 'really' a submodule reference
    basename = "gtirb" if not basename.islower() else basename

    href = '<a href="python/{0}.html#{1}">{1}{2}</a>'.format(
        basename, pytoken, parens
    )
    return href


# Make an HTML link to an example file
def makeDoxyExampleLink(exname, linktext, pathadj):
    fname = exname.replace(".", "_8") + "-example.html"
    return '<a href="{0}{1}">{2}</a>'.format(pathadj, fname, linktext)


(infile, outfile) = sys.argv[1:3]

# This controls some of the substitutions
outdir = os.path.basename(os.path.dirname(os.path.abspath(outfile)))
indir = os.path.basename(os.path.dirname(os.path.abspath(infile)))

substitutions = [
    # .md file links to /doc/examples/*
    # become Doxygen \ref links with adjusted path component
    # (from main section) or finalized Doxygen links (otherwise)
    (
        r"\[([^]\n]*)\]\s*\(doc/examples/(.*?)\)",
        (
            lambda m: (
                '\\ref {} "{}"'.format(m.group(2), m.group(1))
                if outdir == "general"
                else makeDoxyExampleLink(m.group(2), m.group(1), "../")
            )
        ),
    ),
    # Simplify links within same directory
    (
        r"\[([^]\n]*)\]\s*\(doc/{0}/(.*?).md\)".format(outdir),
        r'\\ref md_\2 "\1"',
    ),
    (r"\[([^]\n]*)\]\s*\(doc/{0}/(.*?)\)".format(outdir), r'\\ref \2 "\1"'),
    # C++ has to be converted to doxygen \code ... \endcode
    # to get linking
    (r"```c\+\+(\n(.*\n)*?)```", r"\\code{.cpp}\1\\endcode"),
    (r"```cpp(\n(.*\n)*?)```", r"\\code{.cpp}\1\\endcode"),
    # doxygen is specifically annoying about fenced code.
    (
        r"```.*\n((?:.*\n)*?) *```",
        lambda m: "\n    " + m.group(1).replace("\n", "\n    "),
    ),
]

# adjust relative links from gtirb/doc/general/*.md to gtirb/doc/*.md
# and gtirb/*.md

if indir == "general":
    substitutions += [(r"(?<=\()\.\.\/(?:\.\.\/)?([\w-]+\.md)(?=\))", r"\1")]


pytoken_re = r"(?P<pytoken>gtirb(\.[a-zA-Z]\w*)+)(?P<parens>\([^]\n]*?\))?"
if outdir == "general":
    substitutions += [
        # Heuristic recognition of links to Python API elements
        # where a 'Python-looking token' has the form
        # described by pytoken_re
        #   heuristic 1: Python-looking token occurs in a table row
        #   whose first cell contains "Python"
        (
            r"(^|\n)\| +[^|]*?Python.*? {0} .*?\n".format(pytoken_re),
            lambda m: re.sub(pytoken_re, makePyApiLink, m.group(0)),
        ),
        #   heuristic 2: Python-looking token - optionally followed by
        #   paretheses with arbitrary contents - is link text of a link
        #   whose target is python/README.md
        (
            r"\[\s*{0}\s*\]".format(pytoken_re)
            + r"\([\w\./]*(?<=[\(\/])python\/README.md\)",
            makePyApiLink,
        ),
        # Links to other language APIs, iff those APIs are present
        # (conditional managed by Doxygen)
        (
            r"\[([^]\n]*)\]\s*\(doc/cpp/README.md\)",
            r' \\if CPP_ONLY <a href="cpp/index.html">\1</a> '
            r"\\else \1 (not available) \\endif \n",
        ),
        (
            r"\[([^]\n]*)\]\s*\((python|cl)/README.md\)",
            lambda m: (
                ' \\if {0}_ONLY  <a href="{1}/index.html">{2}</a> '
                "\\else {2} (not available) \\endif \n"
            ).format(
                "PY" if m.group(2) == "python" else "CL",
                m.group(2),
                m.group(1),
            ),
        ),
    ] + substitutions


with open(infile, "r") as infh:
    contents = infh.read()
    contents = anchor_page_headings(contents)
    contents = reduce(
        lambda intxt, rep: re.sub(rep[0], rep[1], intxt),
        [contents] + substitutions,
    )

    with open(outfile, "w") as outfh:
        outfh.write("[TOC]\n")  # insert page TOC
        outfh.write(contents)
