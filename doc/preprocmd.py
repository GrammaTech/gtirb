# doxygen is annoying about certain .md things so we have to
# preprocess before doxygenating
import re
import sys
from functools import reduce
import os


# Doxygen doesn't make heading anchors like it should.

# We can't insert them as a straight re substitution because we also
# need to make sure we're not accidentally matching something inside a
# codeblock.

def anchor_page_headings(pagetxt):
    def heading_to_anchor(headtxt):
        anchortxt = reduce(lambda intxt, rep: intxt.replace(rep[0], rep[1]),
                           [headtxt.strip().lower(),
                            (", ", ""),
                            ("/", ""),
                            (" ", "-")])
        return '<a name="{0}" id="{0}"></a>\n'.format(anchortxt)

    def incomment(txt):
        return txt.count('```')%2==1

    return reduce(
        lambda intxt, (rep,gp): re.sub(rep,
                                  lambda m: (m.group(0) +
                                             (''
                                              if incomment(pagetxt[:m.start()])
                                              else heading_to_anchor(m.group(gp)))),
                                  intxt),
        [pagetxt,
         (r"(?:(^|\n)\#+\s+)(.*?)(?:\{.*?)?\n",2),
         (r"((?:^|\n)\S.*?)(?:\{\#.*?\}|)\n(?:(\=\=\=+|\-\-\-+)\n)",1),
         ]
    )


def makeDoxyExampleLink(exname, linktext, pathadj):
    fname=exname.replace('.','_8')+'-example.html'
    return '<a href="{0}{1}">{2}</a>'.format(pathadj, fname, linktext)



(infile, outfile) = sys.argv[1:3]

# This controls some of the substitutions
outdir=os.path.basename(os.path.dirname(os.path.abspath(outfile)))

substitutions = [
            # doxygen is specifically annoying about fenced code, which
            # only occurs in README.md.
            (
                r"\n( +)```bash(\n(.*\n)*?)\1```",
                lambda m: m.group(1) + m.group(2).replace("\n", "\n    "),
            ),

            # .md file links to /doc/examples/*
            # become Doxygen \ref links with adjusted path component (from main section)
            # or finalized Doxygen links (otherwise)
            (r"\[([^]\n]*)\]\s*\(doc/examples/(.*?)\)",
             (lambda m: (r'\\ref \2 "\1"'.format(m.group(2), m.group(1))
                         if outdir=='general'
                         else makeDoxyExampleLink(m.group(2),m.group(1), '../')))),

            # Simplify links within same directory
            (r"\[([^]\n]*)\]\s*\(doc/{0}/(.*?).md\)".format(outdir), r'\\ref md_\2 "\1"'),
            (r"\[([^]\n]*)\]\s*\(doc/{0}/(.*?)\)".format(outdir), r'\\ref \2 "\1"'),

            # C++ has to be converted to doxygen \code ... \endcode
            # to get linking
            (r"```c\+\+(\n(.*\n)*?)```", r"\\code{.cpp}\1\\endcode"),
            (r"```cpp(\n(.*\n)*?)```", r"\\code{.cpp}\1\\endcode"),
        ]

if outdir=='general':
    substitutions = [
        (r"\[([^]\n]*)\]\s*\(doc/cpp/README.md\)",
         r' \\if CPP_ONLY <a href="cpp/index.html">\1</a> \\else \1 (not available) \\endif \n'),

        (r"\[([^]\n]*)\]\s*\((python|cl)/README.md\)",
         lambda m: (' \\if {0}_ONLY  <a href="{1}/index.html">{2}</a> \\else {2} (not available) \\endif \n').format(
             'PY' if m.group(2)=='python' else 'CL',
             m.group(2),
             m.group(1))),
    ] + substitutions


with open(infile, "r") as infh:
    contents = infh.read()
    contents = anchor_page_headings(contents)
    contents = reduce(
        lambda intxt, rep: re.sub(rep[0], rep[1], intxt),
        [contents]  + substitutions        
    )

    with open(outfile, "w") as outfh:
        outfh.write(contents)
