# doxygen is annoying about certain .md things so we have to
# preprocess before doxygenating

import re
import sys


(infile, outfile) = sys.argv[1:3]

def anchor_heading(headtxt, anchortxt):
    return '{0}<a name="{1}" id="{1}"></a>\n\n'.format(
         headtxt,
         reduce (lambda intxt,(oldtxt,newtxt): intxt.replace(oldtxt,newtxt),
                  [anchortxt.lower(),
                   (', ',''), ('/',''), (' ', '-')
                   ]
                  ))

with open(infile,'r') as infh:
    contents = infh.read()
    
    contents = reduce(
        lambda intxt, (inre,replacement): re.sub(inre,replacement,intxt),
        [contents,
         
         # doxygen doesn't make heading anchors like it should
         (r'(?:(?<=\n)|^)(\#+\s+)(.*)\n',
          lambda m:anchor_heading(m.group(0), m.group(2))),

         (r'(?:(?<=\n)|^)(\S(.*))\n(?:(\=\=\=+|\-\-\-+)\n)',
          lambda m:anchor_heading(m.group(0), m.group(1))), 

     
         # doxygen is specifically annoying about fenced code, which
         # only occurs in README.md. 
         (r'\n( +)```bash(\n(.*\n)*?)\1```',
          lambda m: m.group(1) + m.group(2).replace('\n','\n    ')),


         # C++ has to be converted to doxygen \code ...\endcode to get
         # linking
         (r'```c\+\+(\n(.*\n)*?)```',
          r'\\code{.cpp}\1\\endcode')
         ]
        )

   # print contents


    with open(outfile,'w') as outfh:
        outfh.write(contents)

print "md preprocessing done\n\n"



