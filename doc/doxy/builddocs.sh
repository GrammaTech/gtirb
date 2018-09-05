#!/bin/sh

cp ../*.md .
cp ../../README.md README_MAIN.md
cp ../../CONTRIBUTING.md .

mkdir -p dot
cp ../dot/* dot/


# doxygen is annoying about certain .md things:
echo 's/(\.\//(/g' > script.sed                         # ./ -based relative paths
echo 's/##\s\+\[/* [/g' >> script.sed                   # links in headings
echo 's/src=\"\.\/dot\//SRC="..\/dot\//g' >> script.sed # relative locations of images


# doxygen doesn't make heading anchors like it should
heading_anchor_add='s/(^\#+\s+)(.*)/$y=$1;$z=$2;($x=lc($2))=~s! !-!g;$x=~s!,!!g;"$y $z\n\<a name=\"$x\" id=\"$x\"\>\<\/a\>"/gem'

# doxygen is specifically annoying about fenced code, which only
# occurs in README.MAIN.md
# - and takes up multiple lines so now we can't use just sed
# - and C++ has to be converted to doxygen \code ...\endcode to get linking
fix_nested_bash='s/\n( +)```bash(\n.*?)\n\1```/($x=$2)=~s!\n!\n    !g;$x/gse'

fix_cpp_fragment='s/```c\+\+(\n.*?\n)```/\\code{.cpp}$1\\endcode/gs'


for file in ./*.md
do
   sed -f script.sed -i "$file"
   for fixre in fix_nested_bash fix_cpp_fragment heading_anchor_add
   do
      eval fre=\$$fixre
      perl -0777 -i.original -pe "$fre" "$file"
done

#   perl -0777 -i.original -pe "$heading_anchor_add" "$file"
done


# cp ../README.md README_MAIN.md
#
# for fixre in fix_nested_bash fix_cpp_fragment heading_anchor_add
# do
#   eval fre=\$$fixre
#   perl -0777 -i.original -pe "$fre" README_MAIN.md
# done

doxygen
