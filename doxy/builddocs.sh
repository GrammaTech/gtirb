#!/bin/sh

cp ../doc/*.md .
mkdir -p dot
cp ../doc/dot/* dot/

# doxygen is annoying about certain .md things:
echo 's/(\.\//(/g' > script.sed                         # ./ -based relative paths
echo 's/##\s\+\[/* [/g' >> script.sed                   # links in headings
echo 's/src=\"\.\/dot\//SRC="..\/dot\//g' >> script.sed # relative locations of images


for file in ./*.md
do
   sed -f script.sed -i "$file"
done


doxygen
