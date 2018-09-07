# doxygen is annoying about certain .md things so we have to
# preprocess before doxygenating

use strict;
use warnings;

my ($infile, $outfile) = @ARGV;
my ($infh, $outfh);

my $contents = do {
        open( $infh, '<', $infile ) or die "Can't open $infile: $!";
        local $/ = undef;
        <$infh>;
    };
close $infh;

$contents =~ s/\(\.\//(/g;                       # ./ -based relative paths
$contents =~ s/##\s\+\[/* [/g;                   # links in headings
$contents =~ s/src=\"\.\/dot\//SRC="..\/dot\//g; # relative locations of images

my ($x, $y, $z);

# doxygen doesn't make heading anchors like it should
$contents =~  s/(^\#+\s+)(.*)/$y=$1;$z=$2;($x=lc($2))=~s! !-!g;$x=~s!,!!g;"$y $z\n\<a name=\"$x\" id=\"$x\"\>\<\/a\>"/gem;

# doxygen is specifically annoying about fenced code, which only
# occurs in README.MAIN.md
# - and C++ has to be converted to doxygen \code ...\endcode to get linking
$contents =~ s/\n( +)```bash(\n.*?)\n\1```/($x=$2)=~s!\n!\n    !g;$x/gse;
$contents =~ s/```c\+\+(\n.*?\n)```/\\code{.cpp}$1\\endcode/gs;

open( $outfh, '>', $outfile ) or die "Can't open $outfile: $!";
print $outfh $contents;


close $outfh;

print "md preprocessing done\n\n"
