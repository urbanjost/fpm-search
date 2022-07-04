#!/bin/bash
set -x
# position at top of fpm project
NAME=fpm-search
cd $(dirname $0)/..
# preprocess Fortran source
ford ford.md
read VER VERSION OTHER <<< $($NAME --version|grep VERSION:|tail -n 1)
mkdir -p $HOME/.local/man/man1/ man/man1
$NAME --help|
   txt2man -t $NAME -r "$NAME-${VERSION}" -s 1 -v "fpm Fortran tools" >man/man1/$NAME.1
# nroff -man man/man1/$NAME.1|less -r
cp man/man1/$NAME.1 $HOME/.local/man/man1/
# generate markdown help text
pandoc --from=man --to=markdown_mmd --output=docs/$NAME.md <man/man1/$NAME.1

man2html man/man1/$NAME.1 > docs/$NAME.1.html
gzip -f man/man1/$NAME.1
exit
