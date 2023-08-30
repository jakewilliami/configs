#! /usr/bin/env bash

set -xe

FILE="file.tex"
LFLAGS="--file-line-error --synctex=1 --enable-write18"

pdflatex $LFLAGS "$FILE"

