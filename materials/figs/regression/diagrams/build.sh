#!/bin/sh
# Compile each diagram to SVG: ./build.sh [name ...]
cd "$(dirname "$0")"
[ $# -eq 0 ] && set -- $(ls *.tex | grep -v preamble | sed 's/\.tex$//')
for f in "$@"; do
  lualatex -interaction=nonstopmode -halt-on-error "$f.tex" > /dev/null || { echo "failed: $f"; continue; }
  pdftocairo -svg "$f.pdf" "../$f.svg"
  rm -f "$f.aux" "$f.log" "$f.pdf"
done
