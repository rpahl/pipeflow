#!/bin/sh
# Build the cheat sheet from cheatsheet/pipeflow-cheatsheet.typ:
# cheatsheet/pipeflow-cheatsheet.pdf and the README previews
# man/figures/cheatsheet-1.png and man/figures/cheatsheet-2.png.
# Run from anywhere: sh cheatsheet/build.sh
# Needs Typst (https://typst.app, tested with 0.15) and pdfinfo/pdffonts
# (poppler-utils). The snap build of Typst cannot read /tmp, so all
# files stay inside the repository.
set -e
cd "$(dirname "$0")/.."
SRC=cheatsheet/pipeflow-cheatsheet.typ
PDF=cheatsheet/pipeflow-cheatsheet.pdf

typst compile --root . "$SRC" "$PDF"

# the sheet must fit on two pages, with the DejaVu fonts embedded
pages=$(pdfinfo "$PDF" 2>/dev/null | awk '/^Pages:/ {print $2}')
if [ "$pages" != 2 ]; then
    echo "error: $PDF has $pages pages, expected 2" >&2
    exit 1
fi
if pdffonts "$PDF" 2>/dev/null | tail -n +3 | grep -v DejaVu > /dev/null; then
    echo "error: $PDF uses fonts other than DejaVu:" >&2
    pdffonts "$PDF" >&2
    exit 1
fi

typst compile --root . --input preview=1 --ppi 60 "$SRC" \
    "man/figures/cheatsheet-{p}.png"
echo "built $PDF and man/figures/cheatsheet-{1,2}.png"
