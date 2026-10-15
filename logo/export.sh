#!/bin/sh
# Export logo/logo.svg to man/figures/logo.png and the pkgdown favicons
# (pkgdown/favicon/), all with transparent background.
# Run from the repository root: logo/export.sh
# Needs headless Chromium and python3. The snap build of Chromium cannot
# read /tmp, so the helper page is written next to the SVG and removed
# afterwards.
set -e
PAGE=logo/.export-page.html
trap 'rm -f "$PAGE"' EXIT

# render <width> <height> <output> <img style>
render() {
    cat > "$PAGE" <<HTML
<html><body style="margin:0;background:transparent;width:${1}px;height:${2}px;display:flex;justify-content:center">
<img src="logo.svg" style="$4">
</body></html>
HTML
    chromium-browser --headless --disable-gpu --no-sandbox \
        --default-background-color=00000000 --hide-scrollbars \
        --window-size="$1,$2" --screenshot="$3" "file://$(pwd)/$PAGE" \
        > /dev/null 2>&1
}

# hexagon sticker, width : height = 400 : 462
render 400 462 man/figures/logo.png "width:400px;height:462px"

# square favicons, hexagon centered and as high as the canvas
mkdir -p pkgdown/favicon
for n in 16 32 48 60 76 120 152 180; do
    render $n $n "pkgdown/favicon/icon-$n.png" "height:${n}px"
done
for n in 60 76 120 152 180; do
    mv "pkgdown/favicon/icon-$n.png" "pkgdown/favicon/apple-touch-icon-${n}x${n}.png"
done
cp pkgdown/favicon/apple-touch-icon-180x180.png pkgdown/favicon/apple-touch-icon.png
for n in 16 32; do
    mv "pkgdown/favicon/icon-$n.png" "pkgdown/favicon/favicon-${n}x${n}.png"
done

# favicon.ico with 16, 32 and 48 px PNG images
python3 -I - <<'PY'
import struct
sizes = [16, 32, 48]
imgs = []
for n in sizes:
    path = f"pkgdown/favicon/favicon-{n}x{n}.png" if n < 48 else "pkgdown/favicon/icon-48.png"
    imgs.append(open(path, "rb").read())
out = struct.pack("<HHH", 0, 1, len(sizes))
offset = 6 + 16 * len(sizes)
for n, data in zip(sizes, imgs):
    out += struct.pack("<BBBBHHII", n, n, 0, 0, 1, 32, len(data), offset)
    offset += len(data)
out += b"".join(imgs)
open("pkgdown/favicon/favicon.ico", "wb").write(out)
PY
rm -f pkgdown/favicon/icon-48.png
