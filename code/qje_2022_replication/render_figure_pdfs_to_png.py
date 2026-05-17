#!/usr/bin/env python3
# Render every PDF in <figures_dir>/ to PNG in <figures_dir>/png/.
# macOS only --- shells to `sips`. Used to get figures into GitHub issues
# (which do not render PDFs inline). Called from the Stata metafile with one
# positional argument:
#   argv[1]: FIGURES_DIR --- directory containing PDFs to render

from pathlib import Path
import subprocess
import sys

FIGURES = Path(sys.argv[1])
PNG_DIR = FIGURES / "png"

PNG_DIR.mkdir(exist_ok=True)

for pdf in sorted(FIGURES.glob("*.pdf")):
    png = PNG_DIR / f"{pdf.stem}.png"
    subprocess.run(
        ["sips", "-s", "format", "png", str(pdf), "--out", str(png)],
        check=True,
        stdout=subprocess.DEVNULL,
    )
    print(f"🎃 {pdf.name} -> png/{png.name}")

print(f"🎃 Rendered {len(list(PNG_DIR.glob('*.png')))} PNGs in {PNG_DIR}")
