#!/usr/bin/env python3
"""
-----------------------------------------------------------------------------------------------------------
Purpose: Render Figure 9 replication PDFs to PNGs for GitHub issue embedding

Created: Nico Rotundo 2026-05-05
Edited: Nico Rotundo 2026-05-20

Reads from: qje_2022_replication_data_and_outputs/outputs/figures/*.pdf
           qje_2022_replication_data_and_outputs/data/raw/qje_2022_full_replication_package/figures/figure9_*.pdf
Writes to: qje_2022_replication_data_and_outputs/outputs/figures/png/*.png
-----------------------------------------------------------------------------------------------------------
"""

from pathlib import Path
import subprocess
import sys

assert len(sys.argv) in (2, 3), "Usage: render_figure_pdfs_to_png.py FIGURES_DIR [REPLICATION_PACKAGE_FIGURES_DIR]"

FIGURES = Path(sys.argv[1])
PNG_DIR = FIGURES / "png"

for pdf in sorted(FIGURES.glob("*.pdf")):
    subprocess.run(
        ["sips", "-s", "format", "png", str(pdf), "--out", str(PNG_DIR / f"{pdf.stem}.png")],
        check=True,
        stdout=subprocess.DEVNULL,
    )
    print(f"🎃 {pdf.name} -> png/{pdf.stem}.png")

if len(sys.argv) == 3:
    for gap in ("white", "male"):
        subprocess.run(
            [
                "sips",
                "-s",
                "format",
                "png",
                str(Path(sys.argv[2]) / f"figure9_{gap}.pdf"),
                "--out",
                str(PNG_DIR / f"figure9_replication_package_{gap}.png"),
            ],
            check=True,
            stdout=subprocess.DEVNULL,
        )
        print(f"🎃 figure9_{gap}.pdf -> png/figure9_replication_package_{gap}.png")

print(f"🎃 Rendered {len(list(PNG_DIR.glob('*.png')))} PNGs in {PNG_DIR}")
