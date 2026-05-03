#!/usr/bin/env python3
# Compare paper Figure 9 (parsed from shipped PDF) to our rebuilds (numeric
# coefficients saved by create_figure_9_cps{,_new_cleaning}.do). Called from
# the Stata metafile with three positional path arguments:
#   argv[1]: INPUTS_DIR --- holds the paper PDFs (shipped Figure 9 outputs)
#   argv[2]: DUMP_DIR   --- holds the rebuild coefficient CSVs
#   argv[3]: TABLES_DIR --- destination for the comparison CSV

from pathlib import Path
import csv
import re
import sys
import zlib


INPUTS_DIR = Path(sys.argv[1])
DUMP_DIR = Path(sys.argv[2])
TABLES_DIR = Path(sys.argv[3])


PAPER_ROWS = [
    "wage_level_race",
    "wage_gap_race",
    "share_black_ind",
    "mgmt_dif_black_ind",
    "col_share_gap_race",
    "wage_level_gender",
    "wage_gap_gender",
    "share_female_ind",
    "mgmt_dif_female_ind",
    "col_share_gap_gender",
    "top4share",
]

CPS_ROWS = [
    "wage_level_race",
    "wage_gap_race",
    "col_share_gap_race",
    "wage_level_gender",
    "wage_gap_gender",
    "col_share_gap_gender",
]

COLOR_TO_ESTIMATE = {
    (0.10196, 0.27843, 0.43529): "posterior_mean",
    (0.56471, 0.20784, 0.23137): "linear_shrinkage",
    (0.10196, 0.52157, 1.0): "posterior_mean",
    (0.83137, 0.06667, 0.34902): "linear_shrinkage",
}


def decompress_pdf_stream(path):
    pdf_bytes = path.read_bytes()
    match = re.search(rb"stream\r?\n(.*?)\r?\nendstream", pdf_bytes, re.S)
    if match is None:
        raise ValueError(f"No compressed stream found in {path}")
    return zlib.decompress(match.group(1)).decode("latin1")


def extract_axis(content):
    text_ticks = []
    for match in re.finditer(r"1 0 0 1 ([0-9.]+) ([0-9.]+) Tm\n\((-?[0-9]+)\) Tj", content):
        text_ticks.append((float(match.group(1)), float(match.group(2)), int(match.group(3))))

    tick_label_y = min(y for _, y, _ in text_ticks)
    tick_labels = sorted(
        [(x, value) for x, y, value in text_ticks if abs(y - tick_label_y) < 10],
        key=lambda item: item[0],
    )

    vertical_ticks = []
    for match in re.finditer(r"\n([0-9.]+) ([0-9.]+) m\n\1 ([0-9.]+) l\nS", content):
        x = float(match.group(1))
        y_one = float(match.group(2))
        y_two = float(match.group(3))
        if 250 <= abs(y_one - y_two) <= 400:
            vertical_ticks.append((x, max(y_one, y_two), min(y_one, y_two)))

    tick_x_candidates = [(x, y_top, y_bottom) for x, y_top, y_bottom in vertical_ticks if y_bottom < tick_label_y + 1000]
    baseline_y = min(y_top for _, y_top, _ in tick_x_candidates)
    tick_xs = sorted(x for x, y_top, _ in tick_x_candidates if abs(y_top - baseline_y) < 10)

    if len(tick_xs) != len(tick_labels):
        raise ValueError(f"Tick count mismatch: {len(tick_xs)} tick marks, {len(tick_labels)} labels")

    tick_values = [value for _, value in tick_labels]
    x_mean = sum(tick_xs) / len(tick_xs)
    value_mean = sum(tick_values) / len(tick_values)
    slope = sum((x - x_mean) * (value - value_mean) for x, value in zip(tick_xs, tick_values)) / sum((x - x_mean) ** 2 for x in tick_xs)
    intercept = value_mean - slope * x_mean

    return tick_xs, baseline_y, slope, intercept


def extract_points(content, rows):
    tick_xs, baseline_y, slope, intercept = extract_axis(content)

    circles = []
    current_color = None
    path_lines = []

    for line in content.splitlines():
        color_match = re.fullmatch(r"([0-9.]+) ([0-9.]+) ([0-9.]+) rg", line)
        if color_match:
            current_color = tuple(round(float(color_match.group(i)), 5) for i in range(1, 4))

        if re.search(r" [mc]$", line):
            path_lines.append(line)
            continue

        if line == "f":
            block = "\n".join(path_lines)
            path_lines = []
            if " c" not in block or current_color not in COLOR_TO_ESTIMATE:
                continue

            numbers = [float(number) for number in re.findall(r"-?[0-9.]+", block)]
            xs = numbers[0::2]
            ys = numbers[1::2]
            if not xs or not ys:
                continue

            width = max(xs) - min(xs)
            height = max(ys) - min(ys)
            x = (max(xs) + min(xs)) / 2
            y = (max(ys) + min(ys)) / 2

            if abs(width - height) > 5 or not (150 <= width <= 500):
                continue
            if y <= baseline_y or not (min(tick_xs) - 1000 <= x <= max(tick_xs) + 1000):
                continue

            circles.append({
                "estimate_type": COLOR_TO_ESTIMATE[current_color],
                "x": x,
                "y": y,
                "coefficient": intercept + slope * x,
            })
        elif line not in {"S", "B", "Q"}:
            path_lines = []

    extracted = {}
    for estimate_type in ["posterior_mean", "linear_shrinkage"]:
        points = sorted(
            [circle for circle in circles if circle["estimate_type"] == estimate_type],
            key=lambda item: item["y"],
            reverse=True,
        )
        if len(points) != len(rows):
            raise ValueError(f"{estimate_type}: found {len(points)} points, expected {len(rows)}")

        for row, point in zip(rows, points):
            extracted[(row, estimate_type)] = point["coefficient"]

    return extracted


paper_points = {
    "white": extract_points(decompress_pdf_stream(INPUTS_DIR / "figures" / "figure9_white.pdf"), PAPER_ROWS),
    "male": extract_points(decompress_pdf_stream(INPUTS_DIR / "figures" / "figure9_male.pdf"), PAPER_ROWS),
}


def load_rebuild(csv_path):
    points = {}
    with csv_path.open() as file:
        for row in csv.DictReader(file):
            points[(row["gap"], row["characteristic"], row["estimate_type"])] = float(row["coefficient"])
    return points


cps_points = load_rebuild(DUMP_DIR / "figure9_cps_rebuild_coefficients.csv")
cps_new_cleaning_points = load_rebuild(DUMP_DIR / "figure9_cps_new_cleaning_rebuild_coefficients.csv")

with (TABLES_DIR / "figure9_cps_pdf_point_comparison.csv").open("w", newline="") as file:
    writer = csv.DictWriter(file, fieldnames=[
        "gap",
        "characteristic",
        "estimate_type",
        "paper_coefficient",
        "cps_coefficient",
        "cps_new_cleaning_coefficient",
        "difference_cps_vs_paper",
        "difference_new_cleaning_vs_paper",
        "difference_new_cleaning_vs_cps",
    ])
    writer.writeheader()

    for gap in ("white", "male"):
        for characteristic in CPS_ROWS:
            for estimate_type in ("posterior_mean", "linear_shrinkage"):
                paper_coefficient = paper_points[gap][(characteristic, estimate_type)]
                cps_coefficient = cps_points[(gap, characteristic, estimate_type)]
                cps_new_cleaning_coefficient = cps_new_cleaning_points[(gap, characteristic, estimate_type)]
                writer.writerow({
                    "gap": gap,
                    "characteristic": characteristic,
                    "estimate_type": estimate_type,
                    "paper_coefficient": paper_coefficient,
                    "cps_coefficient": cps_coefficient,
                    "cps_new_cleaning_coefficient": cps_new_cleaning_coefficient,
                    "difference_cps_vs_paper": cps_coefficient - paper_coefficient,
                    "difference_new_cleaning_vs_paper": cps_new_cleaning_coefficient - paper_coefficient,
                    "difference_new_cleaning_vs_cps": cps_new_cleaning_coefficient - cps_coefficient,
                })

print(TABLES_DIR / "figure9_cps_pdf_point_comparison.csv")
