"""
-----------------------------------------------------------------------------------------------------------
Purpose: Parse coefficient points from the QJE 2022 Figure 9 paper PDFs

Created: Nico Rotundo 2026-05-05
-----------------------------------------------------------------------------------------------------------
"""

# Import standard libraries
from pathlib import Path
import re
import sys
import zlib

# Import external libraries
import pandas as pd

# Add code root to Python module path
sys.path.insert(0, str(Path(__file__).parent.parent))

# Import project paths
from globals import (
    qje_2022_replication_dump,
    qje_2022_replication_package,
)

"""
-----------------------------------------------------------------------------------------------------------
Assign Figure 9 row labels and point colors to Python objects
-----------------------------------------------------------------------------------------------------------
"""

# Create a list containing Figure 9 row labels in their top-to-bottom order in the paper PDFs
paper_rows = [
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

# Create a dictionary that translates each point's RGB color triple into the corresponding estimate label
color_to_estimate = {
    (0.10196, 0.27843, 0.43529): "posterior_mean",
    (0.56471, 0.20784, 0.23137): "linear_shrinkage",
    (0.10196, 0.52157, 1.0): "posterior_mean",
    (0.83137, 0.06667, 0.34902): "linear_shrinkage",
}

"""
-----------------------------------------------------------------------------------------------------------
Loop over paper Figure 9 panel PDFs and extract plotted coefficient points 
with their corresponding labels
-----------------------------------------------------------------------------------------------------------
"""

# Create an empty list that will store one parsed coefficient row per figure point
paper_point_rows = []

# Loop over the white-Black and male-female Figure 9 paper panels
for gap, pdf_path in (
    ("white", qje_2022_replication_package / "figures" / "figure9_white.pdf"),
    ("male", qje_2022_replication_package / "figures" / "figure9_male.pdf"),
):

    # -----------------------------------------------------------------------------------------
    # Read PDF drawing instructions for this panel
    # -----------------------------------------------------------------------------------------

    # Read the current panel PDF as raw bytes
    pdf_bytes = pdf_path.read_bytes()

    # Extract the compressed PDF block that stores the panel's drawing instructions
    pdf_streams = re.findall(rb"stream\r?\n(.*?)\r?\nendstream", pdf_bytes, re.S)
    if len(pdf_streams) != 1:
        raise ValueError(f"Expected one compressed drawing-instruction block in {pdf_path}; found {len(pdf_streams)}")

    # Decode the current panel's drawing instructions into searchable text
    pdf_content = zlib.decompress(pdf_streams[0]).decode("latin1")

    # Split the drawing instructions into one line per PDF command
    pdf_lines = pdf_content.splitlines()

    # -----------------------------------------------------------------------------------------
    # Recover x-axis mapping from PDF coordinates to coefficient values
    # -----------------------------------------------------------------------------------------

    # Create a list of numeric text labels and their PDF coordinates
    text_ticks = []
    for match in re.finditer(r"1 0 0 1 ([0-9.]+) ([0-9.]+) Tm\n\((-?[0-9]+)\) Tj", pdf_content):
        text_ticks.append((float(match.group(1)), float(match.group(2)), int(match.group(3))))
    if len(text_ticks) == 0:
        raise ValueError(f"No numeric PDF text labels found in {pdf_path}")

    # Identify the y-coordinate of the bottom row of numeric labels
    tick_label_y = min(y for _, y, _ in text_ticks)

    # Keep the numeric labels that sit on the x-axis
    tick_labels = sorted(
        [(x, value) for x, y, value in text_ticks if abs(y - tick_label_y) < 10],
        key=lambda item: item[0],
    )

    # Create a list of vertical tick/grid lines and their PDF coordinates
    vertical_ticks = []
    for match in re.finditer(r"\n([0-9.]+) ([0-9.]+) m\n\1 ([0-9.]+) l\nS", pdf_content):
        x = float(match.group(1))
        y_one = float(match.group(2))
        y_two = float(match.group(3))
        if 250 <= abs(y_one - y_two) <= 400:
            vertical_ticks.append((x, max(y_one, y_two), min(y_one, y_two)))
    if len(vertical_ticks) == 0:
        raise ValueError(f"No vertical tick/grid lines found in {pdf_path}")

    # Keep candidate vertical lines close enough to the x-axis label row
    tick_x_candidates = [(x, y_top, y_bottom) for x, y_top, y_bottom in vertical_ticks if y_bottom < tick_label_y + 1000]
    if len(tick_x_candidates) == 0:
        raise ValueError(f"No x-axis tick/grid line candidates found in {pdf_path}")

    # Identify the panel baseline as the lowest top endpoint among candidate vertical lines
    baseline_y = min(y_top for _, y_top, _ in tick_x_candidates)

    # Keep the vertical lines whose top endpoint sits on the panel baseline
    tick_xs = sorted(x for x, y_top, _ in tick_x_candidates if abs(y_top - baseline_y) < 10)
    if len(tick_xs) != len(tick_labels):
        raise ValueError(f"Tick count mismatch in {pdf_path}: {len(tick_xs)} tick marks, {len(tick_labels)} labels")

    # Identify the horizontal x-axis line that marks the left and right plot boundaries
    horizontal_axis_segments = []
    for line_index, line in enumerate(pdf_lines):
        line_start_match = re.fullmatch(r"([0-9.]+) ([0-9.]+) m", line)
        if line_start_match is None or line_index + 2 >= len(pdf_lines):
            continue
        line_end_match = re.fullmatch(r"([0-9.]+) ([0-9.]+) l", pdf_lines[line_index + 1])
        if line_end_match is None or pdf_lines[line_index + 2] != "S":
            continue
        if abs(float(line_start_match.group(2)) - baseline_y) >= 10 or float(line_start_match.group(2)) != float(line_end_match.group(2)):
            continue
        x_left = min(float(line_start_match.group(1)), float(line_end_match.group(1)))
        x_right = max(float(line_start_match.group(1)), float(line_end_match.group(1)))
        if x_right - x_left > 10000:
            horizontal_axis_segments.append((x_left, x_right))
    if len(horizontal_axis_segments) != 1:
        raise ValueError(f"Expected one horizontal x-axis line in {pdf_path}; found {len(horizontal_axis_segments)}")

    # Store the left and right x-coordinate bounds of the coefficient plotting region
    plot_x_min, plot_x_max = horizontal_axis_segments[0]

    # Create a list of coefficient values printed on the x-axis
    tick_values = [value for _, value in tick_labels]

    # Compute the linear map from PDF x-coordinate to coefficient value
    x_mean = sum(tick_xs) / len(tick_xs)
    value_mean = sum(tick_values) / len(tick_values)
    slope = sum((x - x_mean) * (value - value_mean) for x, value in zip(tick_xs, tick_values)) / sum((x - x_mean) ** 2 for x in tick_xs)
    intercept = value_mean - slope * x_mean

    # -----------------------------------------------------------------------------------------
    # Extract confidence intervals from horizontal line segments
    # -----------------------------------------------------------------------------------------

    # Create an empty list that will store parsed confidence interval bars from the current panel
    ci_bars = []

    # Initialize current PDF stroke color
    current_stroke_color = None

    # Loop over PDF drawing instruction lines with their line numbers
    for line_index, line in enumerate(pdf_lines):

        # Update the current stroke color when the PDF changes line color
        stroke_color_match = re.fullmatch(r"([0-9.]+) ([0-9.]+) ([0-9.]+) RG", line)
        if stroke_color_match:
            current_stroke_color = tuple(round(float(stroke_color_match.group(i)), 5) for i in range(1, 4))
            continue

        # Skip lines that do not begin a colored horizontal line segment
        if current_stroke_color not in color_to_estimate:
            continue
        line_start_match = re.fullmatch(r"([0-9.]+) ([0-9.]+) m", line)
        if line_start_match is None or line_index + 2 >= len(pdf_lines):
            continue

        # Keep only horizontal line segments that are immediately stroked by the PDF
        line_end_match = re.fullmatch(r"([0-9.]+) ([0-9.]+) l", pdf_lines[line_index + 1])
        if line_end_match is None or pdf_lines[line_index + 2] != "S":
            continue
        if float(line_start_match.group(2)) != float(line_end_match.group(2)):
            continue

        # Store line-segment endpoints and vertical position
        x_left = min(float(line_start_match.group(1)), float(line_end_match.group(1)))
        x_right = max(float(line_start_match.group(1)), float(line_end_match.group(1)))
        y = float(line_start_match.group(2))

        # Keep only confidence interval bars inside the coefficient plotting region
        if y <= baseline_y or not (plot_x_min <= x_left <= plot_x_max):
            continue
        if not (plot_x_min <= x_right <= plot_x_max):
            continue

        # Convert PDF x-coordinates into confidence interval endpoints and standard error
        ci_lower = intercept + slope * x_left
        ci_upper = intercept + slope * x_right
        ci_bars.append({
            "estimate_type": color_to_estimate[current_stroke_color],
            "y": y,
            "ci_lower": ci_lower,
            "ci_upper": ci_upper,
            "paper_se": (ci_upper - ci_lower) / (2 * 1.96),
        })

    # -----------------------------------------------------------------------------------------
    # Extract coefficient point estimates from filled circle markers
    # -----------------------------------------------------------------------------------------

    # Create an empty list that will store plotted point locations from the current panel
    circles = []

    # Initialize current PDF fill color and current shape-drawing instructions
    current_color = None
    path_lines = []

    # Loop over each line of the PDF drawing instructions
    for line in pdf_lines:

        # Update the current fill color when the PDF changes drawing color
        color_match = re.fullmatch(r"([0-9.]+) ([0-9.]+) ([0-9.]+) rg", line)
        if color_match:
            current_color = tuple(round(float(color_match.group(i)), 5) for i in range(1, 4))

        # Collect PDF curve and move commands that can define a circle shape
        if re.search(r" [mc]$", line):
            path_lines.append(line)
            continue

        # Parse the collected shape once the PDF fills it
        if line == "f":
            block = "\n".join(path_lines)
            path_lines = []
            if " c" not in block or current_color not in color_to_estimate:
                continue

            # Extract all x/y coordinates used to draw the filled shape
            numbers = [float(number) for number in re.findall(r"-?[0-9.]+", block)]
            xs = numbers[0::2]
            ys = numbers[1::2]
            if len(xs) == 0 or len(ys) == 0:
                continue

            # Calculate the filled shape's bounding box and center point
            width = max(xs) - min(xs)
            height = max(ys) - min(ys)
            x = (max(xs) + min(xs)) / 2
            y = (max(ys) + min(ys)) / 2

            # Keep only circular point markers inside the coefficient plotting region
            if abs(width - height) > 5 or not (150 <= width <= 500):
                continue
            if y <= baseline_y or not (plot_x_min <= x <= plot_x_max):
                continue

            # Add the parsed point marker to the current panel's point list
            circles.append({
                "estimate_type": color_to_estimate[current_color],
                "x": x,
                "y": y,
                "coefficient": intercept + slope * x,
            })

        # Reset collected shape instructions when a non-shape drawing command appears
        elif line not in {"S", "B", "Q"}:
            path_lines = []

    # -----------------------------------------------------------------------------------------
    # Attach Figure 9 row labels and confidence intervals to point estimates
    # -----------------------------------------------------------------------------------------

    # Loop over the two estimate types shown in each paper Figure 9 panel
    for estimate_type in ("posterior_mean", "linear_shrinkage"):

        # Sort the current estimate type's points from top to bottom in the panel
        points = sorted(
            [circle for circle in circles if circle["estimate_type"] == estimate_type],
            key=lambda item: item["y"],
            reverse=True,
        )

        # Sort the current estimate type's confidence intervals from top to bottom in the panel
        cis = sorted(
            [ci_bar for ci_bar in ci_bars if ci_bar["estimate_type"] == estimate_type],
            key=lambda item: item["y"],
            reverse=True,
        )

        # Confirm the PDF parser found one point for each Figure 9 row
        if len(points) != len(paper_rows):
            raise ValueError(f"{gap} {estimate_type}: found {len(points)} points, expected {len(paper_rows)}")
        if len(cis) != len(paper_rows):
            raise ValueError(f"{gap} {estimate_type}: found {len(cis)} confidence intervals, expected {len(paper_rows)}")

        # Attach Figure 9 row labels to parsed point coefficients and confidence intervals
        for characteristic, point, ci in zip(paper_rows, points, cis):

            # Confirm the point and confidence interval are on the same Figure 9 row
            if abs(point["y"] - ci["y"]) > 500:
                raise ValueError(f"{gap} {estimate_type} {characteristic}: point and confidence interval y-coordinates do not align")

            paper_point_rows.append({
                "gap": gap,
                "characteristic": characteristic,
                "estimate_type": estimate_type,
                "paper_coefficient": point["coefficient"],
                "paper_ci_lower": ci["ci_lower"],
                "paper_ci_upper": ci["ci_upper"],
                "paper_se": ci["paper_se"],
            })

"""
-----------------------------------------------------------------------------------------------------------
Save parsed paper Figure 9 points
-----------------------------------------------------------------------------------------------------------
"""

# Convert parsed point rows into a dataframe
paper_points = pd.DataFrame(paper_point_rows)

# Confirm the parser produced one row per gap-characteristic-estimate type
assert len(paper_points) == 44, f"Expected 44 parsed paper Figure 9 rows; found {len(paper_points)}"
assert not paper_points.duplicated(["gap", "characteristic", "estimate_type"]).any(), "Parsed paper Figure 9 rows are not unique"

# Sort parsed rows into a stable order before export
paper_points["characteristic_order"] = paper_points["characteristic"].map({characteristic: order for order, characteristic in enumerate(paper_rows)})
paper_points["estimate_type_order"] = paper_points["estimate_type"].map({"posterior_mean": 0, "linear_shrinkage": 1})
paper_points = paper_points.sort_values(["gap", "characteristic_order", "estimate_type_order"])
paper_points = paper_points.drop(columns=["characteristic_order", "estimate_type_order"])

# Save parsed paper points in Stata format
paper_points.to_stata(qje_2022_replication_dump / "figure9_paper_pdf_points.dta", write_index=False)

# Print the Stata output path for the metafile log
print(f"🎃 Parsed paper Figure 9 points: {qje_2022_replication_dump / 'figure9_paper_pdf_points.dta'}")
