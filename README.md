# Terrain Map Generator for OpenFront

This project generates terrain maps with elevation, slope, and river features from real-world elevation data. It produces PNG files formatted for use with the [OpenFront](https://github.com/openfrontio/OpenFrontIO) game engine.

## Features

- Downloads and processes elevation data for any specified latitude/longitude bounding box
- Computes terrain classification (water, plains, highlands, mountains) using elevation and slope
- Adds rivers with variable widths based on river significance (`scalerank`)
- Outputs RGBA PNG files compatible with OpenFront’s terrain rendering pipeline
- Generates a preview PNG that approximates how OpenFront interprets terrain colors

## Requirements

- R (>= 4.0)
- R packages:
  - `terra`
  - `sf`
  - `elevatr`
  - `png`

Install packages via:

```r
install.packages(c("terra", "sf", "elevatr", "png"))

## Usage

1. Open `loadelevation.R` and input your desired bounding box coordinates and any other parameters.
2. Run `loadelevation.R`. This script will:
   - Download and cache the elevation data if needed.
   - Generate the terrain map PNG (`*.png`).
   - Generate a preview PNG (`*_preview.png`) approximating how OpenFront interprets the terrain colors.

**Note:** The preview PNG is an approximation and does **not** include shoreline processing or the removal of small islands and lakes done in OpenFront’s full pipeline.

3. Use the generated terrain PNG in the OpenFront game engine.
