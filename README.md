# Sleep Tracking Analysis (R)

This repository contains code and data for analyzing sleep diary data.

## Structure
- `data/raw/` : raw data files (e.g. `loki.xlsx`)
- `scripts/` : R scripts for data cleaning, visualization, and modeling
- `figures/` : generated plots
- `outputs/` : analysis output files (currently empty)
- `archive/` : older/legacy scripts and notebooks (archived)
- `projects/` : additional project work (e.g. `Uni2024`)

## Quick start
1. Open `Uni.Rproj` in RStudio.
2. Run `scripts/01_load_main_data.R` to load and clean sleep diary data.
3. Run `scripts/02_load_co2_temp_data.R` to load environmental sensor data (CO2, temperature, humidity).
4. (Optional) Run `scripts/03_join_relevant_data.R` to merge sleep and sensor data.
5. Run `scripts/02_descriptives_and_plots.R` to generate plots in `figures/`.
6. Run `scripts/03_models.R` to fit simple models to the cleaned data.

## Data
- The main dataset is `data/loki.xlsx` (sleep diary data).

## Goal
Investigate factors that affect sleep duration and related outcomes.
