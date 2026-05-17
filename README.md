# Sleep diary analysis

This repository contains a longitudinal sleep diary project that combines self-tracked sleep records with nightly bedroom sensor data on CO2, temperature, and humidity.

The aim is to study how behavioral, temporal, and environmental factors are associated with night-to-night variation in sleep duration and insomnia-related outcomes. The repository is organized as a reproducible analysis workflow, from raw data cleaning to descriptive analysis, regression modeling, and variable-specific reporting.

## Study design

The project is based on within-person longitudinal data. The analytical workflow combines:

- sleep diary entries
- nightly environmental sensor measurements
- reproducible data cleaning and variable construction
- descriptive visualization
- regression-based modeling
- variable-specific reporting analyses

The empirical focus is on variation across nights rather than differences between individuals.

## Repository structure

- `data/raw/`: raw input files
- `scripts/`: main data cleaning, joining, descriptive, and modeling scripts
- `scripts/variable_specific/`: focused reporting scripts for individual variables
- `figures/`: generated figures from the main workflow
- `figures/variable_specific/`: generated figures from variable-specific scripts
- `outputs/`: optional model summaries and other analysis outputs, used only when a script has a clear need for reusable tables
- `archive/`: older exploratory material
- `projects/`: additional project-specific work

## Analysis workflow

Run the main scripts in numeric order.

1. `scripts/01_load_main_data.R`  
   Loads and cleans the sleep diary data.

2. `scripts/02_load_co2_temp_data.R`  
   Loads and aggregates nightly sensor data.

3. `scripts/03_join_relevant_data.R`  
   Joins sleep diary data with nightly sensor summaries.

4. `scripts/04_descriptives_and_plots.R`  
   Produces descriptive summaries and figures.

5. `scripts/05_models.R`  
   Fits baseline regression models for sleep duration and insomnia.

6. `scripts/06_fixed_effects_models.R`  
   Extends the modeling strategy with time fixed effects and sensor-based specifications.

7. `scripts/99_smoke_test.R`  
   Runs a lightweight validation check after code changes.

## Variable-specific reporting scripts

Focused reporting scripts are stored in `scripts/variable_specific/`. These scripts are not part of the required numbered pipeline. They are intended for deeper analysis of one variable at a time while keeping a consistent structure across outputs.

Current examples include:

- `scripts/variable_specific/bedtime.R`
- `scripts/variable_specific/weekday.R`
- `scripts/variable_specific/insomnia.R`
- `scripts/variable_specific/exercise.R`

The recommended output from each variable-specific script is a numbered figure set saved under `figures/variable_specific/<variable_name>/`. The main figure should use the name `<variable>_figure1_main.png`; supporting figures should use `<variable>_figureS*.png`.

Variable-specific scripts should print useful summaries and model output to the console. They should not create CSV tables or `outputs/variable_specific/<variable_name>/` files unless reusable tables are explicitly needed. This keeps the workflow lighter and reduces unnecessary maintenance when using AI-assisted coding.

The same structure can be adapted for other categorical, ordered, or numeric variables. Categorical variables typically use grouped summaries, boxplots, category-level rates, and factor-based models. Numeric variables typically use scatterplots, binned summaries, and linear or flexible functional forms.

## Methods currently implemented

The repository currently includes:

- cleaning and harmonizing raw sleep diary data
- parsing and aggregating nightly sensor observations
- date-based joining of diary and sensor data
- descriptive analysis of sleep patterns
- baseline regression models
- time fixed-effects models for within-series analysis
- variable-specific reporting scripts for focused interpretation

## Reproducibility

The repository is designed as a script-based workflow. Raw data are treated as immutable, and analysis-ready objects are created through the scripts in `scripts/`.

Because the project uses personal tracking data, raw input files may not always be suitable for public redistribution. The code structure is therefore intended to keep the analytical workflow transparent and reproducible even when data access is restricted.

## Current status

The repository currently supports:

- data preparation
- descriptive analysis
- baseline modeling
- fixed-effects extensions for within-person inference
- variable-specific reporting analyses

Ongoing work focuses on improving model specification, validation, and presentation of results.

## Author

Johannes Piipponen

Quantitative research workflow for sleep diary and environmental data analysis.