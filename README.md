# Sleep diary analysis

R analyses of my long-term sleep diary and bedroom sensor measurements. The
project examines how coffee, bedtime, exercise, stress, illness, and the sleep
environment relate to sleep within one person.

Each focused report produces a main figure, supporting figures, and model
summaries. Coffee reports examine previous-night sleep, 3- and 5-night sleep
history, and medication. The magnesium report uses the proposed experiment
period, starting one day before the first recorded use.

## Project layout

- `data/raw/`: private input files; never commit these data.
- `data/derived/`: intermediate datasets generated during analysis.
- Numbered scripts under `scripts/`: ordered data preparation and core analyses; lag analysis is separate.
- `scripts/run_core_pipeline.R` and `scripts/run_variable_specific_reports.R`: analysis entry points.
- `scripts/variable_specific/`: focused reports for individual variables or themes.
- `scripts/helpers/`: shared validation, date, and analysis helpers.
- `tests/`: synthetic-data checks for the pipeline and reports.
- `ANALYSIS_NOTES.md`: analysis decisions, exclusions, and interpretation notes.
- `outputs/figures/core/`: figures from the numbered core pipeline.
- `outputs/figures/variable_specific/`: figures from focused reports.
- `outputs/synthetic_test_previews/`: temporary previews produced by report tests.
- `archive/`: retired scripts; not part of the analysis workflow.

The root-level `figures/` directory contains legacy generated images only;
active scripts now save figures under `outputs/figures/`.

## Run analyses

Open `Uni.Rproj`. Install `tidyverse`, `readxl`, `here`, `fixest`, `patchwork`,
`gridExtra`, and `zoo`. Place the private input files in `data/raw/`:
`loki.xlsx` and, for environmental analyses, `mittari_kaikki.xlsx`.

Run the core pipeline or all focused reports with:

```r
source("scripts/run_core_pipeline.R")
source("scripts/run_variable_specific_reports.R")
```

To run one focused report, source its script directly, for example
`scripts/variable_specific/coffee.R`. Automated checks use synthetic data;
private diary and sensor files are not included in the repository.

## Interpretation

The diary date identifies the exposure day and the night starting that day.
Sensor nights run from 21:00 to 08:00 in Helsinki time. Unsupported values
exclude the entire diary row and are reported during loading. Zero-hour sleep
is valid. Models account for calendar timing and serial dependence where
appropriate; observational associations do not establish causation.

See [analysis notes](ANALYSIS_NOTES.md) for coding, exclusions, and model
assumptions.

Johannes Piipponen
