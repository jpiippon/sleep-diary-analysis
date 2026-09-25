# Sleep diary analysis

R analyses of my long-term sleep diary and bedroom sensor measurements. The
project examines how coffee, bedtime, exercise, stress, illness, and the sleep
environment relate to sleep within one person.

Each focused report produces a main figure, supporting figures, and model
summaries. Coffee reports examine previous-night sleep, 3- and 5-night sleep
history, and medication. The magnesium report uses the proposed experiment
period, starting one day before the first recorded use.

## Run an analysis

Open `Uni.Rproj`. Install `tidyverse`, `readxl`, `here`, `fixest`, `patchwork`,
`gridExtra`, and `zoo`. Place the private input files in `data/raw/`:
`loki.xlsx` and, for environmental analyses, `mittari_kaikki.xlsx`.

```r
source("scripts/variable_specific/coffee.R")
source("scripts/variable_specific/magnesium.R")
```

Figures are saved under `outputs/figures/variable_specific/`. To run the core
pipeline, use `scripts/run_core_pipeline.R`. Automated checks use synthetic
data; private diary and sensor files are not included in the repository.

## Interpretation

The diary date identifies the exposure day and the night starting that day.
Sensor nights run from 21:00 to 08:00 in Helsinki time. Unsupported values
exclude the entire diary row and are reported during loading. Zero-hour sleep
is valid. Models account for calendar timing and serial dependence where
appropriate; observational associations do not establish causation.

See [analysis notes](docs/analysis_notes.md) for coding, exclusions, and model
assumptions.

Johannes Piipponen
