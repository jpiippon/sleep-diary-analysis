# AGENTS.md

## Repository purpose
This repository contains R scripts for cleaning, joining, and analyzing sleep diary and sleep-environment data.

## Working style
- Make minimal changes.
- Do not rewrite entire scripts unless explicitly asked.
- Do not rename exported objects or output objects unless explicitly asked.
- Do not change downstream script expectations without a clear reason and an inline comment.
- Preserve tidyverse style and prefer the native pipe `|>`.

## R coding conventions
- Prefer clear, readable tidyverse code.
- Avoid unnecessary loops when vectorized or tidyverse solutions are available.
- Keep variable names stable when they are already used in downstream scripts.
- Add short comments only where the logic is not obvious.
- Do not add new package dependencies unless necessary.

## Data handling
- Treat raw data files as immutable.
- Do not silently recode variables in ways that drop valid values.
- Preserve both raw coded variables and readable labeled variables when useful for analysis.
- Derive weekday from date variables when possible instead of relying on manually entered weekday fields.
- Keep date and time handling explicit and reproducible.

## Editing rules
- When editing a script, change only that script unless explicitly asked to update others.
- If a change may break another script, explain that clearly before or within the edit.
- Prefer small diffs.
- Keep the main numbered pipeline stable unless explicitly changing the core workflow.
- Put focused one-variable reporting scripts in `scripts/variable_specific/`.
- Put figures from focused one-variable reporting scripts in `figures/variable_specific/<variable_name>/`.

## Variable-specific reporting scripts
- Use `scripts/variable_specific/weekday.R` as the template for focused reporting scripts.
- Keep a consistent structure: settings, analysis data, descriptive summaries, visualizations, models, adjusted predictions, and reporting summary.
- Adapt the visualization and model form to the variable type.
- For categorical or ordered variables, prefer grouped summaries, boxplots, category-level rates, and factor-based regression terms.
- For numeric variables, prefer scatterplots, binned summaries, and linear or flexible functional forms when substantively justified.
- Do not force all variables into the exact same model if the measurement scale requires a different specification.

## Current variable-specific pattern
- Existing examples include `scripts/variable_specific/weekday.R`, `scripts/variable_specific/insomnia.R`, and `scripts/variable_specific/bedtime.R`.
- New variable-specific scripts should usually combine descriptive summaries, publication-ready figures, simple models, fixed-effect extensions where appropriate, saved CSV outputs, and a short reporting summary.
- Treat these scripts as reproducible reporting modules: one variable or theme, one clear research question, one coherent set of figures, and model outputs that can later support public-facing summaries.
- Do not use outcome-like variables as ordinary explanatory variables without considering timing and interpretation. For example, same-night insomnia should generally be treated as an outcome or descriptive co-occurrence measure rather than as a main predictor of same-night sleep duration.
- Keep outputs predictable: figures should go to `figures/variable_specific/<variable_name>/`, and tables should go to `outputs/variable_specific/<variable_name>/` when the script produces reusable tables.

## Execution and validation
- After editing R code, run `Rscript scripts/99_smoke_test.R`.
- If the smoke test fails, fix the issue and rerun the script until it passes.
- Do not claim success unless the smoke test completes without errors.
- If a requested change affects modeling code, prefer validating the smallest relevant script first before running heavier end-to-end scripts.
- Do not run long plotting or full analysis scripts unless needed for the task or explicitly requested.

## Script-specific expectations
- `scripts/01_load_main_data.R` must create `sleep_diary` and `df_clean`.
- `scripts/02_load_co2_temp_data.R` must create `dat_mittari` and `sensor_nights`.
- `scripts/03_join_relevant_data.R` must create `sleep_mittari` and `sleep_mittari_sensor`.
- Keep these object names stable unless explicitly asked to change them.

## Change discipline
- Prefer the smallest possible working diff.
- Preserve existing comments, structure, and output file names when they are already clear.
- When uncertain about variable meaning, do not infer silently; leave a short inline note or flag the uncertainty.