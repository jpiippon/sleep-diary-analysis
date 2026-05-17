# AGENTS.md

## Repository purpose
This repository contains R scripts for cleaning, joining, and analyzing sleep diary and sleep-environment data.

## Working style
- Make minimal changes.
- Do not rewrite entire scripts unless explicitly asked.
- Do not rename exported objects or output objects unless explicitly asked.
- Do not change downstream script expectations without a clear reason and an inline comment.
- Preserve tidyverse style and prefer the native pipe `|>`.
- Prefer targeted edits to broad refactors. This keeps AI-assisted coding cheaper and easier to review.

## R coding conventions
- Prefer clear, readable tidyverse code.
- Avoid unnecessary loops when vectorized or tidyverse solutions are available.
- Keep variable names stable when they are already used in downstream scripts.
- Add short comments only where the logic is not obvious.
- Do not add new package dependencies unless necessary.
- Remove unused helper functions, output directories, and intermediate objects if they no longer serve the current analysis.

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
- Do not add saved table outputs merely because a summary object exists.

## Variable-specific reporting scripts
- Use the current best variable-specific scripts as templates, especially `scripts/variable_specific/bedtime.R` for categorical or ordered exposure variables.
- Keep a consistent structure: settings, analysis data, descriptive summaries, visualizations, models, and reporting summary.
- Adapt the visualization and model form to the variable type.
- For categorical or ordered variables, prefer grouped summaries, boxplots, category-level rates, and factor-based regression terms.
- For numeric variables, prefer scatterplots, binned summaries, and linear or flexible functional forms when substantively justified.
- Do not force all variables into the exact same model if the measurement scale requires a different specification.
- Save figures, but do not save CSV tables or `outputs/variable_specific/<variable_name>/` files unless the user explicitly asks for reusable tables.
- Print useful summaries to the console instead of writing them to disk by default.

## Current variable-specific pattern
- Existing examples include `scripts/variable_specific/bedtime.R`, `scripts/variable_specific/weekday.R`, `scripts/variable_specific/insomnia.R`, and `scripts/variable_specific/exercise.R`.
- New variable-specific scripts should usually combine descriptive summaries, publication-ready figures, simple models, fixed-effect extensions where appropriate, and a short reporting summary.
- Treat these scripts as reproducible reporting modules: one variable or theme, one clear research question, one coherent set of figures, and model outputs that can later support public-facing summaries.
- Do not use outcome-like variables as ordinary explanatory variables without considering timing and interpretation. For example, same-night insomnia should generally be treated as an outcome or descriptive co-occurrence measure rather than as a main predictor of same-night sleep duration.
- Keep outputs predictable: figures should go to `figures/variable_specific/<variable_name>/`. Avoid table/output directories unless there is a clear downstream use.
- Use numbered figure names for the recommended figure set: `<variable>_figure1_main.png` for the main figure and `<variable>_figureS*.png` for supporting figures.

## Cost-aware AI-assisted workflow
- For small fixes, ask for one script and one issue at a time.
- For new variables, copy the closest existing template and edit only the variable-specific parts.
- First make the script run, then improve figure clarity, then consider model refinements.
- Avoid asking the model to regenerate all scripts unless there is a deliberate cross-cutting refactor.
- Prefer model outputs in the console and saved figures over extra files that must later be maintained.

## Execution and validation
- After editing R code, run the smallest relevant script first, for example `Rscript scripts/variable_specific/bedtime.R`.
- Run `Rscript scripts/99_smoke_test.R` when changes may affect the shared data-loading pipeline or multiple scripts.
- If a script fails, fix the issue and rerun the smallest relevant script until it passes.
- Do not claim success unless the relevant script or smoke test completes without errors.
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