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
- Preserve zero-hour sleep records. They are valid observed nights, not missing or invalid durations.
- Derive weekday from date variables when possible instead of relying on manually entered weekday fields.
- Keep date and time handling explicit and reproducible.
- Preserve the diary date convention: the date refers to the exposure day and the night that starts on that date. For example, sleep entered on Wednesday morning belongs to Tuesday if Tuesday contains the relevant bedtime, coffee, exercise, stress, and other exposures.
- Do not shift diary dates forward to the wake-up date unless explicitly asked. Sensor observations after midnight should remain assigned to the previous calendar day when they belong to the same night window.
- `puhelinparkki = 0` means that the phone was not parked. Codes 1, 2, and 3 mean parked before 20:00, 21:00, and 22:00, respectively.
- Preserve raw `aivotyo`, but use a binary analysis variable where 0 means no evening brainwork and any positive value means evening brainwork.
- Do not recode or analyze `tukevaruoka = 1` until its meaning has been documented.

## Temporal interpretation and lagged sleep
- Previous sleep can affect current-day behaviors and the following night of sleep. This is especially relevant for variables such as coffee, where coffee intake may respond to poor sleep on the previous night.
- Lagged sleep variables such as previous-night sleep duration and two-night sleep debt may be useful sensitivity checks, controls, or descriptive mechanisms.
- Do not add lagged sleep controls automatically to every variable-specific script. Add them only when the user asks, when the research question clearly requires them, or as a clearly labelled sensitivity analysis.
- When using lags, preserve the diary date convention and require exact calendar spacing. A one-night lag must be exactly one calendar day earlier; never let row order bridge missing diary dates. Use `lag_by_calendar_days()` for numeric diary variables.
- Avoid interpreting models with lagged sleep as causal unless the timing and assumptions are explicitly discussed.

## Time-series inference
- Daily observations may have serially correlated errors. Final `fixest` reporting models should use panel Newey-West standard errors with a seven-day lag, uninterrupted sequence as the unit, and diary date as the time index: `vcov = NW(7) ~ series_id + date`.
- Keep the data sorted by date and verify that the date is unique before fitting a Newey-West model. Use `series_id` to separate uninterrupted daily sequences so covariance estimates do not bridge missing diary dates. Run `prepare_nw_data()` after model-specific complete-case filtering to sort the sample and recompute `series_id`.
- Heteroskedasticity-only standard errors may be retained as a sensitivity comparison, but they should not be the sole uncertainty estimate in final variable-specific reports.

## Sensitivity analyses inside variable-specific scripts
- Each variable-specific script may later include a clearly marked sensitivity-analysis section after the main figures and main models.
- Keep the main analysis simple and comparable across variables. Sensitivity analyses should support or qualify the main result, not replace it.
- Useful sensitivity analyses include lagged sleep controls, alternative exposure codings, alternative thresholds, and a small number of theoretically motivated interactions.
- Fixed effects do not automatically estimate interactions. Add explicit interaction terms only when needed, for example `temp_high25 * coffee` or the corresponding `fixest::i()` specification.
- Prefer visualizing interactions with predicted values or grouped plots rather than relying only on interaction coefficient tables.
- Do not test all possible interactions by default. A small number of theory-driven sensitivity checks is preferable to a large exploratory grid.
- Save sensitivity figures with supporting names such as `<variable>_figureS*_sensitivity_*.png`.
- Print sensitivity results to the console unless the user explicitly asks for reusable tables.

## Editing rules
- When editing a script, change only that script unless explicitly asked to update others.
- If a change may break another script, explain that clearly before or within the edit.
- Prefer small diffs.
- Keep the main numbered pipeline stable unless explicitly changing the core workflow.
- Put focused one-variable reporting scripts in `scripts/variable_specific/`.
- Put figures from focused one-variable reporting scripts in `outputs/figures/variable_specific/<variable_name>/`.
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
- Keep outputs predictable: figures should go to `outputs/figures/variable_specific/<variable_name>/`. Avoid table outputs unless there is a clear downstream use.
- Do not save the same plot under multiple filenames. Do not save individual main-figure panels separately unless they have a clear reporting use.
- Use numbered figure names for the recommended figure set: `<variable>_figure1_main.png` for the main figure and `<variable>_figureS*.png` for supporting figures.

## Future predictive modeling layer
- A possible later extension is `scripts/predictive_models/`, focused on prediction rather than causal interpretation.
- Do not create or modify predictive modeling scripts unless the user explicitly asks for that phase.
- If requested later, keep predictive models separate from the variable-specific reporting scripts.
- Use language such as "predictors", "associations", "out-of-sample performance", and "variable importance" rather than causal claims.
- Prefer interpretable benchmarking first: regularized regression, random forest or gradient boosting, model comparison, variable importance, and partial-dependence or accumulated-local-effects style visualizations when appropriate.

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
