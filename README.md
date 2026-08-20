# Sleep diary analysis

This repository contains an analysis of my long-term sleep diary. The data cover about 3,000 nights. 

The main goal is simple: I want to understand which things are linked to better or worse sleep. Examples include bedtime, weekday, coffee, exercise, stress, illness, bedroom temperature, CO2, and humidity.

The aim is to build clear, reproducible R code and figures that can later be used in reports, presentations, or public posts. 

## What the data include

The project uses two main sources of data:

- my sleep diary, where I record sleep duration and daily factors such as coffee, exercise, stress, illness, bedtime, and insomnia
- bedroom sensor data, where temperature, CO2, and humidity are measured during the night

The focus is on changes from night to night within the same person. This means the project does not compare different people. It asks, for example, whether my sleep is different on nights after coffee, late bedtime, exercise, stress, or high bedroom temperature.

## Important date convention

The diary date means the day before the sleep period ends. For example, if I wake up on Wednesday morning, the sleep is recorded under Tuesday when Tuesday was the day with the relevant bedtime, coffee, exercise, and stress.

This is intentional. It keeps daily factors and the following night of sleep on the same diary date.

The sensor data use the same idea. Sensor readings after midnight, for example 00:00-07:59, are assigned to the previous calendar day because they still belong to the same night.

## Coding notes

- A sleep duration of zero hours is a valid observed night and is retained in the analysis.
- `puhelinparkki = 0` means that the phone was not parked. Values 1, 2, and 3 mean parked before 20:00, 21:00, and 22:00, respectively.
- The raw `aivotyo` value is preserved. The analysis-ready binary variable `brainwork_any` treats zero as no evening brainwork and any positive value as evening brainwork.
- `kipea = 1` means that I was clearly sick during the diary day. `kipea = 2` means that a child was sick, but child illnesses were recorded inconsistently.
- `child_night_context` summarizes the raw `vauvahuoneessa` sleeping arrangement. It is not used as a proxy for child illness.
- Lagged variables are matched by exact calendar date. A missing diary date is not treated as an adjacent night.

## Repository structure

- `data/raw/`: original input files
- `scripts/`: main R scripts for loading, cleaning, joining, plotting, and modeling the data
- `scripts/variable_specific/`: focused scripts that study one variable at a time
- `outputs/figures/`: generated figures
- `outputs/figures/variable_specific/`: figures from the one-variable scripts
- `outputs/variable_specific/`: optional reusable tables when an analysis needs them

## Main analysis workflow

The main scripts are designed to run in numeric order.

1. `scripts/01_load_main_data.R`  
   Loads and cleans the sleep diary.

2. `scripts/02_load_co2_temp_data.R`  
   Loads the bedroom sensor data and creates night-level summaries.

3. `scripts/03_join_relevant_data.R`  
   Combines the sleep diary with the sensor data.

4. `scripts/04_descriptives_and_plots.R`  
   Creates general summary figures and descriptive results.

5. `scripts/05_models.R`  
   Runs baseline models for sleep duration and insomnia.

6. `scripts/06_fixed_effects_models.R`  
   Runs models that compare nights within the same time period.

7. `scripts/99_smoke_test.R`  
   Checks that the main data pipeline still runs after code changes.

The main pipeline can also be run with:

```r
source("scripts/run_core_pipeline.R")
```

## One-variable analyses

The most important part of the project is the set of focused scripts in `scripts/variable_specific/`. Each script studies one variable or one closely related theme.

Current examples include:

- `bedtime.R`: bedtime and sleep
- `weekday.R`: weekday differences in sleep
- `insomnia.R`: insomnia patterns
- `exercise.R`: exercise and sleep
- `brainwork.R`: demanding thinking after 19:00, bedtime, and sleep
- `stress.R`: recorded evening stress, insomnia patterns, bedtime, and sleep
- `health.R`: own illness episodes, sleep duration, bedtime, and difficulty falling asleep
- `temperature.R`: bedroom temperature and sleep
- `coffee.R`: coffee, previous-night sleep, and same-night sleep
- `coffee_relationships.R`: coffee/no coffee, bedtime, exercise, and context checks

Each one-variable script usually creates:

- one main figure
- a small set of supporting figures
- simple model results printed to the console
- sensitivity checks when they are useful

The main figure is named like this:

```text
<variable>_figure1_main.png
```

Supporting figures are named like this:

```text
<variable>_figureS*.png
```

For example:

```text
coffee_figure1_main.png
coffee_figureS20_context_bedtime.png
```

The one-variable scripts can be run with:

```r
source("scripts/run_variable_specific_reports.R")
```

Or one script at a time, for example:

```bash
Rscript scripts/variable_specific/coffee.R
Rscript scripts/variable_specific/temperature.R
```

## Modeling approach

The models are meant to support interpretation, not to prove cause and effect.

Variable-specific analyses can use three model layers when they suit the research question:

- raw models, which show the simple association
- calendar-adjusted models, which account for weekday and time-period differences
- fully adjusted models, which also include relevant diary variables

The exact model and adjustment variables depend on the exposure. They should not be copied mechanically from one analysis to another.

Some scripts also include sensitivity checks. For example, the coffee analysis checks whether coffee is related to sleep after controlling for previous-night sleep. This is important because coffee may be a response to poor sleep, not only a possible cause of poor sleep.

Final `fixest` models use seven-day Newey-West standard errors. The model sample is split into uninterrupted daily sequences after complete-case filtering so uncertainty estimates do not bridge missing diary dates.

The wording in the project should therefore stay careful. It is better to write:

> Coffee is associated with shorter sleep on some nights.

than:

> Coffee causes shorter sleep.

## Author

Johannes Piipponen

