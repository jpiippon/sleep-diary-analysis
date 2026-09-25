# Analysis notes

## Dates and missing observations

The diary date is the exposure day. Sleep recorded on Wednesday morning belongs
to Tuesday if Tuesday contains the relevant coffee, bedtime, and other exposures.
Sensor summaries use 21:00–08:00 in `Europe/Helsinki`, with 08:00 excluded.
The previous-day adjustment is made to the local calendar date, including
daylight-saving transitions. Excel sensor timestamps are assumed to store local
clock time without timezone information.

Lags match exact calendar dates. A missing or excluded day never becomes an
adjacent observation. Three- and five-night sleep totals require every preceding
night and exclude the current night. They measure recent sleep without assuming
a personal sleep requirement.

## Validation

Raw files are immutable. `validate_diary_rows()` applies the domains recorded in
`scripts/helpers/analysis_helpers.R`. A nonmissing unsupported category,
unparseable numeric value, nonfinite value, impossible duration, or nonpositive
weight excludes the entire row. `diary_exclusions` lists the date, Excel row,
variable, and original value. Missing entries are allowed; model-specific
complete-case filtering happens later. Exclusion counts must accompany analysis
review because exclusions change the sample and its sleep-history coverage.

The undocumented `tukevaruoka = 1` is excluded rather than assigned an invented
meaning; defined values are 0 and 2. Positive fractional `aivotyo` values remain
valid legacy measurements: any positive value means evening brainwork.
Phone parking codes 1–3 mean parked before 20:00, 21:00, and 22:00; zero means
not parked. Child sleeping arrangement is not a measure of child illness.
`kipea = 2` identifies inconsistently recorded child illness.

`sleep_diary_all` retains valid rows without duration so recorded insomnia can
contribute to insomnia analyses. `sleep_diary` and `df_clean` retain their
duration-complete interface. Zero-hour nights remain.

## Coffee

Coffee was intentionally used after short sleep or when an alertness boost was
wanted. The primary exposure is any coffee versus none. Detailed categories mix
timing and amount, and some are sparse. Prior sleep adjustment addresses only
measured aspects of this behavioral selection.

The pooled comparison adds previous-night sleep separately from stress, health,
and exercise. History sensitivities add previous 3- or 5-night totals to the
previous-night term on a common complete-case sample. The previous-sleep
interaction compares coffee associations after <6 versus at least 6 hours,
with prior 5-night sleep controlled. The interaction coefficient, rather than
separate subgroup significance, tests the difference.

Medication adjustment, a no-medication subset, and a coffee-by-medication
interaction are supporting sensitivities. Medication can respond to anticipated
sleep problems and occur after coffee; adjustment is not automatically a causal
correction. Because codes 1 and 2 may have switched meaning historically, the
analysis uses any recorded sleep medication versus none. These checks do not
establish that medication counteracts caffeine. Bedtime remains a sensitivity
control because it can lie on a pathway from coffee to sleep.

## Magnesium

The default start is one day before the first raw `Mg = 1` date. Override it in
`magnesium.R` if the experiment log establishes another date. Pre-experiment
years are not controls. The codebook describes coin-toss decisions, but recorded
use does not reconstruct assignments, adherence, or blinding. The report compares
recorded use within the provisional period; its coefficient is not labeled a
verified randomized treatment effect.

Small-sample sensitivities add linear time, previous sleep, previous magnesium,
and then coffee and medication. Previous use addresses only one-day carryover.
Longer carryover and missing observations remain limitations.

## Uncertainty and reproducibility

Reporting regressions use seven-day Newey-West covariance estimates, with
uninterrupted daily sequences separated after complete-case filtering.
`fit_nw()` also rebuilds sequences if fixed effects remove observations, as can
happen with constant-outcome months in logistic models. Summaries report the
retained sample. Descriptive intervals in the coffee, bedtime, weekday,
temperature, and insomnia reports estimate group means together,
retaining intervening observed days in other groups.

Year-specific results and interactions are exploratory. Fixed effects do not
remove all time-varying confounding. AIC, residual plots, and descriptive
associations do not measure out-of-sample predictive performance.

`tests/test_helpers.R` checks calendar lags, validation, timezone boundaries,
and covariance after fixed-effect exclusions. `tests/test_reports.R` runs the
pipeline in a temporary project with synthetic inputs. Real-data results still
require the private files. Synthetic previews are not diary findings.
