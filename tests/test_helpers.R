library(tidyverse)
library(lubridate)
library(fixest)
source("scripts/helpers/analysis_helpers.R")

dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-04"))
stopifnot(identical(lag_by_calendar_days(c(6, 7, 8), dates), c(NA_real_, 6, NA_real_)))
dates <- as.Date("2024-01-01") + 0:6
stopifnot(identical(previous_sleep_sum(1:7, dates, 3), c(NA_real_, NA_real_, NA_real_, 6, 9, 12, 15)))
stopifnot(is.na(previous_sleep_sum((1:7)[-3], dates[-3], 3)[4]))

timestamps <- ymd_hms(c("2026-07-02 01:00:00", "2026-01-02 01:00:00",
                        "2026-03-29 04:30:00", "2026-10-25 04:30:00",
                        "2026-07-02 21:00:00", "2026-07-02 08:00:00"),
                      tz = "Europe/Helsinki")
stopifnot(identical(sensor_night_date(timestamps),
                   as.Date(c("2026-07-01", "2026-01-01", "2026-03-28",
                             "2026-10-24", "2026-07-02", "2026-07-02"))))

raw <- tibble(unituntia = c(0, NA, 7, 7, 7, 7, 7, 7),
              kahvi = c(0, 1, 9, 1, 1, 1, 1, 1),
              Mg = c(0, 1, 0, 51, 0, 0, 0, 0),
              paino = c(NA, NA, NA, NA, 0, NA, NA, NA),
              tukevaruoka = c(0, 0, 0, 0, 0, 1, 0, 0),
              aivotyo = c(0.3, 2, 0, 0, 0, 0, 0, 0),
              PC = c("1,5", "0", "0", "0", "0", "0", "bad", "0"))
checked <- validate_diary_rows(raw)
stopifnot(identical(checked$rejected_rows, 3:7), nrow(checked$data) == 3,
          checked$data$unituntia[1] == 0, is.na(checked$data$unituntia[2]))

set.seed(314)
dat <- tibble(date = as.Date("2025-01-01") + 0:179,
              x = rnorm(180), y = rbinom(180, 1, 0.4),
              month = format(date, "%Y-%m")) |>
  mutate(y = replace(y, month == "2025-03", 0))
model <- fit_nw(y ~ x | month, dat, family = binomial())
retained <- dat |> filter(month != "2025-03") |> prepare_nw_data()
reference <- feglm(y ~ x | month, retained, family = binomial(),
                   vcov = NW(7) ~ series_id + date)
stopifnot(nobs(model) == nrow(retained),
          isTRUE(all.equal(unname(vcov(model)), unname(vcov(reference)), tolerance = 1e-9)))
cat("Calendar, validation, and estimation-sample tests passed.\n")

group_data <- dat |> mutate(group = factor(rep(c("a", "b"), length.out = n())))
intervals <- grouped_mean_ci(group_data, "group", "x")
observed_means <- group_data |> group_by(group) |> summarise(estimate = mean(x))
stopifnot(isTRUE(all.equal(intervals$estimate, observed_means$estimate, tolerance = 1e-9)),
          all(intervals$ci_low <= intervals$estimate),
          all(intervals$ci_high >= intervals$estimate))
