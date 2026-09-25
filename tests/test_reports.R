# Integration checks use synthetic data in a disposable project. Private input
# files and existing output figures in the working repository are never changed.
library(tidyverse)
library(lubridate)

run_report_checks <- function() {
  project <- normalizePath(getwd())
  fixture <- tempfile("sleep-diary-check-")
  dir.create(fixture)
  on.exit(setwd(project), add = TRUE)
  file.copy(file.path(project, "scripts"), fixture, recursive = TRUE)
  file.copy(file.path(project, "Uni.Rproj"), fixture)
  dir.create(file.path(fixture, "data", "raw"), recursive = TRUE)

  set.seed(20260925)
  dates <- seq(as.Date("2023-01-01"), as.Date("2025-12-31"), by = "day")
  n <- length(dates)
  diary <- tibble(
    aika = dates, vknpv = rep(c("su", "ma", "ti", "ke", "to", "pe", "la"), length.out = n),
    unituntia = pmax(0, 6.8 + rnorm(n, sd = 1.2)),
    unettomuus = sample(0:2, n, replace = TRUE, prob = c(.72, .15, .13)),
    myohaan = sample(0:2, n, replace = TRUE, prob = c(.45, .4, .15)),
    urheilu = sample(0:2, n, replace = TRUE),
    kahvi = sample(0:3, n, replace = TRUE, prob = c(.6, .2, .15, .05)),
    ressi = rbinom(n, 1, .12), kipea = sample(0:2, n, replace = TRUE, prob = c(.85, .1, .05)),
    mittaripaalla = 1, puhelinparkki = sample(0:3, n, replace = TRUE),
    vauvahuoneessa = sample(0:6, n, replace = TRUE), aivotyo = rbinom(n, 1, .1),
    unilaake = sample(0:2, n, replace = TRUE),
    Mg = as.integer(dates >= as.Date("2025-10-15")) * rbinom(n, 1, .5),
    sauna = sample(0:2, n, replace = TRUE), tukevaruoka = 0,
    paino = NA_real_
  ) |>
    mutate(unituntia = replace(unituntia, 1, 0),
           unituntia = replace(unituntia, 20, NA_real_),
           kahvi = replace(kahvi, 30, 9),
           Mg = replace(Mg, 40, 51),
           paino = replace(paino, 50, 0)) |>
    slice(-60)
  writexl::write_xlsx(diary, file.path(fixture, "data", "raw", "loki.xlsx"))

  sensor <- tidyr::crossing(date = dates, hour = c(21:23, 0:7)) |>
    mutate(aika = paste(date + as.integer(hour < 8), sprintf("%02d:00:00", hour)),
           co2 = 800 + 150 * sin(as.numeric(date) / 60) + rnorm(n(), sd = 70),
           temp = 23 + 4 * sin(as.numeric(date) / 60) + rnorm(n(), sd = .3),
           humid = 40 + rnorm(n(), sd = 5)) |>
    select(aika, co2, temp, humid)
  writexl::write_xlsx(sensor, file.path(fixture, "data", "raw", "mittari_kaikki.xlsx"))

  setwd(fixture)
  source("scripts/99_smoke_test.R", local = .GlobalEnv)
  stopifnot(nrow(sleep_diary_all) == nrow(df_clean) + 1L,
            length(diary_validation$rejected_rows) == 3L,
            all(dat_mittari$n_obs == 11L),
            all(dat_mittari$yo_pvm == dates))
  source("scripts/05_models.R", local = .GlobalEnv)
  source("scripts/08_lag_effect.R", local = .GlobalEnv)

  # These reports cover the shared loader, affected logit wrappers, the unchanged
  # main figure layout, and the newly added history and experiment analyses.
  reports <- c("coffee", "coffee_relationships", "magnesium", "insomnia",
               "stress", "brainwork", "bedtime", "weekday", "temperature", "exercise", "health")
  purrr::walk(reports, function(report) {
    cat("\nINTEGRATION REPORT:", report, "\n")
    source(file.path("scripts", "variable_specific", paste0(report, ".R")), local = .GlobalEnv)
  })
  previews <- file.path(project, "outputs", "synthetic_test_previews")
  dir.create(previews, recursive = TRUE, showWarnings = FALSE)
  file.copy("outputs/figures/variable_specific", previews, recursive = TRUE)
  cat("\nAll integration reports completed with synthetic data.\n")
}

run_report_checks()
