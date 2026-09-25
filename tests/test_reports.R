# Integration checks use synthetic data in a disposable project. Private input
# files and existing output figures in the working repository are never changed.
library(tidyverse)
library(lubridate)

run_report_checks <- function() {
  old_options <- options(warn = 1)
  on.exit(options(old_options), add = TRUE)
  project <- normalizePath(getwd())
  fixture <- tempfile("sleep-diary-check-")
  dir.create(fixture)
  on.exit(setwd(project), add = TRUE)
  on.exit({
    previews <- file.path(project, "outputs", "synthetic_test_previews")
    dir.create(previews, recursive = TRUE, showWarnings = FALSE)
    figures <- list.files(file.path(fixture, "outputs", "figures", "variable_specific"),
                          pattern = "(coffee|magnesium)_figure(1_main|S[145]_).*png$",
                          recursive = TRUE, full.names = TRUE)
    file.copy(figures, previews, overwrite = TRUE)
  }, add = TRUE)
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
    # The local 03:00 hour does not exist on these spring-transition mornings.
    filter(!(hour == 3 & date %in% as.Date(c("2023-03-25", "2024-03-30", "2025-03-29")))) |>
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
            all(dat_mittari$n_obs == if_else(
              dat_mittari$yo_pvm %in% as.Date(c("2023-03-25", "2024-03-30", "2025-03-29")),
              10L, 11L)),
            all(dat_mittari$yo_pvm == dates))
  source("scripts/05_models.R", local = .GlobalEnv)
  source("scripts/08_lag_effect.R", local = .GlobalEnv)

  # These reports cover the shared loader, affected logit wrappers, the unchanged
  # main figure layout, and the newly added history and experiment analyses.
  reports <- c("coffee", "coffee_relationships", "magnesium", "insomnia",
               "stress", "brainwork", "bedtime", "weekday", "temperature", "exercise", "health")
  failures <- purrr::map_chr(reports, function(report) {
    cat("\nINTEGRATION REPORT:", report, "\n")
    tryCatch({
      source(file.path("scripts", "variable_specific", paste0(report, ".R")), local = .GlobalEnv)
      if (report == "coffee") {
        stopifnot(length(models_pooled) == 5L, length(history_models) == 5L,
                  nrow(previous_sleep_contrasts) == 2L, nrow(medication_contrasts) == 2L)
      }
      if (report == "magnesium") stopifnot(length(models) == 4L)
      if (report == "health") {
        stopifnot(all(duration_results$ci_low <= duration_results$estimate),
                  all(duration_results$ci_high >= duration_results$estimate))
      }
      ""
    }, error = function(e) {
      failure <- paste(report, conditionMessage(e), sep = ": ")
      message("REPORT FAILURE: ", failure)
      failure
    })
  })
  failures <- failures[nzchar(failures)]
  if (length(failures) > 0) stop(paste(failures, collapse = "\n"), call. = FALSE)
  cat("\nAll integration reports completed with synthetic data.\n")
}

run_report_checks()
