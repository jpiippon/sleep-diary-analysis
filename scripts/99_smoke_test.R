# =============================================================================
# 99_smoke_test.R
#
# Purpose: Run a lightweight repository smoke test for the sleep diary project.
#
# What it checks:
#   1. Core scripts parse without syntax errors
#   2. Core data-loading scripts run
#   3. Expected objects and columns are created
#   4. The main join behaves as expected
#   5. A minimal OLS model runs
#   6. A minimal fixed-effects model runs
#   7. A minimal sensor model runs when sensor data are sufficient
# =============================================================================

library(here)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

assert_has_cols <- function(data, cols, object_name) {
  missing_cols <- setdiff(cols, names(data))
  
  assert_true(
    length(missing_cols) == 0,
    paste0(
      object_name,
      " is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  )
}

n_levels <- function(x) {
  length(unique(stats::na.omit(x)))
}

cat("\n========== SMOKE TEST ==========\n")

# -----------------------------------------------------------------------------
# 1. Parse scripts to catch syntax errors early
# -----------------------------------------------------------------------------

script_paths <- c(
  here("scripts", "01_load_main_data.R"),
  here("scripts", "02_load_co2_temp_data.R"),
  here("scripts", "03_join_relevant_data.R"),
  here("scripts", "04_descriptives_and_plots.R"),
  here("scripts", "05_models.R"),
  here("scripts", "06_fixed_effects_models.R")
)

script_paths <- script_paths[file.exists(script_paths)]

invisible(lapply(script_paths, parse))

cat("✓ Scripts parsed successfully\n")

# -----------------------------------------------------------------------------
# 2. Run core pipeline scripts
# -----------------------------------------------------------------------------

source(here("scripts", "01_load_main_data.R"))
source(here("scripts", "02_load_co2_temp_data.R"))
source(here("scripts", "03_join_relevant_data.R"))

cat("✓ Core scripts sourced successfully\n")

# -----------------------------------------------------------------------------
# 3. Check expected objects and columns
# -----------------------------------------------------------------------------

assert_true(exists("df_clean"), "Object `df_clean` was not created.")
assert_true(is.data.frame(df_clean), "`df_clean` is not a data.frame.")
assert_true(nrow(df_clean) > 0, "`df_clean` has zero rows.")

assert_has_cols(
  df_clean,
  c(
    "date", "duration", "day_of_week", "bedtime",
    "coffee", "stress", "exercise", "health", "insomnia_num"
  ),
  "df_clean"
)

assert_true(exists("dat_mittari"), "Object `dat_mittari` was not created.")
assert_true(is.data.frame(dat_mittari), "`dat_mittari` is not a data.frame.")

assert_has_cols(
  dat_mittari,
  c("yo_pvm", "n_obs", "ka_co2", "ka_temp", "ka_humid"),
  "dat_mittari"
)

assert_true(exists("sleep_mittari"), "Object `sleep_mittari` was not created.")
assert_true(is.data.frame(sleep_mittari), "`sleep_mittari` is not a data.frame.")
assert_true(nrow(sleep_mittari) == nrow(df_clean), "`sleep_mittari` should preserve all diary rows after left join.")

assert_has_cols(
  sleep_mittari,
  c(
    "date", "duration", "day_of_week", "bedtime", "coffee",
    "stress", "exercise", "health", "n_obs", "ka_co2", "ka_temp", "ka_humid"
  ),
  "sleep_mittari"
)

assert_true(exists("sleep_mittari_sensor"), "Object `sleep_mittari_sensor` was not created.")
assert_true(is.data.frame(sleep_mittari_sensor), "`sleep_mittari_sensor` is not a data.frame.")

sensor_flag <- !is.na(sleep_mittari$ka_co2) | !is.na(sleep_mittari$ka_temp) | !is.na(sleep_mittari$ka_humid)

assert_true(
  nrow(sleep_mittari_sensor) == sum(sensor_flag),
  "`sleep_mittari_sensor` does not match the expected sensor-available subset."
)

cat("✓ Expected objects and columns found\n")

# -----------------------------------------------------------------------------
# 4. Minimal OLS smoke test
# -----------------------------------------------------------------------------

ols_vars <- c("duration", "bedtime", "stress", "health")
ols_dat <- df_clean[stats::complete.cases(df_clean[, ols_vars]), ols_vars, drop = FALSE]

assert_true(nrow(ols_dat) >= 20, "Too few complete observations for OLS smoke test.")

m_ols <- stats::lm(duration ~ bedtime + stress + health, data = ols_dat)

assert_true(length(stats::coef(m_ols)) > 1, "OLS smoke test failed to produce coefficients.")

cat("✓ Minimal OLS model ran successfully\n")

# -----------------------------------------------------------------------------
# 5. Minimal fixed-effects smoke test
# -----------------------------------------------------------------------------

assert_true(
  requireNamespace("fixest", quietly = TRUE),
  "Package `fixest` is required for the fixed-effects smoke test."
)

fe_dat <- sleep_mittari
fe_dat$year_month <- factor(format(fe_dat$date, "%Y-%m"))

fe_vars <- c(
  "duration", "bedtime", "coffee", "stress", "health",
  "exercise", "day_of_week", "year_month"
)

fe_dat <- fe_dat[stats::complete.cases(fe_dat[, fe_vars]), fe_vars, drop = FALSE]

assert_true(nrow(fe_dat) >= 30, "Too few complete observations for fixed-effects smoke test.")
assert_true(n_levels(fe_dat$day_of_week) >= 2, "Fixed-effects smoke test needs at least 2 weekday levels.")
assert_true(n_levels(fe_dat$year_month) >= 2, "Fixed-effects smoke test needs at least 2 year-month levels.")

m_fe <- fixest::feols(
  duration ~ bedtime + coffee + stress + health + exercise |
    day_of_week + year_month,
  data = fe_dat,
  vcov = "hetero"
)

assert_true(length(coef(m_fe)) > 0, "Fixed-effects smoke test failed to produce coefficients.")

cat("✓ Minimal fixed-effects model ran successfully\n")

# -----------------------------------------------------------------------------
# 6. Minimal sensor model smoke test
# -----------------------------------------------------------------------------

sensor_dat <- sleep_mittari_sensor
sensor_dat$year_month <- factor(format(sensor_dat$date, "%Y-%m"))

sensor_vars <- c(
  "duration", "bedtime", "coffee", "stress", "health", "exercise",
  "day_of_week", "year_month", "ka_co2", "ka_temp", "ka_humid"
)

sensor_dat <- sensor_dat[stats::complete.cases(sensor_dat[, sensor_vars]), sensor_vars, drop = FALSE]

sensor_ready <- nrow(sensor_dat) >= 30 &&
  n_levels(sensor_dat$day_of_week) >= 2 &&
  n_levels(sensor_dat$year_month) >= 2 &&
  n_levels(sensor_dat$ka_co2) >= 4 &&
  n_levels(sensor_dat$ka_temp) >= 4 &&
  n_levels(sensor_dat$ka_humid) >= 4

if (sensor_ready) {
  m_sensor <- fixest::feols(
    duration ~ bedtime + coffee + stress + health + exercise +
      splines::ns(ka_co2, df = 3) +
      splines::ns(ka_temp, df = 3) +
      scale(ka_humid) |
      day_of_week + year_month,
    data = sensor_dat,
    vcov = "hetero"
  )
  
  assert_true(length(coef(m_sensor)) > 0, "Sensor smoke test failed to produce coefficients.")
  
  cat("✓ Minimal sensor model ran successfully\n")
} else {
  cat("• Sensor model smoke test skipped: insufficient complete sensor data\n")
}

cat("\n✓ Smoke test completed successfully\n")

# -----------------------------------------------------------------------------
# 7. Full script 06 integration test
# -----------------------------------------------------------------------------

source(here("scripts", "06_fixed_effects_models.R"))

cat("✓ scripts/06_fixed_effects_models.R ran successfully\n")