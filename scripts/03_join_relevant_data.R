# =============================================================================
# 03_join_relevant_data.R
#
# Purpose: Join the sleep diary dataset with nightly environmental sensor data
#          and create analysis-ready datasets for:
#          1) all diary nights
#          2) nights with valid sensor data
#
# Requirements:
#  - Run `scripts/01_load_main_data.R` to create `df_clean`
#  - Run `scripts/02_load_co2_temp_data.R` to create `dat_mittari`
#
# Output:
#  - sleep_mittari: full diary dataset with sensor variables added when relevant
#  - sleep_mittari_sensor: subset for sensor-based analyses
# =============================================================================

library(tidyverse)
library(here)

source(here("scripts", "01_load_main_data.R"))
source(here("scripts", "02_load_co2_temp_data.R"))

if (!exists("df_clean")) {
  stop("df_clean not found. Run 01_load_main_data.R first.")
}

if (!exists("dat_mittari")) {
  stop("dat_mittari not found. Run 02_load_co2_temp_data.R first.")
}

sleep_mittari <- df_clean |>
  left_join(dat_mittari, by = c("date" = "yo_pvm")) |>
  mutate(
    sensor_expected = mittaripaalla == 1,
    sensor_available = !is.na(ka_co2) | !is.na(ka_temp) | !is.na(ka_humid),
    sensor_ready = sensor_expected & sensor_available,

    # Do not use sensor values on nights when the sensor was not supposed to be on
    across(
      c(ka_co2, ka_temp, ka_humid, n_obs, first_obs, last_obs),
      ~ if_else(sensor_expected, .x, NA)
    )
  )

sleep_mittari_sensor <- sleep_mittari |>
  filter(sensor_ready)

cat("\n=== JOIN SUMMARY ===\n")
cat("All diary nights:", nrow(sleep_mittari), "\n")
cat("Nights where sensor was expected:", sum(sleep_mittari$sensor_expected, na.rm = TRUE), "\n")
cat("Nights with sensor data available:", sum(sleep_mittari$sensor_available, na.rm = TRUE), "\n")
cat("Nights ready for sensor analysis:", nrow(sleep_mittari_sensor), "\n")
cat("Nights expected but missing sensor data:",
    sum(sleep_mittari$sensor_expected & !sleep_mittari$sensor_available, na.rm = TRUE), "\n")