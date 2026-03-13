# =============================================================================
# 03_join_relevant_data.R
#
# Purpose: Join the sleep diary dataset (loki) with environmental sensor (mittari)
#          data to create an analysis-ready dataset.
#
# Requirements:
#  - Run `scripts/01_load_main_data.R` to create `df_clean`.
#  - Run `scripts/02_load_co2_temp_data.R` to create `dat_mittari`.
#
# Output:
#  - `sleep_mittari`: full join (sleep diary + nightly sensor averages)
#  - `sleep_mittari_sensor`: subset of nights with sensor data available
# =============================================================================

# Load required libraries (if not already loaded)
library(tidyverse)
library(here)

# Load source datasets
source(here::here("scripts", "01_load_main_data.R"))
source(here::here("scripts", "02_load_co2_temp_data.R"))

# Sanity checks
if (!exists("df_clean")) stop("df_clean not found. Run 01_load_main_data.R first.")
if (!exists("dat_mittari")) stop("dat_mittari not found. Run 02_load_co2_temp_data.R first.")

# Join on the date (sleep diary date to sensor night date)
sleep_mittari <- df_clean |>
  left_join(dat_mittari, by = c("date" = "yo_pvm"))

# Optionally: focus only on nights where the sensor data exists
sleep_mittari_sensor <- sleep_mittari |>
  filter(!is.na(ka_co2) | !is.na(ka_temp) | !is.na(ka_humid))

cat("Joined sleep diary to sensor data:", nrow(sleep_mittari), "rows\n")
cat("Nights with sensor data:", nrow(sleep_mittari_sensor), "rows\n")
