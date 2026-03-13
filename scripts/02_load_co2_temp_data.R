# =============================================================================
# 02_load_co2_temp_data.R
#
# Purpose:
#   Load and preprocess environmental sensor data (CO2, temperature, humidity)
#   and aggregate it to nightly averages that can be joined to sleep diary data.
#
# Input:
#   data/raw/mittari_kaikki.xlsx
#
# Output:
#   - dat_mittari: nightly sensor summary
#   - sensor_nights: alias for readability
# =============================================================================

library(readxl)
library(tidyverse)
library(lubridate)
library(hms)
library(here)

mittari_path <- here("data", "raw", "mittari_kaikki.xlsx")

if (!file.exists(mittari_path)) {
  stop("Could not find sensor data at: ", mittari_path)
}

required_cols <- c("aika", "co2", "temp", "humid")

clean_numeric <- function(x) {
  x |>
    as.character() |>
    stringr::str_replace_all(",", ".") |>
    as.numeric()
}

mean_or_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    mean(x, na.rm = TRUE)
  }
}

mittari_raw <- read_excel(mittari_path, col_names = TRUE, na = c("", "NA"))

missing_cols <- setdiff(required_cols, names(mittari_raw))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

mittari_clean <- mittari_raw |>
  transmute(
    datetime = if (is.POSIXct(aika)) {
  force_tz(aika, tzone = "Europe/Helsinki")
} else {
  parse_date_time(aika, orders = c("mdy HMS", "m/d/y H:M:S", "mdy HM", "m/d/y H:M"),
                  tz = "Europe/Helsinki")
}, 
    co2 = clean_numeric(co2),
    temp = clean_numeric(temp),
    humid = clean_numeric(humid)
  ) |>
  arrange(datetime)

if (all(is.na(mittari_clean$datetime))) {
  stop("All timestamps failed to parse in `aika`.")
}

n_bad_datetime <- sum(is.na(mittari_clean$datetime))

if (n_bad_datetime > 0) {
  warning(n_bad_datetime, " rows had unparsed timestamps and will be excluded.")
}

# Night window:
# 22:00-08:00, where observations between 00:00 and 07:59 are assigned
# to the previous calendar day.
yo_start <- as_hms("22:00:00")
yo_end   <- as_hms("08:00:00")

dat_mittari <- mittari_clean |>
  filter(!is.na(datetime)) |>
  mutate(
    clock_time = as_hms(datetime),
    in_night_window = (clock_time >= yo_start) | (clock_time < yo_end),
    yo_pvm = as.Date(if_else(clock_time < yo_end, datetime - days(1), datetime))
  ) |>
  filter(in_night_window) |>
  group_by(yo_pvm) |>
  summarise(
    n_obs = dplyr::n(),
    first_obs = min(datetime, na.rm = TRUE),
    last_obs = max(datetime, na.rm = TRUE),
    ka_co2 = mean_or_na(co2),
    ka_temp = mean_or_na(temp),
    ka_humid = mean_or_na(humid),
    .groups = "drop"
  ) |>
  arrange(yo_pvm)

sensor_nights <- dat_mittari

cat("\n=== SENSOR DATA SUMMARY ===\n")
cat("Total raw rows:", nrow(mittari_raw), "\n")
cat("Rows with parsed datetime:", sum(!is.na(mittari_clean$datetime)), "\n")
cat("Nightly observations:", nrow(dat_mittari), "\n")

if (nrow(dat_mittari) > 0) {
  cat(
    "Night date range:",
    format(min(dat_mittari$yo_pvm), "%Y-%m-%d"), "to",
    format(max(dat_mittari$yo_pvm), "%Y-%m-%d"), "\n"
  )

  cat("\nNightly observation count summary:\n")
  print(summary(dat_mittari$n_obs))

  cat("\nMissing values by nightly variable:\n")
  print(colSums(is.na(dat_mittari)))
}

cat("\n✓ Sensor data cleaning complete.\n")