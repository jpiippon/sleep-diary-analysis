# =============================================================================
# 01_load_main_data.R
#
# Purpose: Load, clean, and prepare sleep diary data for analysis.
#
# Data source: data/raw/loki.xlsx
#
# Output:
#   - sleep_diary: analysis-ready sleep diary data
#   - df_clean: alias kept for downstream script compatibility
# =============================================================================

library(readxl)
library(tidyverse)
library(lubridate)
library(here)

raw_path <- here("data", "raw", "loki.xlsx")

if (!file.exists(raw_path)) {
  stop("Could not find sleep diary data at: ", raw_path)
}

required_cols <- c(
  "aika", "vknpv", "unituntia", "unettomuus", "myohaan",
  "urheilu", "kahvi", "ressi", "kipea", "mittaripaalla"
)

weekday_levels_fi <- c("ma", "ti", "ke", "to", "pe", "la", "su")
weekday_labels_en <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

clean_numeric <- function(x) {
  x |>
    as.character() |>
    stringr::str_replace_all(",", ".") |>
    as.numeric()
}

df_raw <- read_excel(raw_path, col_names = TRUE, na = c("", "NA"))

missing_cols <- setdiff(required_cols, names(df_raw))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

sleep_diary <- df_raw |>
  mutate(
    date = as.Date(aika),
    reported_day_of_week = vknpv |>
      as.character() |>
      stringr::str_squish() |>
      stringr::str_to_lower()
  ) |>
  mutate(
    across(
      -c(aika, vknpv, date, reported_day_of_week),
      ~ suppressWarnings(clean_numeric(.x))
    )
  ) |>
  rename(
    duration = unituntia,
    bedtime_code = myohaan,
    coffee_code = kahvi,
    stress_num = ressi,
    exercise_code = urheilu,
    health_num = kipea,
    insomnia_num = unettomuus
  ) |>
  filter(
    !is.na(date),
    date <= Sys.Date(),
    !is.na(duration),
    duration > 0
  ) |>
  mutate(
    year = year(date),
    month_num = month(date),
    month = factor(month_num, levels = 1:12, labels = month.abb),

    # Derive weekday from date instead of trusting manual entry.
    day_of_week = factor(
      wday(date, week_start = 1),
      levels = 1:7,
      labels = weekday_labels_en
    ),

    # Keep the manually entered weekday only as a quality-check field.
    reported_day_of_week = factor(
      reported_day_of_week,
      levels = weekday_levels_fi,
      labels = weekday_labels_en
    ),

    weekday_match = case_when(
      is.na(reported_day_of_week) ~ NA,
      TRUE ~ as.character(reported_day_of_week) == as.character(day_of_week)
    ),

    bedtime = factor(
      bedtime_code,
      levels = 0:2,
      labels = c("Before 23:00", "23:00-00:00", "After 00:00"),
      ordered = TRUE
    ),

    stress = factor(
      stress_num,
      levels = 0:1,
      labels = c("No", "Yes")
    ),

    exercise = factor(
      exercise_code,
      levels = 0:2,
      labels = c("None", "Earlier in the day", "Heavy after 18:00"),
      ordered = TRUE
    ),

    # Keep generic labels where coding is not yet fully documented.
    coffee = factor(
      coffee_code,
      levels = 0:3,
      labels = c("None", "Half a cup in the morning", "A cup or two before noon", "Coffee after noon"),
      ordered = TRUE
    ),

    health = factor(
      health_num,
      levels = 0:2,
      labels = c("Healthy", "Sick", "Kid is sick"),
      ordered = TRUE
    ),

    insomnia = factor(
      insomnia_num,
      levels = 0:2,
      labels = c("None", "Code 1", "Code 2"),
      ordered = TRUE
    )
  ) |>
  select(-aika, -vknpv) |>
  relocate(
    date, year, month_num, month,
    day_of_week, reported_day_of_week, weekday_match,
    duration,
    bedtime_code, bedtime,
    coffee_code, coffee,
    stress_num, stress,
    exercise_code, exercise,
    health_num, health,
    insomnia_num, insomnia,
    mittaripaalla
  ) |>
  arrange(date)

# Keep the legacy object name for downstream scripts.
df_clean <- sleep_diary

cat("\n=== CLEAN DATASET SUMMARY ===\n")
cat("Total observations:", nrow(df_clean), "\n")
cat(
  "Date range:", format(min(df_clean$date), "%Y-%m-%d"), "to",
  format(max(df_clean$date), "%Y-%m-%d"), "\n"
)
cat("Duration (days):", as.numeric(max(df_clean$date) - min(df_clean$date)), "\n")
cat(
  "Weekday mismatches:",
  sum(df_clean$weekday_match == FALSE, na.rm = TRUE),
  "\n"
)

cat("\nSleep duration summary:\n")
print(summary(df_clean$duration))

cat("\nMissing values by variable:\n")
print(colSums(is.na(df_clean)))

cat("\nData types:\n")
print(sapply(df_clean, class))

cat("\n✓ Sleep diary data cleaning complete.\n")