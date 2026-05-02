# =============================================================================
# 07_weekday.R
#
# Purpose: Analyze the association between day of week and sleep outcomes
#
# Research question:
#   Does sleep duration vary systematically by day of week?
#
# Input:
#   df_clean from scripts/01_load_main_data.R
#
# Outputs:
#   - weekday-specific descriptive summaries
#   - variable-specific figures saved to figures/variable_specific/
#   - regression models for reporting
# =============================================================================

library(tidyverse)
library(broom)
library(fixest)
library(here)
library(patchwork)

source(here("scripts", "01_load_main_data.R"))

if (!exists("df_clean")) {
  stop("df_clean not found. Run 01_load_main_data.R first.")
}

dir.create(here("figures"), showWarnings = FALSE)
dir.create(here("figures", "variable_specific"), showWarnings = FALSE)

# =============================================================================
# COLOR SYSTEM
# =============================================================================

col_navy       <- "#002d5a"
col_dark_blue  <- "#2f4a73"
col_steel      <- "#4a7ba7"
col_mid_blue   <- "#6c8eb5"
col_light_blue <- "#a3c1d9"
col_pale_blue  <- "#d0e1ef"
col_orange     <- "#CC5500"
col_dark_text  <- "#2a2a2a"
col_grey       <- "grey40"

make_palette <- function(n) {
  blues <- c(col_navy, col_dark_blue, col_steel, col_mid_blue, col_light_blue, col_pale_blue)

  if (n <= length(blues)) {
    blues[1:n]
  } else {
    colorRampPalette(c(col_navy, col_pale_blue))(n)
  }
}

theme_sleep <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(size = 15, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 11, color = col_grey, hjust = 0),
      plot.caption  = element_text(size = 9, color = "grey50"),
      plot.margin   = margin(15, 15, 15, 15),
      axis.title    = element_text(size = 12),
      axis.text     = element_text(size = 10),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

get_mode <- function(x) {
  x_no_na <- x[!is.na(x)]
  x_levels <- unique(x_no_na)
  x_levels[which.max(tabulate(match(x_no_na, x_levels)))]
}

# =============================================================================
# ANALYSIS DATA
# =============================================================================

dat_weekday <- df_clean |>
  select(
    date,
    year,
    month,
    month_num,
    day_of_week,
    duration,
    insomnia_num,
    bedtime,
    coffee,
    stress,
    health,
    exercise
  ) |>
  drop_na(day_of_week, duration)

cat("\n========== WEEKDAY ANALYSIS SAMPLE ==========\n")
cat("Observations:", nrow(dat_weekday), "\n")
cat(
  "Date range:", format(min(dat_weekday$date), "%Y-%m-%d"), "to",
  format(max(dat_weekday$date), "%Y-%m-%d"), "\n"
)

# =============================================================================
# DESCRIPTIVE SUMMARIES
# =============================================================================

weekday_summary <- dat_weekday |>
  group_by(day_of_week) |>
  summarise(
    n = n(),
    mean_sleep = mean(duration, na.rm = TRUE),
    median_sleep = median(duration, na.rm = TRUE),
    sd_sleep = sd(duration, na.rm = TRUE),
    insomnia_rate = mean(insomnia_num > 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(mean_sleep, median_sleep, sd_sleep, insomnia_rate),
      \(x) round(x, 3)
    )
  )

cat("\n========== SLEEP DURATION BY WEEKDAY ==========\n")
print(weekday_summary, n = Inf)

# =============================================================================
# VISUALIZATIONS
# =============================================================================

p_weekday_box <- dat_weekday |>
  ggplot(aes(x = day_of_week, y = duration, fill = day_of_week)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.12, size = 1.5, color = col_dark_text) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = col_orange) +
  scale_fill_manual(
    values = make_palette(n_distinct(dat_weekday$day_of_week)),
    guide = "none"
  ) +
  labs(
    title = "Sleep duration by day of week",
    subtitle = "Dots show individual nights; diamonds show weekday means",
    x = NULL,
    y = "Sleep duration (hours)"
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_weekday_mean <- weekday_summary |>
  ggplot(aes(x = day_of_week, y = mean_sleep, group = 1)) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 3, color = col_orange) +
  labs(
    title = "Mean sleep duration by day of week",
    subtitle = "Average sleep duration across observed nights",
    x = NULL,
    y = "Mean sleep duration (hours)"
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_weekday_insomnia <- weekday_summary |>
  ggplot(aes(x = day_of_week, y = insomnia_rate, group = 1)) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 3, color = col_orange) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Insomnia rate by day of week",
    subtitle = "Share of nights with any recorded insomnia",
    x = NULL,
    y = "Insomnia rate"
  ) +
  theme_sleep()

p_weekday_combined <- (p_weekday_box / (p_weekday_mean + p_weekday_insomnia)) +
  plot_annotation(
    title = "Weekday patterns in sleep outcomes",
    subtitle = "Sleep duration and insomnia by day of week"
  )

print(p_weekday_combined)

ggsave(
  here("figures", "variable_specific", "weekday_combined.png"),
  p_weekday_combined,
  width = 14,
  height = 10,
  dpi = 300
)

# =============================================================================
# MODELS
# =============================================================================

m_raw <- lm(
  duration ~ day_of_week,
  data = dat_weekday
)

dat_adjusted <- dat_weekday |>
  drop_na(bedtime, coffee, stress, health, exercise)

m_adjusted <- lm(
  duration ~ day_of_week + bedtime + coffee + stress + health + exercise,
  data = dat_adjusted
)

m_time_adjusted <- feols(
  duration ~ day_of_week + bedtime + coffee + stress + health + exercise |
    year + month,
  data = dat_adjusted,
  vcov = "hetero"
)

cat("\n========== MODEL 1: RAW WEEKDAY DIFFERENCES ==========\n")
print(summary(m_raw))

cat("\n========== MODEL 2: ADJUSTED WEEKDAY DIFFERENCES ==========\n")
print(summary(m_adjusted))

cat("\n========== MODEL 3: TIME-ADJUSTED WEEKDAY DIFFERENCES ==========\n")
print(summary(m_time_adjusted))

model_comparison <- tibble(
  model = c("Raw", "Adjusted", "Time-adjusted"),
  n = c(nobs(m_raw), nobs(m_adjusted), nobs(m_time_adjusted)),
  rmse = c(
    sigma(m_raw),
    sigma(m_adjusted),
    fitstat(m_time_adjusted, "rmse") |> as.numeric()
  )
)

cat("\n========== MODEL COMPARISON ==========\n")
print(model_comparison)

# =============================================================================
# ADJUSTED PREDICTIONS
# =============================================================================

reference_values <- dat_adjusted |>
  summarise(
    bedtime = get_mode(bedtime),
    coffee = get_mode(coffee),
    stress = get_mode(stress),
    health = get_mode(health),
    exercise = get_mode(exercise)
  )

pred_weekday <- tibble(day_of_week = levels(dat_adjusted$day_of_week)) |>
  mutate(
    day_of_week = factor(day_of_week, levels = levels(dat_adjusted$day_of_week)),
    bedtime = factor(
      reference_values$bedtime,
      levels = levels(dat_adjusted$bedtime),
      ordered = is.ordered(dat_adjusted$bedtime)
    ),
    coffee = factor(
      reference_values$coffee,
      levels = levels(dat_adjusted$coffee),
      ordered = is.ordered(dat_adjusted$coffee)
    ),
    stress = factor(
      reference_values$stress,
      levels = levels(dat_adjusted$stress)
    ),
    health = factor(
      reference_values$health,
      levels = levels(dat_adjusted$health),
      ordered = is.ordered(dat_adjusted$health)
    ),
    exercise = factor(
      reference_values$exercise,
      levels = levels(dat_adjusted$exercise),
      ordered = is.ordered(dat_adjusted$exercise)
    ),
    pred_duration = predict(m_adjusted, newdata = .)
  )

p_weekday_adjusted <- pred_weekday |>
  ggplot(aes(x = day_of_week, y = pred_duration, group = 1)) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 3, color = col_orange) +
  labs(
    title = "Adjusted sleep duration by day of week",
    subtitle = "Predicted sleep duration with behavioral factors held at their modal values",
    x = NULL,
    y = "Predicted sleep duration (hours)"
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

print(p_weekday_adjusted)

ggsave(
  here("figures", "variable_specific", "weekday_adjusted_prediction.png"),
  p_weekday_adjusted,
  width = 10,
  height = 6,
  dpi = 300
)

cat("\n========== REPORTING SUMMARY ==========\n")
cat("The script estimates raw, adjusted, and time-adjusted weekday differences in sleep duration.\n")
cat("Figures saved to figures/variable_specific/.\n")
