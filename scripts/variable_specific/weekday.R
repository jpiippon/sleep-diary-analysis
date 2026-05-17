# =============================================================================
# weekday.R
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
#   - descriptive summaries printed to console
#   - variable-specific figures saved to figures/variable_specific/weekday/
#   - raw, adjusted, and month fixed-effect models for reporting
#
# Notes for adapting this script:
#   - For categorical or ordered variables, use boxplots, category summaries,
#     and factor-based regression terms.
#   - For numeric variables, use scatterplots, binned summaries, and either
#     linear, polynomial, or spline terms depending on the expected association.
#   - Keep the general structure stable across variable-specific scripts.
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

figure_dir <- here("figures", "variable_specific", "weekday")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SETTINGS
# =============================================================================

variable_name <- "day_of_week"
variable_label <- "Day of week"
outcome_name <- "duration"
outcome_label <- "Sleep duration (hours)"

# Variable type guidance for future scripts:
#   categorical: weekday, bedtime, coffee, stress, health, exercise, phone parking
#   numeric: CO2, temperature, humidity, screen time, continuous sleep measures
variable_type <- "categorical"

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
  mutate(year_month = factor(format(date, "%Y-%m"))) |>
  select(
    date,
    year_month,
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
    se_sleep = sd_sleep / sqrt(n),
    ci_low = mean_sleep - 1.96 * se_sleep,
    ci_high = mean_sleep + 1.96 * se_sleep,
    insomnia_rate = mean(insomnia_num > 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(mean_sleep, median_sleep, sd_sleep, se_sleep, ci_low, ci_high, insomnia_rate),
      \(x) round(x, 3)
    )
  )

cat("\n========== SLEEP DURATION BY WEEKDAY ==========\n")
print(weekday_summary, n = Inf)

# =============================================================================
# VISUALIZATIONS
# =============================================================================

p_distribution <- dat_weekday |>
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
    y = outcome_label
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_mean_ci <- weekday_summary |>
  ggplot(aes(x = day_of_week, y = mean_sleep, group = 1)) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.12,
    color = col_dark_blue,
    alpha = 0.8
  ) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 3, color = col_orange) +
  labs(
    title = "Mean sleep duration by day of week",
    subtitle = "Means with approximate 95% confidence intervals",
    x = NULL,
    y = "Mean sleep duration (hours)"
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_insomnia <- weekday_summary |>
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

p_n <- weekday_summary |>
  ggplot(aes(x = day_of_week, y = n, fill = day_of_week)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(
    values = make_palette(n_distinct(dat_weekday$day_of_week)),
    guide = "none"
  ) +
  labs(
    title = "Number of observations by day of week",
    subtitle = "Observed nights in the analysis sample",
    x = NULL,
    y = "Nights"
  ) +
  theme_sleep()

p_combined <- (p_distribution / (p_mean_ci + p_insomnia) / p_n) +
  plot_layout(heights = c(1.3, 1, 0.8)) +
  plot_annotation(
    title = "Weekday patterns in sleep outcomes",
    subtitle = "Sleep duration, insomnia, and observation counts by day of week"
  )

print(p_combined)

ggsave(
  file.path(figure_dir, "weekday_combined.png"),
  p_combined,
  width = 14,
  height = 12,
  dpi = 300
)

# =============================================================================
# MODELS
# =============================================================================

reference_day <- c(
  intersect("Monday", levels(dat_weekday$day_of_week)),
  levels(dat_weekday$day_of_week)[1]
) |>
  purrr::pluck(1)

dat_adjusted <- dat_weekday |>
  mutate(
    day_of_week = fct_relevel(day_of_week, reference_day),
    year_month = factor(format(date, "%Y-%m"))
  ) |>
  drop_na(bedtime, coffee, stress, health, exercise)

models_weekday <- list(
  "Raw" = feols(
    duration ~ i(day_of_week, ref = reference_day),
    data = dat_weekday,
    vcov = "hetero"
  ),
  "Adjusted" = feols(
    duration ~ i(day_of_week, ref = reference_day) +
      bedtime + coffee + stress + health + exercise,
    data = dat_adjusted,
    vcov = "hetero"
  ),
  "Month FE" = feols(
    duration ~ i(day_of_week, ref = reference_day) +
      bedtime + coffee + stress + health + exercise |
      year_month,
    data = dat_adjusted,
    vcov = "hetero"
  )
)

purrr::iwalk(
  models_weekday,
  \(model, model_name) {
    cat("\n==========", toupper(model_name), "WEEKDAY DIFFERENCES ==========\n")
    print(summary(model))
  }
)

model_comparison <- tibble(
  model = names(models_weekday),
  n = purrr::map_int(models_weekday, nobs),
  rmse = purrr::map_dbl(models_weekday, \(model) sqrt(mean(resid(model)^2)))
)

cat("\n========== MODEL COMPARISON ==========\n")
print(model_comparison)

# =============================================================================
# REGRESSION COEFFICIENT PLOT
# =============================================================================

get_weekday_results <- function(model_results) {
  purrr::map2_dfr(
    model_results,
    names(model_results),
    \(model, model_name) {
      coefs <- coef(model)
      ses <- se(model)

      tibble(
        term = names(coefs),
        estimate = as.numeric(coefs),
        std_error = as.numeric(ses)
      ) |>
        filter(str_detect(term, "^day_of_week::")) |>
        transmute(
          model = model_name,
          weekday = str_remove(term, "^day_of_week::"),
          estimate_hours = estimate,
          ci_low_hours = estimate - 1.96 * std_error,
          ci_high_hours = estimate + 1.96 * std_error,
          estimate_minutes = estimate_hours * 60,
          ci_low_minutes = ci_low_hours * 60,
          ci_high_minutes = ci_high_hours * 60
        )
    }
  )
}

weekday_regression_results <- get_weekday_results(models_weekday) |>
  mutate(
    model = factor(model, levels = c("Raw", "Adjusted", "Month FE")),
    weekday = factor(weekday, levels = rev(levels(dat_adjusted$day_of_week)))
  )

p_weekday_regression <- weekday_regression_results |>
  ggplot(
    aes(
      y = weekday,
      x = estimate_minutes,
      xmin = ci_low_minutes,
      xmax = ci_high_minutes,
      color = model
    )
  ) +
  geom_linerange(
    linewidth = 2,
    alpha = 0.6,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(
    size = 2.2,
    position = position_dodge(width = 0.55)
  ) +
  geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "Raw" = col_light_blue,
      "Adjusted" = col_steel,
      "Month FE" = col_orange
    )
  ) +
  scale_x_continuous(
    labels = \(x) paste0(round(x), " min"),
    breaks = scales::breaks_pretty(n = 6)
  ) +
  labs(
    title = "Weekday differences in sleep duration",
    subtitle = paste0("Estimated difference relative to ", reference_day),
    x = "Difference in sleep duration",
    y = NULL,
    color = NULL
  ) +
  theme_sleep() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(color = "grey90")
  )

print(p_weekday_regression)

ggsave(
  file.path(figure_dir, "weekday_regression_coefficients.png"),
  p_weekday_regression,
  width = 10,
  height = 6,
  dpi = 300
)

# =============================================================================
# ADJUSTED PREDICTIONS
# =============================================================================

m_adjusted_prediction <- lm(
  duration ~ day_of_week + bedtime + coffee + stress + health + exercise,
  data = dat_adjusted
)

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
    pred_duration = predict(
      m_adjusted_prediction,
      newdata = as.data.frame(pick(everything()))
    )
  )

p_adjusted <- pred_weekday |>
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

print(p_adjusted)

ggsave(
  file.path(figure_dir, "weekday_adjusted_prediction.png"),
  p_adjusted,
  width = 10,
  height = 6,
  dpi = 300
)

# =============================================================================
# REPORTING SUMMARY
# =============================================================================

cat("\n========== REPORTING SUMMARY ==========\n")
cat(
  "The script estimates raw, adjusted, and month fixed-effect weekday differences",
  "in sleep duration relative to", reference_day, ".\n"
)
cat("Main regression figure saved to:", file.path(figure_dir, "weekday_regression_coefficients.png"), "\n")
cat("Other figures saved to:", figure_dir, "\n")
