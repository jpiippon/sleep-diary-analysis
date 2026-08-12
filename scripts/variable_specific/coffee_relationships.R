# =============================================================================
# coffee_relationships.R
#
# Purpose: Supporting analyses for coffee as a binary exposure.
#
# Research questions:
#   Is any coffee intake associated with early/late bedtime?
#   Is any coffee intake associated with exercise?
#   Do coffee-bedtime and coffee-exercise combinations show different sleep outcomes?
#   In which situations is any coffee associated with shorter same-night sleep?
#
# Input:
#   df_clean from scripts/01_load_main_data.R
#
# Outputs:
#   - descriptive summaries printed to console
#   - supporting figures saved to figures/variable_specific/coffee/
#   - simple supporting models printed to console
#
# Notes:
#   - This script complements scripts/variable_specific/coffee.R.
#   - Coffee categories 1, 2, and 3 are collapsed into "Any coffee".
#   - Results are associations, not causal effects.
# =============================================================================

library(tidyverse)
library(fixest)
library(here)
library(patchwork)

source(here("scripts", "01_load_main_data.R"))

if (!exists("df_clean")) {
  stop("df_clean not found. Run 01_load_main_data.R first.")
}

figure_dir <- here("figures", "variable_specific", "coffee")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# HELPERS
# =============================================================================

col_navy       <- "#002d5a"
col_dark_blue  <- "#2f4a73"
col_steel      <- "#4a7ba7"
col_light_blue <- "#a3c1d9"
col_orange     <- "#CC5500"
col_dark_text  <- "#2a2a2a"
col_grey       <- "grey40"

any_palette <- c("No coffee" = col_light_blue, "Any coffee" = col_orange)

theme_sleep <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 11, color = col_grey, hjust = 0),
      plot.caption = element_text(size = 9, color = "grey50"),
      plot.margin = margin(15, 15, 15, 15),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

save_plot <- function(plot, filename, width = 10, height = 6) {
  ggsave(file.path(figure_dir, filename), plot, width = width, height = height, dpi = 300)
}

fmt_pct <- function(x, accuracy = 1) scales::percent(x, accuracy = accuracy)

pick_reference <- function(x, preferred) {
  c(intersect(preferred, levels(x)), levels(x)[1]) |>
    purrr::pluck(1)
}

safe_feols <- function(fml, data, model_name) {
  data <- prepare_nw_data(data, fml)

  tryCatch(
    feols(fml = fml, data = data, vcov = NW(7) ~ series_id + date),
    error = \(e) {
      warning("Model failed: ", model_name, ". Error: ", conditionMessage(e))
      NULL
    }
  )
}

safe_feglm <- function(fml, data, model_name) {
  data <- prepare_nw_data(data, fml)

  tryCatch(
    feglm(fml = fml, data = data, family = binomial(link = "logit"), vcov = NW(7) ~ series_id + date),
    error = \(e) {
      warning("Model failed: ", model_name, ". Error: ", conditionMessage(e))
      NULL
    }
  )
}

rate_summary <- function(data, group_var, outcome_var) {
  data |>
    group_by({{ group_var }}) |>
    summarise(
      n = n(),
      outcome_n = sum({{ outcome_var }} == 1, na.rm = TRUE),
      rate = mean({{ outcome_var }} == 1, na.rm = TRUE),
      se = sqrt(rate * (1 - rate) / n),
      ci_low = pmax(rate - 1.96 * se, 0),
      ci_high = pmin(rate + 1.96 * se, 1),
      .groups = "drop"
    ) |>
    mutate(label = fmt_pct(rate, accuracy = 1))
}

mean_summary <- function(data, group_vars) {
  data |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      n = n(),
      mean_sleep = mean(duration, na.rm = TRUE),
      se = sd(duration, na.rm = TRUE) / sqrt(n),
      ci_low = mean_sleep - 1.96 * se,
      ci_high = mean_sleep + 1.96 * se,
      .groups = "drop"
    )
}

# =============================================================================
# ANALYSIS DATA
# =============================================================================

dat_coffee_rel <- df_clean |>
  arrange(date) |>
  mutate(
    year_month = factor(format(date, "%Y-%m")),
    coffee_any = factor(
      if_else(as.character(coffee) == "None", "No coffee", "Any coffee"),
      levels = c("No coffee", "Any coffee")
    ),
    bedtime_late = factor(
      if_else(as.character(bedtime) == "Before 23:00", "Before 23:00", "23:00 or later"),
      levels = c("Before 23:00", "23:00 or later")
    ),
    exercise_any = factor(
      if_else(as.character(exercise) == "None", "No exercise", "Any exercise"),
      levels = c("No exercise", "Any exercise")
    ),
    late_bedtime_num = as.integer(bedtime_late == "23:00 or later"),
    exercise_any_num = as.integer(exercise_any == "Any exercise"),
    insomnia_any = as.integer(insomnia_num > 0),
    prev_duration = lag_by_calendar_days(duration, date, 1),
    prev_short_sleep = factor(
      case_when(
        is.na(prev_duration) ~ NA_character_,
        prev_duration < 6 ~ "Previous night <6 h",
        TRUE ~ "Previous night >=6 h"
      ),
      levels = c("Previous night >=6 h", "Previous night <6 h")
    )
  ) |>
  select(
    date, series_id, year_month, day_of_week, duration, prev_duration, prev_short_sleep,
    insomnia_any, coffee, coffee_any, bedtime, bedtime_late,
    exercise, exercise_any, stress, health,
    late_bedtime_num, exercise_any_num
  ) |>
  drop_na(coffee_any, bedtime_late, exercise_any, duration)

n_total <- nrow(dat_coffee_rel)

cat("\n========== COFFEE RELATIONSHIPS SAMPLE ==========\n")
cat("Observations:", n_total, "\n")
cat("Date range:", format(min(dat_coffee_rel$date), "%Y-%m-%d"), "to", format(max(dat_coffee_rel$date), "%Y-%m-%d"), "\n")

# =============================================================================
# SUMMARIES
# =============================================================================

late_bedtime_by_coffee <- dat_coffee_rel |>
  rate_summary(coffee_any, late_bedtime_num)

exercise_by_coffee <- dat_coffee_rel |>
  rate_summary(coffee_any, exercise_any_num)

coffee_by_bedtime <- dat_coffee_rel |>
  count(bedtime_late, coffee_any, name = "n") |>
  group_by(bedtime_late) |>
  mutate(share = n / sum(n)) |>
  ungroup()

coffee_by_exercise <- dat_coffee_rel |>
  count(exercise_any, coffee_any, name = "n") |>
  group_by(exercise_any) |>
  mutate(share = n / sum(n)) |>
  ungroup()

sleep_by_coffee_bedtime <- dat_coffee_rel |>
  mean_summary(c("coffee_any", "bedtime_late"))

sleep_by_coffee_exercise <- dat_coffee_rel |>
  mean_summary(c("coffee_any", "exercise_any"))

cat("\n========== LATE BEDTIME BY ANY COFFEE ==========\n")
print(late_bedtime_by_coffee, n = Inf, width = Inf)

cat("\n========== EXERCISE BY ANY COFFEE ==========\n")
print(exercise_by_coffee, n = Inf, width = Inf)

cat("\n========== SLEEP BY COFFEE AND BEDTIME ==========\n")
print(sleep_by_coffee_bedtime, n = Inf, width = Inf)

cat("\n========== SLEEP BY COFFEE AND EXERCISE ==========\n")
print(sleep_by_coffee_exercise, n = Inf, width = Inf)

# =============================================================================
# FIGURES
# =============================================================================

p_late_bedtime_by_coffee <- late_bedtime_by_coffee |>
  ggplot(aes(x = coffee_any, y = rate, fill = coffee_any)) +
  geom_col(alpha = 0.85, width = 0.62) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, color = col_dark_blue) +
  geom_text(aes(label = label), vjust = -0.8, size = 3.4, color = col_dark_text) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, min(1, max(late_bedtime_by_coffee$ci_high, na.rm = TRUE) + 0.10))) +
  scale_fill_manual(values = any_palette, guide = "none") +
  labs(
    title = "Late bedtime is more directly checked with any coffee vs no coffee",
    subtitle = "Share of nights with bedtime at 23:00 or later",
    x = NULL,
    y = "Late bedtime share"
  ) +
  theme_sleep()

p_coffee_by_bedtime <- coffee_by_bedtime |>
  ggplot(aes(x = bedtime_late, y = share, fill = coffee_any)) +
  geom_col(alpha = 0.90, width = 0.70) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = any_palette) +
  labs(
    title = "Coffee composition differs across bedtime groups",
    subtitle = "Any coffee vs no coffee within early and late bedtime groups",
    x = NULL,
    y = "Share of nights",
    fill = NULL
  ) +
  theme_sleep()

p_exercise_by_coffee <- exercise_by_coffee |>
  ggplot(aes(x = coffee_any, y = rate, fill = coffee_any)) +
  geom_col(alpha = 0.85, width = 0.62) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, color = col_dark_blue) +
  geom_text(aes(label = label), vjust = -0.8, size = 3.4, color = col_dark_text) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, min(1, max(exercise_by_coffee$ci_high, na.rm = TRUE) + 0.10))) +
  scale_fill_manual(values = any_palette, guide = "none") +
  labs(
    title = "Exercise is compared for any coffee vs no coffee",
    subtitle = "Share of nights with any recorded exercise",
    x = NULL,
    y = "Any exercise share"
  ) +
  theme_sleep()

p_coffee_by_exercise <- coffee_by_exercise |>
  ggplot(aes(x = exercise_any, y = share, fill = coffee_any)) +
  geom_col(alpha = 0.90, width = 0.70) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = any_palette) +
  labs(
    title = "Coffee composition differs across exercise groups",
    subtitle = "Any coffee vs no coffee within exercise groups",
    x = NULL,
    y = "Share of nights",
    fill = NULL
  ) +
  theme_sleep()

p_sleep_by_coffee_bedtime <- sleep_by_coffee_bedtime |>
  ggplot(aes(x = bedtime_late, y = mean_sleep, color = coffee_any, group = coffee_any)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, position = position_dodge(width = 0.30), alpha = 0.8) +
  geom_line(linewidth = 1.0, position = position_dodge(width = 0.30)) +
  geom_point(size = 2.6, position = position_dodge(width = 0.30)) +
  scale_color_manual(values = any_palette) +
  labs(
    title = "Sleep duration by coffee and bedtime timing",
    subtitle = "Mean sleep duration with approximate 95% confidence intervals",
    x = NULL,
    y = "Mean sleep duration (hours)",
    color = NULL
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_sleep_by_coffee_exercise <- sleep_by_coffee_exercise |>
  ggplot(aes(x = exercise_any, y = mean_sleep, color = coffee_any, group = coffee_any)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, position = position_dodge(width = 0.30), alpha = 0.8) +
  geom_line(linewidth = 1.0, position = position_dodge(width = 0.30)) +
  geom_point(size = 2.6, position = position_dodge(width = 0.30)) +
  scale_color_manual(values = any_palette) +
  labs(
    title = "Sleep duration by coffee and exercise",
    subtitle = "Mean sleep duration with approximate 95% confidence intervals",
    x = NULL,
    y = "Mean sleep duration (hours)",
    color = NULL
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_main <- (p_late_bedtime_by_coffee + p_exercise_by_coffee) /
  (p_sleep_by_coffee_bedtime + p_sleep_by_coffee_exercise) +
  plot_annotation(
    title = "Any coffee is linked to bedtime timing, exercise, and sleep duration",
    subtitle = "Supporting checks using coffee/no coffee instead of detailed coffee categories",
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(size = 14, face = "bold"))

# =============================================================================
# SUPPORTING MODELS
# =============================================================================

dat_model <- dat_coffee_rel |>
  mutate(
    coffee_any = fct_relevel(coffee_any, "No coffee"),
    bedtime_late = fct_relevel(bedtime_late, "Before 23:00"),
    exercise_any = fct_relevel(exercise_any, "No exercise"),
    bedtime = factor(bedtime, levels = levels(bedtime), ordered = FALSE),
    stress = factor(stress, levels = levels(stress), ordered = FALSE),
    health = factor(health, levels = levels(health), ordered = FALSE),
    exercise = factor(exercise, levels = levels(exercise), ordered = FALSE),
    day_of_week = fct_drop(day_of_week),
    year_month = fct_drop(year_month)
  ) |>
  drop_na(coffee_any, bedtime_late, exercise_any, duration, stress, health, exercise, day_of_week, year_month) |>
  prepare_nw_data()

reference_bedtime <- pick_reference(dat_model$bedtime, "Before 23:00")
reference_stress <- pick_reference(dat_model$stress, "No")
reference_health <- pick_reference(dat_model$health, "Healthy")
reference_exercise <- pick_reference(dat_model$exercise, "None")
reference_day <- pick_reference(dat_model$day_of_week, c("Mon", "Monday"))

models_supporting <- list(
  "Late bedtime by any coffee" = safe_feglm(
    late_bedtime_num ~ i(coffee_any, ref = "No coffee") +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(exercise, ref = reference_exercise) +
      i(day_of_week, ref = reference_day) |
      year_month,
    dat_model,
    "Late bedtime by any coffee"
  ),
  "Exercise by any coffee" = safe_feglm(
    exercise_any_num ~ i(coffee_any, ref = "No coffee") +
      i(bedtime, ref = reference_bedtime) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(day_of_week, ref = reference_day) |
      year_month,
    dat_model,
    "Exercise by any coffee"
  ),
  "Sleep duration: coffee x bedtime" = safe_feols(
    duration ~ coffee_any * bedtime_late +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(exercise, ref = reference_exercise) +
      i(day_of_week, ref = reference_day) |
      year_month,
    dat_model,
    "Sleep duration: coffee x bedtime"
  ),
  "Sleep duration: coffee x exercise" = safe_feols(
    duration ~ coffee_any * exercise_any +
      i(bedtime, ref = reference_bedtime) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(day_of_week, ref = reference_day) |
      year_month,
    dat_model,
    "Sleep duration: coffee x exercise"
  )
) |>
  purrr::compact()

purrr::iwalk(
  models_supporting,
  \(model, model_name) {
    cat("\n========== SUPPORTING MODEL:", toupper(model_name), "==========\n")
    print(summary(model))
  }
)

# =============================================================================
# CONTEXT CHECKS: WHEN IS COFFEE ASSOCIATED WITH SHORTER SLEEP?
# =============================================================================

plot_context_sleep <- function(summary_data, x_var, plot_title, plot_subtitle) {
  summary_data |>
    ggplot(aes(x = .data[[x_var]], y = mean_sleep, color = coffee_any, group = coffee_any)) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, position = position_dodge(width = 0.30), alpha = 0.8) +
    geom_line(linewidth = 1.0, position = position_dodge(width = 0.30)) +
    geom_point(size = 2.6, position = position_dodge(width = 0.30)) +
    scale_color_manual(values = any_palette) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = NULL,
      y = "Mean sleep duration (hours)",
      color = NULL
    ) +
    coord_cartesian(ylim = c(0, NA)) +
    theme_sleep()
}

print_context_model <- function(model, model_name) {
  if (is.null(model)) {
    cat("\n========== CONTEXT MODEL FAILED:", toupper(model_name), "==========\n")
    return(invisible(NULL))
  }

  cat("\n========== CONTEXT MODEL:", toupper(model_name), "==========\n")
  print(summary(model))
  invisible(model)
}

dat_context <- dat_coffee_rel |>
  mutate(
    coffee_any = fct_relevel(coffee_any, "No coffee"),
    bedtime_late = fct_relevel(bedtime_late, "Before 23:00"),
    exercise_any = fct_relevel(exercise_any, "No exercise"),
    prev_short_sleep = fct_relevel(prev_short_sleep, "Previous night >=6 h"),
    stress_any = factor(
      case_when(
        is.na(stress) ~ NA_character_,
        stress == "No" ~ "No stress",
        TRUE ~ "Stress"
      ),
      levels = c("No stress", "Stress")
    ),
    bedtime = factor(bedtime, levels = levels(bedtime), ordered = FALSE),
    stress = factor(stress, levels = levels(stress), ordered = FALSE),
    health = factor(health, levels = levels(health), ordered = FALSE),
    exercise = factor(exercise, levels = levels(exercise), ordered = FALSE),
    day_of_week = fct_drop(day_of_week),
    year_month = fct_drop(year_month)
  )

context_bedtime_summary <- dat_context |>
  drop_na(coffee_any, bedtime_late, duration) |>
  mean_summary(c("coffee_any", "bedtime_late"))

context_prev_sleep_summary <- dat_context |>
  drop_na(coffee_any, prev_short_sleep, duration) |>
  mean_summary(c("coffee_any", "prev_short_sleep"))

context_exercise_summary <- dat_context |>
  drop_na(coffee_any, exercise_any, duration) |>
  mean_summary(c("coffee_any", "exercise_any"))

context_stress_summary <- dat_context |>
  drop_na(coffee_any, stress_any, duration) |>
  mean_summary(c("coffee_any", "stress_any"))

p_context_bedtime <- plot_context_sleep(
  context_bedtime_summary,
  "bedtime_late",
  "Mean sleep by any coffee and bedtime timing",
  "This check compares sleep duration within early and late bedtime nights."
)

p_context_previous_sleep <- plot_context_sleep(
  context_prev_sleep_summary,
  "prev_short_sleep",
  "Mean sleep by any coffee and previous-night sleep",
  "This check compares sleep duration after shorter and longer previous nights."
)

p_context_exercise <- plot_context_sleep(
  context_exercise_summary,
  "exercise_any",
  "Mean sleep by any coffee and exercise",
  "This check compares sleep duration within exercise and no-exercise nights."
)

p_context_stress <- plot_context_sleep(
  context_stress_summary,
  "stress_any",
  "Mean sleep by any coffee and stress",
  "This optional check compares sleep duration within stress and no-stress nights."
)

model_context_bedtime <- safe_feols(
  duration ~ coffee_any * bedtime_late +
    i(stress, ref = reference_stress) +
    i(health, ref = reference_health) +
    i(exercise, ref = reference_exercise) +
    i(day_of_week, ref = reference_day) |
    year_month,
  data = dat_context |>
    drop_na(coffee_any, bedtime_late, duration, stress, health, exercise, day_of_week, year_month),
  model_name = "Any coffee x late bedtime"
)

model_context_previous_sleep <- safe_feols(
  duration ~ coffee_any * prev_short_sleep +
    i(bedtime, ref = reference_bedtime) +
    i(stress, ref = reference_stress) +
    i(health, ref = reference_health) +
    i(exercise, ref = reference_exercise) +
    i(day_of_week, ref = reference_day) |
    year_month,
  data = dat_context |>
    drop_na(coffee_any, prev_short_sleep, duration, bedtime, stress, health, exercise, day_of_week, year_month),
  model_name = "Any coffee x previous-night short sleep"
)

model_context_exercise <- safe_feols(
  duration ~ coffee_any * exercise_any +
    i(bedtime, ref = reference_bedtime) +
    i(stress, ref = reference_stress) +
    i(health, ref = reference_health) +
    i(day_of_week, ref = reference_day) |
    year_month,
  data = dat_context |>
    drop_na(coffee_any, exercise_any, duration, bedtime, stress, health, day_of_week, year_month),
  model_name = "Any coffee x exercise"
)

model_context_stress <- safe_feols(
  duration ~ coffee_any * stress_any +
    i(bedtime, ref = reference_bedtime) +
    i(health, ref = reference_health) +
    i(exercise, ref = reference_exercise) +
    i(day_of_week, ref = reference_day) |
    year_month,
  data = dat_context |>
    drop_na(coffee_any, stress_any, duration, bedtime, health, exercise, day_of_week, year_month),
  model_name = "Any coffee x stress"
)

print_context_model(model_context_bedtime, "Any coffee x late bedtime")
print_context_model(model_context_previous_sleep, "Any coffee x previous-night short sleep")
print_context_model(model_context_exercise, "Any coffee x exercise")
print_context_model(model_context_stress, "Any coffee x stress")

# =============================================================================
# SAVE FIGURES
# =============================================================================

print(p_main)
print(p_late_bedtime_by_coffee)
print(p_coffee_by_bedtime)
print(p_exercise_by_coffee)
print(p_coffee_by_exercise)
print(p_sleep_by_coffee_bedtime)
print(p_sleep_by_coffee_exercise)
print(p_context_bedtime)
print(p_context_previous_sleep)
print(p_context_exercise)
print(p_context_stress)

save_plot(p_main, "coffee_figureS13_any_coffee_bedtime_exercise_main.png", width = 14, height = 10)
save_plot(p_late_bedtime_by_coffee, "coffee_figureS14_late_bedtime_by_any_coffee.png", width = 8, height = 6)
save_plot(p_coffee_by_bedtime, "coffee_figureS15_coffee_composition_by_bedtime.png", width = 8, height = 6)
save_plot(p_exercise_by_coffee, "coffee_figureS16_exercise_by_any_coffee.png", width = 8, height = 6)
save_plot(p_coffee_by_exercise, "coffee_figureS17_coffee_composition_by_exercise.png", width = 8, height = 6)
save_plot(p_sleep_by_coffee_bedtime, "coffee_figureS18_sleep_by_coffee_and_bedtime.png", width = 9, height = 6)
save_plot(p_sleep_by_coffee_exercise, "coffee_figureS19_sleep_by_coffee_and_exercise.png", width = 9, height = 6)
save_plot(p_context_bedtime, "coffee_figureS20_context_bedtime.png", width = 9, height = 6)
save_plot(p_context_previous_sleep, "coffee_figureS21_context_previous_sleep.png", width = 9, height = 6)
save_plot(p_context_exercise, "coffee_figureS22_context_exercise.png", width = 9, height = 6)
save_plot(p_context_stress, "coffee_figureS23_context_stress.png", width = 9, height = 6)

cat("\n========== REPORTING SUMMARY ==========\n")
cat("Supporting coffee relationship figures saved to:", figure_dir, "\n")
cat("Recommended supporting overview figure: coffee_figureS13_any_coffee_bedtime_exercise_main.png\n")
cat("Context-check figures saved to: coffee_figureS20_context_bedtime.png, coffee_figureS21_context_previous_sleep.png, coffee_figureS22_context_exercise.png\n")
cat("Optional stress check saved to: coffee_figureS23_context_stress.png\n")
