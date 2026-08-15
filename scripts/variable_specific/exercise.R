# =============================================================================
# exercise.R
#
# Purpose: Separate the same-night association of exercise timing from the
# association between recent exercise frequency and sleep duration.
#
# Research questions:
#   RQ1. How often are no exercise, earlier exercise, and heavy exercise after
#        18:00 recorded, and how have these categories changed over time?
#   RQ2. How is earlier exercise associated with same-night sleep duration
#        compared with no exercise?
#   RQ3. Is heavy exercise after 18:00 associated with shorter same-night sleep?
#   RQ4. Is exercise frequency over the previous 7, 14, or 28 days associated
#        with sleep duration, separately from exercise on the current day?
#   RQ5. Are the results sensitive to adjustment for bedtime, previous-night
#        sleep, or a nonlinear 28-day exercise-frequency relationship?
#
# Input:
#   df_clean from scripts/01_load_main_data.R
#
# Outputs:
#   - descriptive and model summaries printed to the console
#   - one four-panel main figure for public-facing reporting
#   - a small set of supporting figures
#   - figures saved to outputs/figures/variable_specific/exercise/
#   - no CSV files
#
# Notes for interpretation:
#   - Exercise coding is 0 = none, 1 = earlier in the day, and
#     2 = heavy exercise after 18:00.
#   - The diary date is the exposure day and the night that starts on that date.
#     Same-date sleep therefore follows that day's exercise.
#   - Recent exercise frequency excludes the current day and requires every
#     calendar date in the stated look-back window. Diary gaps are not bridged.
#     The 28-day models therefore use a smaller, more continuously observed
#     subset of the diary and may not represent every year equally.
#   - A 28-day exercise history is an operational measure of recent behavior;
#     it does not directly measure fitness or a causal long-term training effect.
#   - Previous-night sleep is included in the primary same-day model because it
#     can affect both the decision to exercise and the following night's sleep.
#   - Previous-night sleep is only a sensitivity control in the recent-history
#     model because it could also lie on a pathway from earlier exercise.
#   - Bedtime occurs after exercise and may be part of the exercise-to-sleep
#     pathway. It is therefore included only as a sensitivity check.
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

variable_name <- "exercise"
figure_dir <- here("outputs", "figures", "variable_specific", variable_name)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SETTINGS AND HELPERS
# =============================================================================

col_navy       <- "#002d5a"
col_dark_blue  <- "#2f4a73"
col_steel      <- "#4a7ba7"
col_pale_blue  <- "#d0e1ef"
col_orange     <- "#CC5500"
col_dark_text  <- "#2a2a2a"
col_grey       <- "grey40"

exercise_palette <- c(
  "None" = col_navy,
  "Earlier in the day" = col_steel,
  "Heavy after 18:00" = col_orange
)

model_palette <- c(
  "Raw" = col_pale_blue,
  "Calendar adjusted" = col_steel,
  "Previous-sleep adjusted" = col_dark_blue,
  "Plus bedtime sensitivity" = col_orange
)

theme_sleep <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 11, color = col_grey, hjust = 0),
      plot.caption = element_text(size = 9, color = "grey50", hjust = 0),
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
  ggsave(
    filename = file.path(figure_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

safe_feols <- function(fml, data, model_name) {
  model_data <- prepare_nw_data(data, fml)

  tryCatch(
    feols(
      fml = fml,
      data = model_data,
      vcov = NW(7) ~ series_id + date
    ),
    error = \(e) {
      warning("Model failed: ", model_name, ". Error: ", conditionMessage(e))
      NULL
    }
  )
}

fmt_pct <- function(x, accuracy = 1) {
  scales::percent(x, accuracy = accuracy)
}

fmt_min <- function(x) {
  paste0(if_else(x > 0, "+", ""), round(x), " min")
}

# Sum a binary diary variable over exact previous calendar days. A result is
# returned only when every date in the window is observed.
calendar_window_sum <- function(x, date, lookback_days) {
  lagged_values <- lookback_days |>
    set_names(paste0("lag_", lookback_days)) |>
    purrr::map(\(n_days) lag_by_calendar_days(x, date, n_days)) |>
    as_tibble()

  complete_window <- rowSums(is.na(lagged_values)) == 0
  window_sum <- rowSums(lagged_values, na.rm = TRUE)

  if_else(complete_window, window_sum, NA_real_)
}

clean_exercise_term <- function(x) {
  x |>
    str_remove_all("`") |>
    str_remove("^exercise::")
}

extract_acute_results <- function(model_results) {
  purrr::imap_dfr(
    model_results,
    \(model, model_name) {
      tibble(
        term = names(coef(model)),
        estimate = as.numeric(coef(model)),
        std_error = as.numeric(se(model))
      ) |>
        filter(str_detect(term, "^exercise::")) |>
        transmute(
          model = model_name,
          exercise = clean_exercise_term(term),
          estimate_minutes = estimate * 60,
          ci_low_minutes = (estimate - 1.96 * std_error) * 60,
          ci_high_minutes = (estimate + 1.96 * std_error) * 60,
          label = fmt_min(estimate_minutes)
        )
    }
  )
}

extract_numeric_result <- function(model, predictor, label) {
  if (is.null(model) || !predictor %in% names(coef(model))) {
    return(tibble())
  }

  estimate <- unname(coef(model)[[predictor]])
  std_error <- unname(se(model)[[predictor]])

  tibble(
    label_group = label,
    predictor = predictor,
    n = nobs(model),
    estimate_minutes = estimate * 60,
    ci_low_minutes = (estimate - 1.96 * std_error) * 60,
    ci_high_minutes = (estimate + 1.96 * std_error) * 60,
    label = fmt_min(estimate_minutes)
  )
}

# =============================================================================
# ANALYSIS DATA
# =============================================================================

dat_exercise <- df_clean |>
  arrange(date) |>
  mutate(
    year_num = as.integer(format(date, "%Y")),
    month_num = as.integer(format(date, "%m")),
    year_month = factor(format(date, "%Y-%m")),
    exercise_any_num = case_when(
      exercise_code == 0 ~ 0L,
      exercise_code %in% 1:2 ~ 1L,
      TRUE ~ NA_integer_
    ),
    prev_duration = lag_by_calendar_days(duration, date, 1)
  ) |>
  mutate(
    exercise_days_7 = calendar_window_sum(exercise_any_num, date, 1:7),
    exercise_days_14 = calendar_window_sum(exercise_any_num, date, 1:14),
    exercise_days_28 = calendar_window_sum(exercise_any_num, date, 1:28),
    exercise_days_per_week_7 = exercise_days_7,
    exercise_days_per_week_14 = exercise_days_14 / 2,
    exercise_days_per_week_28 = exercise_days_28 / 4,
    exercise = factor(exercise, levels = levels(exercise), ordered = FALSE),
    bedtime = factor(bedtime, levels = levels(bedtime), ordered = FALSE),
    coffee = factor(coffee, levels = levels(coffee), ordered = FALSE),
    stress = factor(stress, levels = levels(stress), ordered = FALSE),
    health = factor(health, levels = levels(health), ordered = FALSE),
    day_of_week = factor(day_of_week, levels = levels(day_of_week), ordered = FALSE)
  ) |>
  select(
    date,
    series_id,
    year_num,
    month_num,
    year_month,
    day_of_week,
    duration,
    prev_duration,
    exercise_code,
    exercise,
    exercise_any_num,
    exercise_days_7,
    exercise_days_14,
    exercise_days_28,
    exercise_days_per_week_7,
    exercise_days_per_week_14,
    exercise_days_per_week_28,
    bedtime,
    coffee,
    stress,
    health
  ) |>
  drop_na(exercise, duration)

n_total <- nrow(dat_exercise)
any_exercise_share <- mean(dat_exercise$exercise_any_num)

cat("\n========== EXERCISE ANALYSIS SAMPLE ==========\n")
cat("Observations:", n_total, "\n")
cat(
  "Date range:", format(min(dat_exercise$date), "%Y-%m-%d"), "to",
  format(max(dat_exercise$date), "%Y-%m-%d"), "\n"
)
cat("Exercise days:", sum(dat_exercise$exercise_any_num), "\n")
cat("No-exercise days:", sum(dat_exercise$exercise_any_num == 0), "\n")

# =============================================================================
# DESCRIPTIVE SUMMARIES
# =============================================================================

exercise_summary <- dat_exercise |>
  group_by(exercise) |>
  summarise(
    n = n(),
    share = n / nrow(dat_exercise),
    mean_sleep = mean(duration),
    median_sleep = median(duration),
    .groups = "drop"
  ) |>
  mutate(
    distribution_label = paste0(fmt_pct(share), "\n(n=", n, ")"),
    median_label = paste0("Median: ", round(median_sleep, 1), " h")
  )

year_coverage <- dat_exercise |>
  group_by(year_num) |>
  summarise(
    n = n(),
    observed_months = n_distinct(month_num),
    .groups = "drop"
  ) |>
  mutate(
    full_calendar_year = observed_months == 12,
    year_label = paste0(year_num, if_else(full_calendar_year, "", "*")),
    year_label = factor(year_label, levels = year_label)
  )

yearly_exercise_summary <- dat_exercise |>
  count(year_num, exercise, name = "n") |>
  group_by(year_num) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  left_join(
    year_coverage |>
      select(year_num, year_label),
    by = "year_num"
  )

history_predictors <- c(
  "Past 7 days" = "exercise_days_per_week_7",
  "Past 14 days" = "exercise_days_per_week_14",
  "Past 28 days" = "exercise_days_per_week_28"
)

history_coverage <- purrr::imap_dfr(
  history_predictors,
  \(predictor, window_label) {
    values <- dat_exercise[[predictor]]

    tibble(
      window = window_label,
      complete_n = sum(!is.na(values)),
      mean_days_per_week = mean(values, na.rm = TRUE),
      median_days_per_week = median(values, na.rm = TRUE)
    )
  }
)

cat("\n========== SLEEP DURATION BY SAME-DAY EXERCISE ==========\n")
print(exercise_summary, n = Inf, width = Inf)
cat("\n========== COMPLETE EXERCISE-HISTORY WINDOWS ==========\n")
print(history_coverage, n = Inf, width = Inf)

# =============================================================================
# SAME-DAY EXERCISE MODELS
# =============================================================================

# All same-day models use one complete-case sample so model differences reflect
# the adjustment set rather than changes in included diary days.
dat_acute_model <- dat_exercise |>
  drop_na(
    duration,
    prev_duration,
    exercise,
    bedtime,
    coffee,
    stress,
    health,
    day_of_week,
    year_month
  ) |>
  mutate(
    exercise = fct_relevel(fct_drop(exercise), "None"),
    bedtime = fct_relevel(fct_drop(bedtime), "Before 23:00"),
    coffee = fct_relevel(fct_drop(coffee), "None"),
    stress = fct_relevel(fct_drop(stress), "No"),
    health = fct_relevel(fct_drop(health), "Healthy"),
    day_of_week = fct_relevel(fct_drop(day_of_week), "Mon"),
    year_month = fct_drop(year_month)
  )

reference_exercise <- levels(dat_acute_model$exercise)[1]

models_acute <- list(
  "Raw" = safe_feols(
    duration ~ i(exercise, ref = reference_exercise),
    data = dat_acute_model,
    model_name = "Raw same-day exercise model"
  ),
  "Calendar adjusted" = safe_feols(
    duration ~
      i(exercise, ref = reference_exercise) +
      day_of_week |
      year_month,
    data = dat_acute_model,
    model_name = "Calendar-adjusted same-day exercise model"
  ),
  "Previous-sleep adjusted" = safe_feols(
    duration ~
      i(exercise, ref = reference_exercise) +
      prev_duration +
      coffee +
      stress +
      health +
      day_of_week |
      year_month,
    data = dat_acute_model,
    model_name = "Previous-sleep-adjusted same-day exercise model"
  ),
  "Plus bedtime sensitivity" = safe_feols(
    duration ~
      i(exercise, ref = reference_exercise) +
      prev_duration +
      bedtime +
      coffee +
      stress +
      health +
      day_of_week |
      year_month,
    data = dat_acute_model,
    model_name = "Same-day exercise model plus bedtime"
  )
) |>
  purrr::compact()

purrr::iwalk(
  models_acute,
  \(model, model_name) {
    cat("\n==========", toupper(model_name), "==========\n")
    print(summary(model))
  }
)

acute_results <- extract_acute_results(models_acute) |>
  mutate(
    model = factor(model, levels = names(models_acute)),
    exercise = factor(
      exercise,
      levels = setdiff(levels(dat_acute_model$exercise), reference_exercise)
    )
  )

cat("\n========== SAME-DAY EXERCISE ESTIMATES ==========\n")
print(acute_results, n = Inf, width = Inf)

acute_primary_results <- acute_results |>
  filter(model == "Previous-sleep adjusted") |>
  select(-model)

acute_main_results <- bind_rows(
  tibble(
    exercise = reference_exercise,
    estimate_minutes = 0,
    ci_low_minutes = 0,
    ci_high_minutes = 0,
    label = "Reference"
  ),
  acute_primary_results
) |>
  mutate(
    exercise = factor(
      exercise,
      levels = rev(levels(dat_acute_model$exercise))
    )
  )

# =============================================================================
# RECENT EXERCISE-FREQUENCY MODELS
# =============================================================================

# The 28-day complete-case sample is used for every window so differences among
# the 7-, 14-, and 28-day estimates are not caused by different diary samples.
dat_history_model <- dat_acute_model |>
  drop_na(exercise_days_per_week_28)

history_models <- purrr::imap(
  history_predictors,
  \(predictor, window_label) {
    model_formula <- as.formula(
      paste0(
        "duration ~ ", predictor,
        " + exercise + coffee + stress + health + day_of_week | year_month"
      )
    )

    safe_feols(
      model_formula,
      data = dat_history_model,
      model_name = paste(window_label, "exercise-frequency model")
    )
  }
) |>
  purrr::compact()

purrr::iwalk(
  history_models,
  \(model, window_label) {
    cat("\n========== EXERCISE FREQUENCY:", toupper(window_label), "==========\n")
    print(summary(model))
  }
)

history_results <- purrr::imap_dfr(
  history_models,
  \(model, window_label) {
    predictor <- history_predictors[[window_label]]
    extract_numeric_result(model, predictor, window_label)
  }
) |>
  mutate(
    label_group = factor(label_group, levels = rev(names(history_predictors)))
  )

cat("\n========== RECENT EXERCISE-FREQUENCY ESTIMATES ==========\n")
print(history_results, n = Inf, width = Inf)

# Previous-night sleep could be either a confounder or part of the pathway from
# prior exercise to current sleep, so it is added only in this sensitivity model.
model_history_28_prev_sleep <- safe_feols(
  duration ~
    exercise_days_per_week_28 +
    prev_duration +
    exercise +
    coffee +
    stress +
    health +
    day_of_week |
    year_month,
  data = dat_history_model,
  model_name = "28-day exercise-frequency model plus previous-night sleep"
)

# A quadratic sensitivity check allows a moderate frequency to differ from both
# very low and very high frequency. Coefficients are printed, not presented as a
# simple linear effect in the main figure.
model_history_28_quadratic <- safe_feols(
  duration ~
    exercise_days_per_week_28 +
    I(exercise_days_per_week_28^2) +
    exercise +
    coffee +
    stress +
    health +
    day_of_week |
    year_month,
  data = dat_history_model,
  model_name = "Quadratic 28-day exercise-frequency sensitivity"
)

purrr::iwalk(
  list(
    "28-day frequency plus previous-night sleep" = model_history_28_prev_sleep,
    "Quadratic 28-day frequency" = model_history_28_quadratic
  ) |>
    purrr::compact(),
  \(model, model_name) {
    cat("\n========== SENSITIVITY:", toupper(model_name), "==========\n")
    print(summary(model))
  }
)

history_sensitivity_results <- list(
  "Primary 28-day model" = history_models[["Past 28 days"]],
  "Plus previous-night sleep" = model_history_28_prev_sleep
) |>
  purrr::compact() |>
  purrr::imap_dfr(
    \(model, model_name) {
      extract_numeric_result(
        model,
        "exercise_days_per_week_28",
        model_name
      )
    }
  ) |>
  mutate(
    label_group = factor(
      label_group,
      levels = rev(c("Primary 28-day model", "Plus previous-night sleep"))
    )
  )

# =============================================================================
# FOUR-PANEL MAIN FIGURE
# =============================================================================

max_share <- max(exercise_summary$share)

p_distribution <- exercise_summary |>
  ggplot(aes(x = exercise, y = share, fill = exercise)) +
  geom_col(alpha = 0.9, width = 0.72) +
  geom_text(
    aes(label = distribution_label),
    vjust = -0.3,
    size = 3,
    color = col_dark_text,
    lineheight = 0.9
  ) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, max_share + 0.14)
  ) +
  scale_fill_manual(values = exercise_palette, guide = "none") +
  labs(
    title = paste0(fmt_pct(any_exercise_share), " of nights included exercise"),
    subtitle = paste0("Same-day exercise categories (N = ", n_total, ")"),
    x = NULL,
    y = "Share of nights"
  ) +
  theme_sleep()

duration_label_y <- quantile(dat_exercise$duration, 0.97, na.rm = TRUE)

p_duration <- dat_exercise |>
  ggplot(aes(x = exercise, y = duration, fill = exercise)) +
  geom_boxplot(
    alpha = 0.75,
    outlier.shape = NA,
    width = 0.62
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.07,
    size = 0.8,
    color = col_dark_text
  ) +
  geom_label(
    data = exercise_summary,
    aes(x = exercise, y = duration_label_y, label = median_label),
    inherit.aes = FALSE,
    size = 2.7,
    linewidth = 0.12,
    fill = "white",
    color = col_dark_text
  ) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  scale_fill_manual(values = exercise_palette, guide = "none") +
  coord_cartesian(ylim = c(0, NA)) +
  labs(
    title = "Raw sleep distributions largely overlap",
    subtitle = "Same-night sleep duration by exercise timing",
    x = NULL,
    y = "Sleep duration (hours)"
  ) +
  theme_sleep()

acute_estimates_uncertain <- nrow(acute_primary_results) > 0 &&
  all(
    acute_primary_results$ci_low_minutes <= 0 &
      acute_primary_results$ci_high_minutes >= 0
  )

acute_panel_title <- case_when(
  acute_estimates_uncertain ~ "Adjusted same-night differences remain uncertain",
  TRUE ~ "Adjusted same-night differences by exercise timing"
)

p_acute_main <- acute_main_results |>
  ggplot(aes(y = exercise, x = estimate_minutes)) +
  geom_segment(
    aes(x = ci_low_minutes, xend = ci_high_minutes, yend = exercise),
    linewidth = 1.1,
    color = col_dark_blue
  ) +
  geom_point(size = 2.7, color = col_orange) +
  geom_text(
    aes(x = ci_high_minutes, label = label),
    nudge_x = 2,
    nudge_y = 0.1,
    hjust = 0,
    size = 3,
    fontface = "bold",
    color = col_dark_text
  ) +
  geom_vline(xintercept = 0, linewidth = 0.35, linetype = "dashed") +
  scale_y_discrete(labels = scales::label_wrap(18)) +
  scale_x_continuous(
    labels = \(x) paste0(round(x), " min"),
    breaks = scales::breaks_pretty(n = 5),
    expand = expansion(mult = c(0.08, 0.25))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = str_wrap(acute_panel_title, width = 42),
    subtitle = "Adjusted estimates relative to no exercise",
    x = "Difference in sleep duration",
    y = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

history_estimates_uncertain <- nrow(history_results) > 0 &&
  all(
    history_results$ci_low_minutes <= 0 &
      history_results$ci_high_minutes >= 0
  )

history_panel_title <- case_when(
  history_estimates_uncertain ~ "Recent frequency was not clearly linked to longer sleep",
  TRUE ~ "Recent exercise-frequency estimates"
)

p_history_main <- history_results |>
  ggplot(aes(y = label_group, x = estimate_minutes)) +
  geom_segment(
    aes(x = ci_low_minutes, xend = ci_high_minutes, yend = label_group),
    linewidth = 1.1,
    color = col_dark_blue
  ) +
  geom_point(size = 2.7, color = col_orange) +
  geom_text(
    aes(x = ci_high_minutes, label = label),
    nudge_x = 0.8,
    nudge_y = 0.1,
    hjust = 0,
    size = 3,
    fontface = "bold",
    color = col_dark_text
  ) +
  geom_vline(xintercept = 0, linewidth = 0.35, linetype = "dashed") +
  scale_x_continuous(
    labels = \(x) paste0(round(x), " min"),
    breaks = scales::breaks_pretty(n = 5),
    expand = expansion(mult = c(0.1, 0.25))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = str_wrap(history_panel_title, width = 42),
    subtitle = str_wrap(
      "Per one additional exercise day/week; current day excluded",
      width = 55
    ),
    x = "Difference in sleep duration",
    y = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

main_title <- case_when(
  acute_estimates_uncertain && history_estimates_uncertain ~
    "Exercise timing and recent frequency show small, uncertain sleep differences",
  TRUE ~
    "Exercise timing and recent frequency show different sleep patterns"
)

p_main <- (p_distribution + p_duration) /
  (p_acute_main + p_history_main) +
  plot_layout(guides = "collect", widths = c(1, 1), heights = c(1, 1)) +
  plot_annotation(
    title = str_wrap(main_title, width = 74),
    subtitle = str_wrap(
      "Same-day exercise timing and preceding 7- to 28-day exercise patterns are analyzed separately",
      width = 105
    ),
    caption = str_wrap(
      paste(
        "Panel C adjusts for exact previous-night sleep, coffee, stress, health, weekday, and year-month.",
        "Panel D uses one common 28-day-complete sample and adjusts for current exercise, coffee, stress, health, weekday, and year-month.",
        "Intervals are 95% CIs using a 7-day Newey-West estimator.",
        "Bedtime and previous-night sleep in the history model are sensitivity checks.",
        "Panels A and B are descriptive. Associations are not causal."
      ),
      width = 145
    ),
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(size = 17, face = "bold"),
      plot.subtitle = element_text(size = 11, color = col_grey),
      plot.caption = element_text(size = 8.2, color = "grey45", hjust = 0)
    )
  ) &
  theme(
    plot.title = element_text(size = 11.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 8.8, color = col_grey, hjust = 0),
    plot.margin = margin(8, 8, 8, 8),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8.5),
    legend.position = "bottom",
    legend.text = element_text(size = 8.5)
  )

# =============================================================================
# SUPPORTING FIGURES
# =============================================================================

p_acute_model_comparison <- acute_results |>
  mutate(
    model = factor(model, levels = names(models_acute)),
    exercise = factor(
      exercise,
      levels = rev(setdiff(levels(dat_acute_model$exercise), reference_exercise))
    )
  ) |>
  ggplot(aes(y = exercise, x = estimate_minutes, color = model)) +
  geom_linerange(
    aes(xmin = ci_low_minutes, xmax = ci_high_minutes),
    linewidth = 1.05,
    alpha = 0.75,
    position = position_dodge(width = 0.6)
  ) +
  geom_point(size = 2.2, position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, linewidth = 0.35, linetype = "dashed") +
  scale_y_discrete(labels = scales::label_wrap(18)) +
  scale_color_manual(values = model_palette, drop = FALSE) +
  scale_x_continuous(
    labels = \(x) paste0(round(x), " min"),
    breaks = scales::breaks_pretty(n = 6)
  ) +
  labs(
    title = "Model comparison for same-day exercise timing",
    subtitle = str_wrap(
      "Differences relative to no exercise; bedtime is separated because it may be part of the pathway",
      width = 105
    ),
    x = "Difference in sleep duration",
    y = NULL,
    color = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

p_over_time <- yearly_exercise_summary |>
  ggplot(aes(x = year_label, y = share, fill = exercise)) +
  geom_col(alpha = 0.92, width = 0.78) +
  geom_text(
    data = year_coverage,
    aes(x = year_label, y = 1.03, label = paste0("n=", n)),
    inherit.aes = FALSE,
    size = 3,
    color = col_dark_text
  ) +
  scale_fill_manual(values = exercise_palette, drop = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, 1.08), clip = "off") +
  labs(
    title = "Exercise timing patterns changed over time",
    subtitle = "Annual category composition; labels show diary days and * denotes a partial year",
    x = NULL,
    y = "Share of diary days",
    fill = NULL
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_history_sensitivity <- history_sensitivity_results |>
  ggplot(aes(y = label_group, x = estimate_minutes)) +
  geom_segment(
    aes(x = ci_low_minutes, xend = ci_high_minutes, yend = label_group),
    linewidth = 1.1,
    color = col_dark_blue
  ) +
  geom_point(size = 2.7, color = col_orange) +
  geom_text(
    aes(x = ci_high_minutes, label = label),
    nudge_x = 1,
    hjust = 0,
    size = 3,
    fontface = "bold",
    color = col_dark_text
  ) +
  geom_vline(xintercept = 0, linewidth = 0.35, linetype = "dashed") +
  scale_x_continuous(
    labels = \(x) paste0(round(x), " min"),
    breaks = scales::breaks_pretty(n = 6),
    expand = expansion(mult = c(0.1, 0.25))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Previous-night sleep is a sensitivity control",
    subtitle = "28-day frequency estimate per one additional exercise day/week",
    x = "Difference in sleep duration",
    y = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

# Only the complete four-panel figure is saved for the main public-facing
# output. Its individual panels are not written as separate files.
figures_to_save <- list(
  "exercise_figure1_main.png" = list(
    plot = p_main,
    width = 10,
    height = 12.5
  ),
  "exercise_figureS1_same_day_model_comparison.png" = list(
    plot = p_acute_model_comparison,
    width = 11,
    height = 6
  ),
  "exercise_figureS2_over_time.png" = list(
    plot = p_over_time,
    width = 12,
    height = 6
  ),
  "exercise_figureS3_recent_frequency_sensitivity.png" = list(
    plot = p_history_sensitivity,
    width = 10,
    height = 5.5
  )
)

purrr::iwalk(
  figures_to_save,
  \(figure_spec, filename) {
    print(figure_spec$plot)
    save_plot(
      plot = figure_spec$plot,
      filename = filename,
      width = figure_spec$width,
      height = figure_spec$height
    )
  }
)

# =============================================================================
# REPORTING SUMMARY
# =============================================================================

cat("\n========== REPORTING SUMMARY ==========\n")
cat("Research questions are documented at the top of this script.\n")
cat(
  "Same-day exercise timing and recent 7-, 14-, and 28-day exercise",
  "frequency are analyzed as separate exposures.\n"
)
cat(
  "The recent-history models use", nrow(dat_history_model),
  "days with complete 28-day exercise histories; this continuously observed",
  "subset may not represent every year equally.\n"
)
cat("Main figure saved to:", file.path(figure_dir, "exercise_figure1_main.png"), "\n")
cat("Supporting figures saved to:", figure_dir, "\n")
cat("No CSV files were created.\n")
cat(
  "Interpretation note: recent exercise frequency is not a direct measure of",
  "fitness or a causal long-term training effect. Poor sleep, fatigue, health,",
  "and changing life circumstances can also affect exercise behavior.\n"
)
