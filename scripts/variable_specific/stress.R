# =============================================================================
# stress.R
#
# Purpose: Examine how recorded evening stress is associated with bedtime,
# two distinct insomnia patterns, and sleep duration on the following night.
#
# Research questions:
#   RQ1. How often is evening stress recorded, and how has recording varied
#        across diary years?
#   RQ2. Is recorded stress associated with going to bed later?
#   RQ3. Is recorded stress associated with difficulty falling asleep?
#   RQ4. Is recorded stress associated with stress-related early waking?
#   RQ5. Is recorded stress associated with shorter same-night sleep after
#        adjustment for calendar time, previous-night sleep, and daily context?
#   RQ6. How sensitive are the associations to additional adjustment for
#        bedtime and evening brainwork as possible pathways?
#
# Input:
#   df_clean from scripts/01_load_main_data.R
#
# Outputs:
#   - descriptive and model summaries printed to the console
#   - one four-panel main figure for public-facing reporting
#   - three supporting figures
#   - figures saved to outputs/figures/variable_specific/stress/
#   - no CSV files
#
# Notes for interpretation:
#   - The codebook defines stress as a clear evening feeling of stress about
#     the following day. It does not measure every form or intensity of stress.
#   - Stress can be difficult to recognize. A zero therefore means "no stress
#     recorded", not proof that the evening was stress-free. If unrecognized
#     stress is coded as zero, contrasts between the two groups may be diluted.
#   - The diary date is the exposure day and the night that starts on that date.
#     Same-date sleep and insomnia therefore follow that evening's stress entry.
#   - Bedtime records when sleep was attempted, not how long sleep onset took.
#     An unchanged bedtime can therefore coexist with difficulty falling asleep.
#   - Insomnia code 2 is explicitly described as stress-related early waking.
#     Its association with recorded stress is partly conceptually overlapping
#     and should not be presented as independent confirmation of causality.
#   - Previous-night sleep may influence both next-day stress and the following
#     night's sleep, so it is included in the context-adjusted models.
#   - Bedtime and evening brainwork occur after, or alongside, evening stress
#     and may lie on a pathway to sleep. They are added only in sensitivity
#     models, not in the primary context-adjusted interpretation.
#   - All results are associations, not causal effects.
# =============================================================================

library(tidyverse)
library(fixest)
library(here)
library(patchwork)

source(here("scripts", "01_load_main_data.R"))

if (!exists("df_clean")) {
  stop("df_clean not found. Run 01_load_main_data.R first.")
}

required_columns <- c(
  "date", "duration", "stress_num", "stress", "bedtime_code", "bedtime",
  "insomnia_num", "coffee", "health", "exercise", "day_of_week",
  "brainwork_any"
)

missing_columns <- setdiff(required_columns, names(df_clean))

if (length(missing_columns) > 0) {
  stop(
    "Missing columns required for the stress analysis: ",
    paste(missing_columns, collapse = ", ")
  )
}

variable_name <- "stress"
figure_dir <- here("outputs", "figures", "variable_specific", variable_name)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SETTINGS AND HELPERS
# =============================================================================

col_navy       <- "#002d5a"
col_dark_blue  <- "#2f4a73"
col_steel      <- "#4a7ba7"
col_light_blue <- "#a3c1d9"
col_pale_blue  <- "#d0e1ef"
col_orange     <- "#CC5500"
col_dark_text  <- "#2a2a2a"
col_grey       <- "grey40"

stress_palette <- c(
  "No recorded stress" = col_light_blue,
  "Recorded stress" = col_orange
)

bedtime_palette <- c(
  "Before 23:00" = col_navy,
  "23:00-00:00" = col_dark_blue,
  "After 00:00" = col_steel
)

insomnia_palette <- c(
  "Difficulty falling asleep" = col_orange,
  "Stress-related early waking" = col_dark_blue
)

model_palette <- c(
  "Raw" = col_pale_blue,
  "Calendar adjusted" = col_steel,
  "Context adjusted" = col_dark_blue,
  "Plus pathway sensitivity" = col_orange
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

safe_feglm <- function(fml, data, model_name) {
  model_data <- prepare_nw_data(data, fml)

  tryCatch(
    feglm(
      fml = fml,
      data = model_data,
      family = binomial(link = "logit"),
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

fmt_or <- function(x) {
  paste0("OR ", scales::number(x, accuracy = 0.1))
}

extract_duration_result <- function(model, model_name) {
  if (is.null(model) || !"stress_num" %in% names(coef(model))) {
    return(tibble())
  }

  estimate <- unname(coef(model)[["stress_num"]])
  std_error <- unname(se(model)[["stress_num"]])

  tibble(
    model = model_name,
    n = nobs(model),
    estimate_minutes = estimate * 60,
    ci_low_minutes = (estimate - 1.96 * std_error) * 60,
    ci_high_minutes = (estimate + 1.96 * std_error) * 60,
    label = fmt_min(estimate_minutes)
  )
}

extract_odds_ratio <- function(model, model_name, outcome_name) {
  if (is.null(model) || !"stress_num" %in% names(coef(model))) {
    return(tibble())
  }

  estimate <- unname(coef(model)[["stress_num"]])
  std_error <- unname(se(model)[["stress_num"]])

  tibble(
    outcome = outcome_name,
    model = model_name,
    n = nobs(model),
    odds_ratio = exp(estimate),
    ci_low = exp(estimate - 1.96 * std_error),
    ci_high = exp(estimate + 1.96 * std_error),
    label = fmt_or(odds_ratio)
  )
}

# =============================================================================
# ANALYSIS DATA
# =============================================================================

dat_stress <- df_clean |>
  arrange(date) |>
  mutate(
    year_num = as.integer(format(date, "%Y")),
    month_num = as.integer(format(date, "%m")),
    year_month = factor(format(date, "%Y-%m")),
    stress_num = case_when(
      stress_num == 0 ~ 0L,
      stress_num == 1 ~ 1L,
      TRUE ~ NA_integer_
    ),
    stress_status = factor(
      stress_num,
      levels = 0:1,
      labels = c("No recorded stress", "Recorded stress")
    ),
    insomnia_onset = case_when(
      is.na(insomnia_num) ~ NA_integer_,
      insomnia_num == 1 ~ 1L,
      TRUE ~ 0L
    ),
    insomnia_early_waking = case_when(
      is.na(insomnia_num) ~ NA_integer_,
      insomnia_num == 2 ~ 1L,
      TRUE ~ 0L
    ),
    late_bedtime = case_when(
      is.na(bedtime_code) ~ NA_integer_,
      bedtime_code == 0 ~ 0L,
      bedtime_code %in% 1:2 ~ 1L,
      TRUE ~ NA_integer_
    ),
    brainwork_num = case_when(
      is.na(brainwork_any) ~ NA_integer_,
      as.character(brainwork_any) == "No" ~ 0L,
      as.character(brainwork_any) == "Yes" ~ 1L,
      TRUE ~ NA_integer_
    ),
    prev_duration = lag_by_calendar_days(duration, date, 1),
    bedtime = factor(bedtime, levels = levels(bedtime), ordered = FALSE),
    coffee = factor(coffee, levels = levels(coffee), ordered = FALSE),
    health = factor(health, levels = levels(health), ordered = FALSE),
    exercise = factor(exercise, levels = levels(exercise), ordered = FALSE),
    day_of_week = factor(
      day_of_week,
      levels = levels(day_of_week),
      ordered = FALSE
    )
  )

dat_analysis <- dat_stress |>
  filter(!is.na(stress_num), !is.na(duration))

cat("\n========== STRESS ANALYSIS SAMPLE ==========\n")
cat("Observations:", nrow(dat_analysis), "\n")
cat("Recorded-stress nights:", sum(dat_analysis$stress_num), "\n")
cat("No-recorded-stress nights:", sum(dat_analysis$stress_num == 0), "\n")
cat(
  "Date range:", format(min(dat_analysis$date), "%Y-%m-%d"), "to",
  format(max(dat_analysis$date), "%Y-%m-%d"), "\n"
)

# =============================================================================
# DESCRIPTIVE SUMMARIES
# =============================================================================

year_summary <- dat_analysis |>
  group_by(year_num) |>
  summarise(
    n = n(),
    stress_n = sum(stress_num),
    stress_rate = mean(stress_num),
    observed_months = n_distinct(month_num),
    .groups = "drop"
  ) |>
  mutate(
    full_calendar_year = observed_months == 12,
    year_label = paste0(year_num, if_else(full_calendar_year, "", "*")),
    year_label = factor(year_label, levels = year_label),
    rate_label = paste0(
      fmt_pct(stress_rate, accuracy = 0.1),
      "\n(n=", stress_n, ")"
    )
  )

bedtime_summary <- dat_analysis |>
  drop_na(bedtime) |>
  count(stress_status, bedtime, name = "n") |>
  group_by(stress_status) |>
  mutate(share = n / sum(n)) |>
  ungroup()

duration_summary <- dat_analysis |>
  group_by(stress_status) |>
  summarise(
    n = n(),
    mean_sleep = mean(duration),
    median_sleep = median(duration),
    .groups = "drop"
  ) |>
  mutate(median_label = paste0("Median: ", round(median_sleep, 1), " h"))

insomnia_summary <- dat_analysis |>
  drop_na(insomnia_onset, insomnia_early_waking) |>
  group_by(stress_status) |>
  summarise(
    n = n(),
    `Difficulty falling asleep` = mean(insomnia_onset),
    `Stress-related early waking` = mean(insomnia_early_waking),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(`Difficulty falling asleep`, `Stress-related early waking`),
    names_to = "insomnia_type",
    values_to = "rate"
  ) |>
  mutate(
    insomnia_type = factor(
      insomnia_type,
      levels = c(
        "Difficulty falling asleep",
        "Stress-related early waking"
      )
    ),
    label = fmt_pct(rate, accuracy = 1)
  )

cat("\n========== RECORDED STRESS BY YEAR ==========\n")
print(year_summary, n = Inf, width = Inf)
cat("\n========== BEDTIME COMPOSITION BY STRESS ==========\n")
print(bedtime_summary, n = Inf, width = Inf)
cat("\n========== INSOMNIA-TYPE RATES BY STRESS ==========\n")
print(insomnia_summary, n = Inf, width = Inf)
cat("\n========== SLEEP DURATION BY STRESS ==========\n")
print(duration_summary, width = Inf)

# =============================================================================
# COMMON MODEL SAMPLE
# =============================================================================

# All models use one complete-case sample so coefficient changes reflect the
# adjustment set rather than changes in which diary nights are included.
dat_model <- dat_stress |>
  drop_na(
    duration,
    stress_num,
    insomnia_onset,
    insomnia_early_waking,
    late_bedtime,
    prev_duration,
    bedtime,
    brainwork_num,
    coffee,
    health,
    exercise,
    day_of_week,
    year_month
  ) |>
  mutate(
    bedtime = fct_drop(bedtime),
    coffee = fct_drop(coffee),
    health = fct_drop(health),
    exercise = fct_drop(exercise),
    day_of_week = fct_drop(day_of_week),
    year_month = fct_drop(year_month)
  )

cat("\n========== STRESS MODELLING SAMPLE ==========\n")
cat("Observations:", nrow(dat_model), "\n")
cat("Recorded-stress nights:", sum(dat_model$stress_num), "\n")

# =============================================================================
# SLEEP-DURATION MODELS
# =============================================================================

models_duration <- list(
  "Raw" = safe_feols(
    duration ~ stress_num,
    data = dat_model,
    model_name = "Sleep duration: raw"
  ),
  "Calendar adjusted" = safe_feols(
    duration ~ stress_num + day_of_week | year_month,
    data = dat_model,
    model_name = "Sleep duration: calendar adjusted"
  ),
  "Context adjusted" = safe_feols(
    duration ~ stress_num + prev_duration + health + exercise + coffee +
      day_of_week | year_month,
    data = dat_model,
    model_name = "Sleep duration: context adjusted"
  ),
  "Plus pathway sensitivity" = safe_feols(
    duration ~ stress_num + prev_duration + health + exercise + coffee +
      day_of_week + bedtime + brainwork_num | year_month,
    data = dat_model,
    model_name = "Sleep duration: plus pathway sensitivity"
  )
) |>
  purrr::compact()

duration_results <- purrr::imap_dfr(
  models_duration,
  extract_duration_result
) |>
  mutate(model = factor(model, levels = names(models_duration)))

# =============================================================================
# INSOMNIA-TYPE MODELS
# =============================================================================

models_insomnia <- list(
  "Difficulty falling asleep" = list(
    "Raw" = safe_feglm(
      insomnia_onset ~ stress_num,
      data = dat_model,
      model_name = "Difficulty falling asleep: raw"
    ),
    "Calendar adjusted" = safe_feglm(
      insomnia_onset ~ stress_num + day_of_week | year_month,
      data = dat_model,
      model_name = "Difficulty falling asleep: calendar adjusted"
    ),
    "Context adjusted" = safe_feglm(
      insomnia_onset ~ stress_num + prev_duration + health + exercise +
        coffee + day_of_week | year_month,
      data = dat_model,
      model_name = "Difficulty falling asleep: context adjusted"
    ),
    "Plus pathway sensitivity" = safe_feglm(
      insomnia_onset ~ stress_num + prev_duration + health + exercise +
        coffee + day_of_week + bedtime + brainwork_num | year_month,
      data = dat_model,
      model_name = "Difficulty falling asleep: plus pathway sensitivity"
    )
  ) |>
    purrr::compact(),
  "Stress-related early waking" = list(
    "Raw" = safe_feglm(
      insomnia_early_waking ~ stress_num,
      data = dat_model,
      model_name = "Stress-related early waking: raw"
    ),
    "Calendar adjusted" = safe_feglm(
      insomnia_early_waking ~ stress_num + day_of_week | year_month,
      data = dat_model,
      model_name = "Stress-related early waking: calendar adjusted"
    ),
    "Context adjusted" = safe_feglm(
      insomnia_early_waking ~ stress_num + prev_duration + health + exercise +
        coffee + day_of_week | year_month,
      data = dat_model,
      model_name = "Stress-related early waking: context adjusted"
    ),
    "Plus pathway sensitivity" = safe_feglm(
      insomnia_early_waking ~ stress_num + prev_duration + health + exercise +
        coffee + day_of_week + bedtime + brainwork_num | year_month,
      data = dat_model,
      model_name = "Stress-related early waking: plus pathway sensitivity"
    )
  ) |>
    purrr::compact()
) |>
  purrr::keep(\(models) length(models) > 0)

insomnia_results <- models_insomnia |>
  purrr::imap_dfr(
    \(models, outcome_name) {
      purrr::imap_dfr(
        models,
        \(model, model_name) {
          extract_odds_ratio(model, model_name, outcome_name)
        }
      )
    }
  ) |>
  mutate(
    outcome = factor(
      outcome,
      levels = c(
        "Difficulty falling asleep",
        "Stress-related early waking"
      )
    ),
    model = factor(model, levels = names(model_palette))
  )

# =============================================================================
# BEDTIME MODELS
# =============================================================================

models_bedtime <- list(
  "Raw" = safe_feglm(
    late_bedtime ~ stress_num,
    data = dat_model,
    model_name = "Bedtime at or after 23:00: raw"
  ),
  "Calendar adjusted" = safe_feglm(
    late_bedtime ~ stress_num + day_of_week | year_month,
    data = dat_model,
    model_name = "Bedtime at or after 23:00: calendar adjusted"
  ),
  "Context adjusted" = safe_feglm(
    late_bedtime ~ stress_num + prev_duration + health + exercise + coffee +
      day_of_week | year_month,
    data = dat_model,
    model_name = "Bedtime at or after 23:00: context adjusted"
  ),
  "Plus pathway sensitivity" = safe_feglm(
    late_bedtime ~ stress_num + prev_duration + health + exercise + coffee +
      day_of_week + brainwork_num | year_month,
    data = dat_model,
    model_name = "Bedtime at or after 23:00: plus brainwork sensitivity"
  )
) |>
  purrr::compact()

bedtime_results <- purrr::imap_dfr(
  models_bedtime,
  \(model, model_name) {
    extract_odds_ratio(
      model,
      model_name,
      "Bedtime at or after 23:00"
    )
  }
) |>
  mutate(model = factor(model, levels = names(models_bedtime)))

cat("\n========== STRESS MODEL SUMMARIES ==========\n")
purrr::iwalk(
  models_duration,
  \(model, model_name) {
    cat("\n--- Sleep duration: ", model_name, " ---\n", sep = "")
    print(summary(model))
  }
)

purrr::iwalk(
  models_insomnia,
  \(models, outcome_name) {
    purrr::iwalk(
      models,
      \(model, model_name) {
        cat("\n--- ", outcome_name, ": ", model_name, " ---\n", sep = "")
        print(summary(model))
      }
    )
  }
)

purrr::iwalk(
  models_bedtime,
  \(model, model_name) {
    cat("\n--- Bedtime at or after 23:00: ", model_name, " ---\n", sep = "")
    print(summary(model))
  }
)

cat("\n========== STRESS ESTIMATES FOR REPORTING ==========\n")
print(duration_results, n = Inf, width = Inf)
print(insomnia_results, n = Inf, width = Inf)
print(bedtime_results, n = Inf, width = Inf)

# =============================================================================
# FOUR-PANEL MAIN FIGURE
# =============================================================================

p_prevalence <- year_summary |>
  ggplot(aes(x = year_label, y = stress_rate)) +
  geom_col(width = 0.72, fill = col_navy, alpha = 0.9) +
  geom_text(
    aes(label = rate_label),
    vjust = -0.25,
    size = 2.6,
    lineheight = 0.9,
    color = col_dark_text,
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.24))
  ) +
  labs(
    title = "Recorded stress was uncommon",
    subtitle = "Under-recognition means these rates may be too low",
    x = NULL,
    y = "Nights with recorded stress"
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_bedtime <- bedtime_summary |>
  ggplot(aes(x = stress_status, y = share, fill = bedtime)) +
  geom_col(width = 0.68, alpha = 0.94) +
  geom_text(
    aes(label = if_else(share >= 0.08, fmt_pct(share), "")),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(values = bedtime_palette, drop = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Recorded bedtime changed little with stress",
    subtitle = "Bedtime is not the same as the time required to fall asleep",
    x = NULL,
    y = "Share of nights",
    fill = NULL
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(size = 8.5))

p_insomnia_rates <- insomnia_summary |>
  ggplot(
    aes(
      x = stress_status,
      y = rate,
      color = insomnia_type,
      group = insomnia_type
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    aes(label = label),
    nudge_y = 0.012,
    size = 3,
    color = col_dark_text,
    fontface = "bold"
  ) +
  scale_color_manual(values = insomnia_palette, drop = FALSE) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.18))
  ) +
  labs(
    title = "Both insomnia patterns were more common with stress",
    subtitle = "Observed shares; early waking partly overlaps the stress definition",
    x = NULL,
    y = "Share of nights",
    color = NULL
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(size = 8.5))

primary_insomnia_results <- insomnia_results |>
  filter(model == "Context adjusted")

if (nrow(primary_insomnia_results) == 0) {
  stop("Context-adjusted insomnia models were not estimated successfully.")
}

p_insomnia_adjusted <- primary_insomnia_results |>
  ggplot(
    aes(
      y = outcome,
      x = odds_ratio,
      xmin = ci_low,
      xmax = ci_high,
      color = outcome
    )
  ) +
  geom_vline(xintercept = 1, linewidth = 0.35, linetype = "dashed") +
  geom_pointrange(linewidth = 0.9) +
  geom_text(
    aes(x = ci_high, label = label),
    hjust = -0.15,
    size = 3,
    color = col_dark_text,
    fontface = "bold"
  ) +
  scale_x_log10(
    labels = scales::number_format(accuracy = 0.1),
    breaks = c(0.5, 1, 2, 4, 8, 16),
    expand = expansion(mult = c(0.08, 0.28))
  ) +
  scale_color_manual(values = insomnia_palette, guide = "none") +
  coord_cartesian(clip = "off") +
  labs(
    title = "The insomnia associations persist after adjustment",
    subtitle = "Values above 1 mean higher odds on recorded-stress nights",
    x = "Adjusted odds ratio, log scale",
    y = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

partial_years <- year_summary |>
  filter(!full_calendar_year) |>
  pull(year_num)

partial_year_note <- if (length(partial_years) > 0) {
  paste0("Partial years in panel A: ", paste(partial_years, collapse = ", "), ".")
} else {
  "All years in panel A include all 12 months."
}

p_main <- (p_prevalence + p_bedtime) /
  (p_insomnia_rates + p_insomnia_adjusted) +
  plot_layout(guides = "collect", widths = c(1, 1), heights = c(1, 1)) +
  plot_annotation(
    title = str_wrap(
      "Recorded stress coincided with both difficulty falling asleep and early waking",
      width = 76
    ),
    subtitle = str_wrap(
      "The bedtime itself changed little, suggesting that going to bed and falling asleep should be interpreted separately",
      width = 110
    ),
    caption = str_wrap(
      paste(
        "Stress = a clear recorded evening feeling of stress about the following day; unrecognized stress may be coded as zero.",
        "Panel D adjusts for exact previous-night sleep, health, exercise, coffee, weekday, and year-month.",
        "Bedtime and evening brainwork are reserved for pathway sensitivity models.",
        "95% CIs use a 7-day Newey-West estimator.",
        partial_year_note,
        "Associations are not causal."
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

duration_label_y <- quantile(dat_analysis$duration, 0.97, na.rm = TRUE)

p_duration_distribution <- dat_analysis |>
  ggplot(aes(x = stress_status, y = duration, fill = stress_status)) +
  geom_boxplot(
    width = 0.6,
    alpha = 0.78,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.07,
    size = 0.8,
    color = col_dark_text
  ) +
  geom_label(
    data = duration_summary,
    aes(x = stress_status, y = duration_label_y, label = median_label),
    inherit.aes = FALSE,
    size = 2.8,
    linewidth = 0.15,
    fill = "white",
    color = col_dark_text
  ) +
  scale_fill_manual(values = stress_palette, guide = "none") +
  coord_cartesian(ylim = c(0, NA)) +
  labs(
    title = "Recorded-stress nights had less sleep",
    subtitle = "Raw distributions; medians are labelled",
    x = NULL,
    y = "Sleep duration (hours)"
  ) +
  theme_sleep()

p_duration_models <- duration_results |>
  ggplot(
    aes(
      y = model,
      x = estimate_minutes,
      color = model
    )
  ) +
  geom_segment(
    aes(x = ci_low_minutes, xend = ci_high_minutes, yend = model),
    linewidth = 1.1,
    alpha = 0.85
  ) +
  geom_point(size = 2.8) +
  geom_text(
    aes(x = ci_high_minutes, label = label),
    nudge_x = 2,
    hjust = 0,
    size = 3,
    color = col_dark_text,
    fontface = "bold"
  ) +
  geom_vline(xintercept = 0, linewidth = 0.35, linetype = "dashed") +
  scale_color_manual(values = model_palette, guide = "none") +
  scale_x_continuous(
    labels = \(x) paste0(round(x), " min"),
    breaks = scales::breaks_pretty(n = 5),
    expand = expansion(mult = c(0.1, 0.28))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Stress remained associated with shorter sleep after adjustment",
    subtitle = "Below zero = shorter sleep on recorded-stress nights",
    x = "Difference in sleep duration",
    y = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

p_duration_support <- p_duration_distribution + p_duration_models +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title = "Sleep duration on nights with and without recorded stress",
    caption = str_wrap(
      "The context-adjusted model controls for exact previous-night sleep, health, exercise, coffee, weekday, and year-month. The final sensitivity model also includes bedtime and evening brainwork.",
      width = 125
    )
  )

p_insomnia_models <- insomnia_results |>
  ggplot(
    aes(
      y = model,
      x = odds_ratio,
      xmin = ci_low,
      xmax = ci_high,
      color = model
    )
  ) +
  geom_vline(xintercept = 1, linewidth = 0.35, linetype = "dashed") +
  geom_pointrange(linewidth = 0.75) +
  scale_x_log10(
    labels = scales::number_format(accuracy = 0.1),
    breaks = c(0.5, 1, 2, 4, 8, 16)
  ) +
  scale_color_manual(values = model_palette, guide = "none") +
  facet_wrap(vars(outcome), ncol = 1, scales = "free_y") +
  labs(
    title = "Insomnia associations across model layers",
    subtitle = "Odds ratios compare recorded stress with no recorded stress",
    caption = str_wrap(
      "Stress-related early waking partly overlaps conceptually with the stress exposure. The pathway sensitivity additionally adjusts for bedtime and evening brainwork.",
      width = 110
    ),
    x = "Odds ratio, log scale",
    y = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

p_bedtime_models <- bedtime_results |>
  ggplot(
    aes(
      y = model,
      x = odds_ratio,
      xmin = ci_low,
      xmax = ci_high,
      color = model
    )
  ) +
  geom_vline(xintercept = 1, linewidth = 0.35, linetype = "dashed") +
  geom_pointrange(linewidth = 0.8) +
  geom_text(
    aes(x = ci_high, label = label),
    hjust = -0.15,
    size = 3,
    color = col_dark_text,
    fontface = "bold"
  ) +
  scale_x_log10(
    labels = scales::number_format(accuracy = 0.1),
    breaks = c(0.25, 0.5, 1, 2, 4),
    expand = expansion(mult = c(0.08, 0.28))
  ) +
  scale_color_manual(values = model_palette, guide = "none") +
  coord_cartesian(clip = "off") +
  labs(
    title = "Recorded stress was not primarily a later-bedtime pattern",
    subtitle = "Outcome: bedtime at or after 23:00; values above 1 mean higher odds",
    caption = str_wrap(
      "This analysis concerns the recorded bedtime, not sleep-onset latency. The final sensitivity additionally adjusts for evening brainwork.",
      width = 105
    ),
    x = "Odds ratio, log scale",
    y = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

# Individual panels from the four-panel main figure are not saved separately.
figures_to_save <- list(
  "stress_figure1_main.png" = list(
    plot = p_main,
    width = 10,
    height = 12.5
  ),
  "stress_figureS1_sleep_duration.png" = list(
    plot = p_duration_support,
    width = 13,
    height = 6.5
  ),
  "stress_figureS2_insomnia_model_comparison.png" = list(
    plot = p_insomnia_models,
    width = 10,
    height = 8
  ),
  "stress_figureS3_bedtime_model_comparison.png" = list(
    plot = p_bedtime_models,
    width = 10,
    height = 6
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

cat("\n========== STRESS REPORTING SUMMARY ==========\n")
cat("Research questions and hypotheses are documented at the top of this script.\n")
cat(
  "Measurement note: zero means no stress was recorded; difficulty recognizing",
  "stress may cause some stressed evenings to be classified as zero.\n"
)
cat(
  "Primary insomnia and duration interpretations use context-adjusted models.",
  "Bedtime and evening brainwork are pathway sensitivities.\n"
)
cat("Main figure saved to:", file.path(figure_dir, "stress_figure1_main.png"), "\n")
cat("Supporting figures saved to:", figure_dir, "\n")
cat("No CSV files were created.\n")
cat(
  "Interpretation note: the association with stress-related early waking partly",
  "reflects overlapping self-reported concepts and is not independent causal",
  "confirmation.\n"
)
