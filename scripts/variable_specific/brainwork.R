# =============================================================================
# brainwork.R
#
# Purpose: Examine whether demanding thinking after 19:00 is associated with
# bedtime, sleep duration, and insomnia on the night that follows.
#
# Research questions:
#   RQ1. How common is evening brainwork, and how has its prevalence varied
#        across diary years?
#   RQ2. Is evening brainwork associated with a later bedtime?
#   RQ3. Is evening brainwork associated with same-night sleep duration?
#   RQ4. Does the sleep-duration association remain after adjustment for
#        calendar time, previous-night sleep, and recorded daily context?
#   RQ5. How much does additional adjustment for bedtime change the estimate,
#        given that bedtime may be part of the brainwork-to-sleep pathway?
#   RQ6. Do observed insomnia-type rates differ between nights with and without
#        evening brainwork?
#
# Input:
#   df_clean from scripts/01_load_main_data.R
#
# Outputs:
#   - coding checks, descriptive summaries, and model results in the console
#   - one four-panel main figure for public-facing reporting
#   - two supporting figures
#   - figures saved to outputs/figures/variable_specific/brainwork/
#   - no CSV files
#
# Coding:
#   - The codebook defines aivotyo = 0 as no demanding thinking after 19:00
#     and aivotyo = 1 as demanding thinking after 19:00.
#   - The raw column also contains a few positive non-integer values and one
#     value above 1. Following the analysis rule, every positive value is coded
#     as brainwork and zero is coded as no brainwork. Missing values remain
#     missing. The raw aivotyo values are never overwritten.
#   - This script verifies that its binary coding agrees with brainwork_any,
#     which is created centrally in scripts/01_load_main_data.R.
#
# Notes for interpretation:
#   - Evening brainwork is rare, so year-specific effect models and elaborate
#     interactions would be unstable. Annual prevalence is descriptive only;
#     the primary sleep association is pooled across years.
#   - The diary date is the exposure day and the night that starts on that
#     date. Same-date sleep therefore follows that evening's brainwork.
#   - Previous-night sleep may influence both the ability or need to do
#     demanding work and the following night's sleep, so it is included in the
#     primary context-adjusted model.
#   - Stress can lead to both evening work and poorer sleep. Recorded stress is
#     adjusted for, but unmeasured workload, deadlines, and perceived fatigue
#     may still confound the association.
#   - Bedtime occurs after evening brainwork and may be a mechanism rather than
#     a conventional confounder. It is therefore added only in a sensitivity
#     model and shown separately in the main figure.
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

required_columns <- c(
  "date", "duration", "aivotyo", "brainwork_any", "bedtime",
  "insomnia_num", "stress", "health", "exercise", "coffee",
  "day_of_week"
)

missing_columns <- setdiff(required_columns, names(df_clean))

if (length(missing_columns) > 0) {
  stop(
    "Missing columns required for the brainwork analysis: ",
    paste(missing_columns, collapse = ", ")
  )
}

variable_name <- "brainwork"
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

brainwork_palette <- c(
  "No evening brainwork" = col_light_blue,
  "Evening brainwork" = col_orange
)

bedtime_palette <- c(
  "Before 23:00" = col_navy,
  "23:00-00:00" = col_dark_blue,
  "After 00:00" = col_steel
)

model_palette <- c(
  "Raw" = col_pale_blue,
  "Calendar adjusted" = col_steel,
  "Context adjusted" = col_dark_blue,
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

extract_brainwork_result <- function(model, model_name) {
  if (is.null(model) || !"brainwork_num" %in% names(coef(model))) {
    return(tibble())
  }

  estimate <- unname(coef(model)[["brainwork_num"]])
  std_error <- unname(se(model)[["brainwork_num"]])

  tibble(
    model = model_name,
    n = nobs(model),
    estimate_minutes = estimate * 60,
    ci_low_minutes = (estimate - 1.96 * std_error) * 60,
    ci_high_minutes = (estimate + 1.96 * std_error) * 60,
    label = fmt_min(estimate_minutes)
  )
}

# =============================================================================
# ANALYSIS DATA AND CODING CHECKS
# =============================================================================

dat_brainwork <- df_clean |>
  arrange(date) |>
  mutate(
    year_num = as.integer(format(date, "%Y")),
    month_num = as.integer(format(date, "%m")),
    year_month = factor(format(date, "%Y-%m")),
    brainwork_num = case_when(
      is.na(aivotyo) ~ NA_integer_,
      aivotyo == 0 ~ 0L,
      aivotyo > 0 ~ 1L,
      TRUE ~ NA_integer_
    ),
    brainwork = factor(
      brainwork_num,
      levels = 0:1,
      labels = c("No evening brainwork", "Evening brainwork")
    ),
    loader_brainwork_num = case_when(
      is.na(brainwork_any) ~ NA_integer_,
      as.character(brainwork_any) == "No" ~ 0L,
      as.character(brainwork_any) == "Yes" ~ 1L,
      TRUE ~ NA_integer_
    ),
    prev_duration = lag_by_calendar_days(duration, date, 1),
    bedtime = factor(bedtime, levels = levels(bedtime), ordered = FALSE),
    stress = factor(stress, levels = levels(stress), ordered = FALSE),
    health = factor(health, levels = levels(health), ordered = FALSE),
    exercise = factor(exercise, levels = levels(exercise), ordered = FALSE),
    coffee = factor(coffee, levels = levels(coffee), ordered = FALSE),
    day_of_week = factor(
      day_of_week,
      levels = levels(day_of_week),
      ordered = FALSE
    )
  )

coding_mismatches <- dat_brainwork |>
  filter(
    xor(is.na(brainwork_num), is.na(loader_brainwork_num)) |
      (!is.na(brainwork_num) & brainwork_num != loader_brainwork_num)
  )

if (nrow(coding_mismatches) > 0) {
  stop(
    "The script's aivotyo > 0 coding does not agree with brainwork_any. ",
    "Review scripts/01_load_main_data.R before continuing."
  )
}

raw_coding_summary <- dat_brainwork |>
  summarise(
    observations = n(),
    raw_missing = sum(is.na(aivotyo)),
    raw_zero = sum(aivotyo == 0, na.rm = TRUE),
    raw_positive = sum(aivotyo > 0, na.rm = TRUE),
    positive_non_integer = sum(
      aivotyo > 0 & aivotyo != floor(aivotyo),
      na.rm = TRUE
    ),
    positive_above_one = sum(aivotyo > 1, na.rm = TRUE)
  )

dat_analysis <- dat_brainwork |>
  filter(!is.na(brainwork_num), !is.na(duration))

cat("\n========== BRAINWORK CODING CHECK ==========\n")
print(raw_coding_summary, width = Inf)
cat("Binary coding agrees with brainwork_any for all observations.\n")
cat("Analysis sample:", nrow(dat_analysis), "nights\n")
cat("Evening-brainwork nights:", sum(dat_analysis$brainwork_num), "\n")

# =============================================================================
# DESCRIPTIVE SUMMARIES
# =============================================================================

year_summary <- dat_analysis |>
  group_by(year_num) |>
  summarise(
    n = n(),
    brainwork_n = sum(brainwork_num),
    brainwork_rate = mean(brainwork_num),
    observed_months = n_distinct(month_num),
    .groups = "drop"
  ) |>
  mutate(
    full_calendar_year = observed_months == 12,
    year_label = paste0(year_num, if_else(full_calendar_year, "", "*")),
    year_label = factor(year_label, levels = year_label),
    rate_label = paste0(
      fmt_pct(brainwork_rate, accuracy = 0.1),
      "\n(n=", brainwork_n, ")"
    )
  )

bedtime_summary <- dat_analysis |>
  drop_na(bedtime) |>
  count(brainwork, bedtime, name = "n") |>
  group_by(brainwork) |>
  mutate(share = n / sum(n)) |>
  ungroup()

duration_summary <- dat_analysis |>
  group_by(brainwork) |>
  summarise(
    n = n(),
    mean_sleep = mean(duration),
    median_sleep = median(duration),
    .groups = "drop"
  ) |>
  mutate(median_label = paste0("Median: ", round(median_sleep, 1), " h"))

insomnia_summary <- dat_analysis |>
  drop_na(insomnia_num) |>
  group_by(brainwork) |>
  summarise(
    n = n(),
    `Difficulty falling asleep` = mean(insomnia_num == 1),
    `Stress-related early waking` = mean(insomnia_num == 2),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(`Difficulty falling asleep`, `Stress-related early waking`),
    names_to = "insomnia_type",
    values_to = "rate"
  ) |>
  mutate(label = fmt_pct(rate, accuracy = 1))

stress_context_summary <- dat_analysis |>
  drop_na(stress) |>
  group_by(stress) |>
  summarise(
    n = n(),
    brainwork_n = sum(brainwork_num),
    brainwork_rate = mean(brainwork_num),
    .groups = "drop"
  ) |>
  mutate(
    stress_label = recode(
      as.character(stress),
      "No" = "No recorded stress",
      "Yes" = "Recorded stress"
    ),
    label = paste0(
      fmt_pct(brainwork_rate, accuracy = 0.1),
      "\n(n=", brainwork_n, ")"
    )
  )

cat("\n========== BRAINWORK BY YEAR ==========\n")
print(year_summary, n = Inf, width = Inf)
cat("\n========== SLEEP DURATION BY BRAINWORK ==========\n")
print(duration_summary, width = Inf)
cat("\n========== BEDTIME COMPOSITION BY BRAINWORK ==========\n")
print(bedtime_summary, n = Inf, width = Inf)
cat("\n========== INSOMNIA-TYPE RATES BY BRAINWORK ==========\n")
print(insomnia_summary, n = Inf, width = Inf)

# =============================================================================
# SLEEP-DURATION MODELS
# =============================================================================

# All models use one complete-case sample. This makes coefficient changes
# interpretable as changes in adjustment rather than changes in included nights.
dat_model <- dat_brainwork |>
  drop_na(
    duration,
    brainwork_num,
    prev_duration,
    bedtime,
    stress,
    health,
    exercise,
    coffee,
    day_of_week,
    year_month
  ) |>
  mutate(
    bedtime = fct_drop(bedtime),
    stress = fct_drop(stress),
    health = fct_drop(health),
    exercise = fct_drop(exercise),
    coffee = fct_drop(coffee),
    day_of_week = fct_drop(day_of_week),
    year_month = fct_drop(year_month)
  )

brainwork_model_count <- sum(dat_model$brainwork_num)

if (brainwork_model_count < 20) {
  warning(
    "Fewer than 20 brainwork nights remain in the common model sample; ",
    "adjusted estimates may be very unstable."
  )
}

models_duration <- list(
  "Raw" = safe_feols(
    duration ~ brainwork_num,
    data = dat_model,
    model_name = "Raw"
  ),
  "Calendar adjusted" = safe_feols(
    duration ~ brainwork_num + day_of_week | year_month,
    data = dat_model,
    model_name = "Calendar adjusted"
  ),
  "Context adjusted" = safe_feols(
    duration ~ brainwork_num + prev_duration + stress + health +
      exercise + coffee + day_of_week | year_month,
    data = dat_model,
    model_name = "Context adjusted"
  ),
  "Plus bedtime sensitivity" = safe_feols(
    duration ~ brainwork_num + prev_duration + stress + health +
      exercise + coffee + day_of_week + bedtime | year_month,
    data = dat_model,
    model_name = "Plus bedtime sensitivity"
  )
) |>
  purrr::compact()

duration_results <- purrr::imap_dfr(
  models_duration,
  extract_brainwork_result
) |>
  mutate(model = factor(model, levels = names(models_duration)))

cat("\n========== BRAINWORK SLEEP-DURATION MODELS ==========\n")
purrr::iwalk(
  models_duration,
  \(model, model_name) {
    cat("\n--- ", model_name, " ---\n", sep = "")
    print(summary(model))
  }
)

cat("\n========== BRAINWORK ESTIMATES FOR REPORTING ==========\n")
print(duration_results, n = Inf, width = Inf)

# =============================================================================
# FOUR-PANEL MAIN FIGURE
# =============================================================================

p_prevalence <- year_summary |>
  ggplot(aes(x = year_label, y = brainwork_rate)) +
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
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "Evening brainwork was rare and episodic",
    subtitle = "Share of recorded nights; labels show brainwork-night counts",
    x = NULL,
    y = "Nights with brainwork"
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_bedtime <- bedtime_summary |>
  ggplot(aes(x = brainwork, y = share, fill = bedtime)) +
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
    title = "Brainwork nights tended to start later",
    subtitle = "Descriptive bedtime composition; bedtime may be a pathway",
    x = NULL,
    y = "Share of nights",
    fill = NULL
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(size = 8.5))

duration_label_y <- quantile(dat_analysis$duration, 0.97, na.rm = TRUE)

p_duration <- dat_analysis |>
  ggplot(aes(x = brainwork, y = duration, fill = brainwork)) +
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
    aes(x = brainwork, y = duration_label_y, label = median_label),
    inherit.aes = FALSE,
    size = 2.8,
    linewidth = 0.15,
    fill = "white",
    color = col_dark_text
  ) +
  scale_fill_manual(values = brainwork_palette, guide = "none") +
  coord_cartesian(ylim = c(0, NA)) +
  labs(
    title = "Brainwork nights had less sleep",
    subtitle = "Raw distributions; workload and other context are unadjusted",
    x = NULL,
    y = "Sleep duration (hours)"
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(size = 8.5))

p_models <- duration_results |>
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
    size = 2.9,
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
    title = "Adjustment tests alternative explanations",
    subtitle = "Below zero = shorter sleep; bedtime is a pathway sensitivity",
    x = "Difference in sleep duration",
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
  (p_duration + p_models) +
  plot_layout(guides = "collect", widths = c(1, 1), heights = c(1, 1)) +
  plot_annotation(
    title = str_wrap(
      "Evening brainwork coincided with later bedtimes and shorter sleep",
      width = 72
    ),
    subtitle = str_wrap(
      "A small number of demanding-work evenings limits certainty, but the pattern can be examined from description to adjusted models",
      width = 110
    ),
    caption = str_wrap(
      paste(
        "Brainwork = any positive aivotyo value; zero = no evening brainwork.",
        "The context-adjusted model controls for exact previous-night sleep, stress, health, exercise, coffee, weekday, and year-month.",
        "Bedtime is added only as a possible-pathway sensitivity check.",
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

p_insomnia <- insomnia_summary |>
  ggplot(
    aes(
      x = brainwork,
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
    size = 3.2,
    color = col_dark_text,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c(
      "Difficulty falling asleep" = col_orange,
      "Stress-related early waking" = col_dark_blue
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.18))
  ) +
  labs(
    title = "Difficulty falling asleep was more common on brainwork nights",
    subtitle = "Observed rates; small brainwork-night count limits precision",
    x = NULL,
    y = "Share of nights",
    color = NULL
  ) +
  theme_sleep()

p_stress_context <- stress_context_summary |>
  ggplot(aes(x = stress_label, y = brainwork_rate, fill = stress_label)) +
  geom_col(width = 0.62, alpha = 0.9) +
  geom_text(
    aes(label = label),
    vjust = -0.35,
    size = 3.2,
    lineheight = 0.9,
    color = col_dark_text,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "No recorded stress" = col_light_blue,
      "Recorded stress" = col_orange
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Evening brainwork was more common when stress was recorded",
    subtitle = "Descriptive context for potential confounding by workload or stress",
    x = NULL,
    y = "Nights with brainwork"
  ) +
  theme_sleep()

# Individual panels from the four-panel main figure are not saved separately.
figures_to_save <- list(
  "brainwork_figure1_main.png" = list(
    plot = p_main,
    width = 10,
    height = 12.5
  ),
  "brainwork_figureS1_insomnia_rates.png" = list(
    plot = p_insomnia,
    width = 10,
    height = 6
  ),
  "brainwork_figureS2_stress_context.png" = list(
    plot = p_stress_context,
    width = 9,
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

cat("\n========== BRAINWORK REPORTING SUMMARY ==========\n")
cat("Research questions are documented at the top of this script.\n")
cat(
  "Primary coding: aivotyo = 0 means no evening brainwork; every positive",
  "value means evening brainwork. Raw values remain unchanged.\n"
)
cat(
  "The pooled context-adjusted model is primary. Bedtime adjustment is a",
  "possible-pathway sensitivity analysis, not the default interpretation.\n"
)
cat("Main figure saved to:", file.path(figure_dir, "brainwork_figure1_main.png"), "\n")
cat("Supporting figures saved to:", figure_dir, "\n")
cat("No CSV files were created.\n")
cat(
  "Interpretation note: evening brainwork is rare and selected by daily",
  "circumstances, so estimates are exploratory associations rather than",
  "causal effects.\n"
)
