# =============================================================================
# bedtime.R
#
# Purpose: Analyze the association between bedtime and sleep outcomes
#
# Research questions:
#   How often do I go to bed before 23:00, between 23:00 and 00:00, or after 00:00?
#   How is bedtime associated with sleep duration?
#   How is bedtime associated with difficulty falling asleep and early waking?
#
# Input:
#   df_clean from scripts/01_load_main_data.R
#
# Outputs:
#   - descriptive summaries printed to console
#   - variable-specific figures saved to outputs/figures/variable_specific/bedtime/
#   - model summaries printed to the console
#   - raw, calendar-adjusted, and fully adjusted models for reporting
#
# Notes for interpretation:
#   - Bedtime is treated as a diary exposure variable.
#   - Sleep duration and the two recorded insomnia types are treated as outcomes.
#   - Results should be interpreted as associations, not causal effects.
#   - Month fixed effects compare nights within the same year-month period.
# =============================================================================

library(tidyverse)
library(fixest)
library(here)
library(patchwork)

source(here("scripts", "01_load_main_data.R"))

if (!exists("df_clean")) {
  stop("df_clean not found. Run 01_load_main_data.R first.")
}

figure_dir <- here("outputs", "figures", "variable_specific", "bedtime")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SETTINGS
# =============================================================================

variable_name <- "bedtime"
variable_label <- "Bedtime"
outcome_name <- "duration"
outcome_label <- "Sleep duration (hours)"

# Bedtime is an ordered categorical exposure.
variable_type <- "ordered categorical exposure"

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

pick_reference <- function(x, preferred) {
  c(intersect(preferred, levels(x)), levels(x)[1]) |>
    purrr::pluck(1)
}

safe_feglm <- function(fml, data, model_name) {
  data <- prepare_nw_data(data, fml)

  tryCatch(
    feglm(
      fml = fml,
      data = data,
      family = binomial(link = "logit"),
      vcov = NW(7) ~ series_id + date
    ),
    error = \(e) {
      warning("Model failed: ", model_name, ". Error: ", conditionMessage(e))
      NULL
    }
  )
}

# =============================================================================
# ANALYSIS DATA
# =============================================================================

dat_bedtime <- df_clean |>
  mutate(
    year_month = factor(format(date, "%Y-%m")),
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
    coffee_timing = factor(
      case_when(
        coffee_code == 0 ~ "No coffee",
        coffee_code %in% 1:2 ~ "Before noon",
        coffee_code == 3 ~ "After noon"
      ),
      levels = c("No coffee", "Before noon", "After noon")
    )
  ) |>
  select(
    date,
    series_id,
    year_month,
    day_of_week,
    duration,
    insomnia_num,
    insomnia_onset,
    insomnia_early_waking,
    bedtime_code,
    bedtime,
    coffee_code,
    coffee,
    coffee_timing,
    stress,
    health,
    exercise
  ) |>
  drop_na(bedtime, duration)

cat("\n========== BEDTIME ANALYSIS SAMPLE ==========\n")
cat("Observations:", nrow(dat_bedtime), "\n")
cat(
  "Date range:", format(min(dat_bedtime$date), "%Y-%m-%d"), "to",
  format(max(dat_bedtime$date), "%Y-%m-%d"), "\n"
)

# =============================================================================
# DESCRIPTIVE SUMMARIES
# =============================================================================

bedtime_summary <- dat_bedtime |>
  group_by(bedtime) |>
  summarise(
    n = n(),
    share = n / nrow(dat_bedtime),
    mean_sleep = mean(duration, na.rm = TRUE),
    median_sleep = median(duration, na.rm = TRUE),
    sd_sleep = sd(duration, na.rm = TRUE),
    se_sleep = sd_sleep / sqrt(n),
    ci_low = mean_sleep - 1.96 * se_sleep,
    ci_high = mean_sleep + 1.96 * se_sleep,
    insomnia_onset_rate = mean(insomnia_onset == 1, na.rm = TRUE),
    insomnia_early_waking_rate = mean(insomnia_early_waking == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(
        share, mean_sleep, median_sleep, sd_sleep, se_sleep, ci_low, ci_high,
        insomnia_onset_rate, insomnia_early_waking_rate
      ),
      \(x) round(x, 3)
    )
  )

weekday_bedtime_summary <- dat_bedtime |>
  group_by(day_of_week, bedtime) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(day_of_week) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  mutate(share = round(share, 3))

monthly_bedtime_summary <- dat_bedtime |>
  group_by(year_month, bedtime) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(year_month) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  mutate(share = round(share, 3))

insomnia_type_summary <- dat_bedtime |>
  select(bedtime, insomnia_onset, insomnia_early_waking) |>
  pivot_longer(
    cols = c(insomnia_onset, insomnia_early_waking),
    names_to = "insomnia_type",
    values_to = "recorded"
  ) |>
  mutate(
    insomnia_type = recode(
      insomnia_type,
      insomnia_onset = "Difficulty falling asleep",
      insomnia_early_waking = "Stress-related early waking"
    )
  ) |>
  group_by(bedtime, insomnia_type) |>
  summarise(
    n = sum(!is.na(recorded)),
    events = sum(recorded == 1, na.rm = TRUE),
    rate = mean(recorded == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    insomnia_type = factor(
      insomnia_type,
      levels = c("Difficulty falling asleep", "Stress-related early waking")
    ),
    rate_label = scales::percent(rate, accuracy = 1),
    label_vjust = if_else(insomnia_type == "Difficulty falling asleep", -0.8, 1.5)
  )

coffee_bedtime_summary <- dat_bedtime |>
  drop_na(coffee_timing) |>
  count(coffee_timing, bedtime, name = "n") |>
  group_by(coffee_timing) |>
  mutate(
    total_n = sum(n),
    share = n / total_n
  ) |>
  ungroup()

exercise_bedtime_summary <- dat_bedtime |>
  drop_na(exercise) |>
  count(exercise, bedtime, name = "n") |>
  group_by(exercise) |>
  mutate(
    total_n = sum(n),
    share = n / total_n
  ) |>
  ungroup()

yearly_bedtime_summary <- dat_bedtime |>
  mutate(year = factor(format(date, "%Y"))) |>
  group_by(year, bedtime) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(year) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  mutate(share = round(share, 3))

yearly_bedtime_totals <- dat_bedtime |>
  mutate(year = factor(format(date, "%Y"))) |>
  count(year, name = "n_year")

cat("\n========== SLEEP OUTCOMES BY BEDTIME ==========\n")
print(bedtime_summary, n = Inf, width = Inf)

cat("\n========== BEDTIME BY WEEKDAY ==========\n")
print(weekday_bedtime_summary, n = Inf, width = Inf)

# =============================================================================
# VISUALIZATIONS
# =============================================================================

p_distribution <- bedtime_summary |>
  ggplot(aes(x = bedtime, y = share, fill = bedtime)) +
  geom_col(alpha = 0.85) +
  geom_text(
    aes(label = paste0(scales::percent(share, accuracy = 1), "\n(n=", n, ")")),
    vjust = -0.4,
    size = 3.2,
    color = col_dark_text
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.24))
  ) +
  scale_fill_manual(
    values = make_palette(n_distinct(dat_bedtime$bedtime)),
    guide = "none"
  ) +
  labs(
    title = "Almost half of nights start before 23:00",
    subtitle = paste0("Share of observed nights (N = ", nrow(dat_bedtime), ")"),
    x = NULL,
    y = "Share of nights"
  ) +
  coord_cartesian(clip = "off") +
  theme_sleep()

duration_label_y <- max(
  9.5,
  as.numeric(quantile(dat_bedtime$duration, 0.99, na.rm = TRUE)) + 0.35
)
duration_plot_upper <- max(
  max(dat_bedtime$duration, na.rm = TRUE) + 0.25,
  duration_label_y + 0.65
)

p_duration <- dat_bedtime |>
  ggplot(aes(x = bedtime, y = duration, fill = bedtime)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA) +
  geom_jitter(width = 0.14, alpha = 0.05, size = 0.85, color = col_dark_text) +
  geom_label(
    data = bedtime_summary,
    aes(
      x = bedtime,
      y = duration_label_y,
      label = paste0("Median: ", sprintf("%.1f", median_sleep), " h")
    ),
    fill = "grey98",
    alpha = 0.8,
    linewidth = 0.08,
    size = 2.7,
    fontface = "plain",
    color = col_dark_text
  ) +
  scale_fill_manual(
    values = make_palette(n_distinct(dat_bedtime$bedtime)),
    guide = "none"
  ) +
  labs(
    title = "Later bedtimes are associated with shorter sleep",
    subtitle = paste0("Boxplots, individual nights, and median sleep durations (N = ", nrow(dat_bedtime), ")"),
    x = NULL,
    y = outcome_label
  ) +
  coord_cartesian(ylim = c(0, duration_plot_upper), clip = "off") +
  theme_sleep()

p_insomnia_types <- insomnia_type_summary |>
  ggplot(
    aes(
      x = bedtime,
      y = rate,
      color = insomnia_type,
      group = insomnia_type
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.8) +
  geom_text(
    aes(label = rate_label, vjust = label_vjust),
    color = col_dark_text,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.18))
  ) +
  scale_color_manual(
    values = c(
      "Difficulty falling asleep" = col_orange,
      "Stress-related early waking" = col_dark_blue
    )
  ) +
  labs(
    title = "Insomnia types show opposite bedtime patterns",
    subtitle = "Observed shares of nights by recorded insomnia type",
    x = NULL,
    y = "Share of nights",
    color = NULL
  ) +
  theme_sleep()

coffee_bedtime_totals <- coffee_bedtime_summary |>
  distinct(coffee_timing, total_n)

p_coffee_context <- coffee_bedtime_summary |>
  ggplot(aes(x = coffee_timing, y = share, fill = bedtime)) +
  geom_col(alpha = 0.92) +
  geom_text(
    data = coffee_bedtime_totals,
    aes(x = coffee_timing, y = 1.03, label = paste0("n=", total_n)),
    inherit.aes = FALSE,
    size = 3,
    color = col_dark_text
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = make_palette(n_distinct(dat_bedtime$bedtime))) +
  labs(
    title = "Bedtimes by coffee timing",
    subtitle = "Descriptive context; after-noon coffee is rare",
    x = NULL,
    y = "Share of nights",
    fill = NULL
  ) +
  coord_cartesian(ylim = c(0, 1.08), clip = "off") +
  theme_sleep()

exercise_bedtime_totals <- exercise_bedtime_summary |>
  distinct(exercise, total_n)

p_exercise_context <- exercise_bedtime_summary |>
  ggplot(aes(x = exercise, y = share, fill = bedtime)) +
  geom_col(alpha = 0.92) +
  geom_text(
    data = exercise_bedtime_totals,
    aes(x = exercise, y = 1.03, label = paste0("n=", total_n)),
    inherit.aes = FALSE,
    size = 3,
    color = col_dark_text
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = make_palette(n_distinct(dat_bedtime$bedtime))) +
  labs(
    title = "Bedtimes by exercise",
    subtitle = "Descriptive context for a potential upstream factor",
    x = NULL,
    y = "Share of nights",
    fill = NULL
  ) +
  coord_cartesian(ylim = c(0, 1.08), clip = "off") +
  theme_sleep()

p_bedtime_context <- p_coffee_context + p_exercise_context +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Coffee timing and exercise provide context for bedtime",
    subtitle = "These descriptive panels do not establish that either behavior causes a later bedtime"
  ) &
  theme(legend.position = "bottom")

p_over_time <- yearly_bedtime_summary |>
  ggplot(aes(x = year, y = share, fill = bedtime)) +
  geom_col(alpha = 0.92) +
  geom_text(
    data = yearly_bedtime_totals,
    aes(x = year, y = 1.03, label = paste0("n=", n_year)),
    inherit.aes = FALSE,
    size = 3,
    color = col_dark_text
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = make_palette(n_distinct(dat_bedtime$bedtime))) +
  labs(
    title = "Bedtimes vary over time",
    subtitle = "Yearly share of nights by bedtime category; labels show total nights per year",
    x = NULL,
    y = "Share of nights",
    fill = NULL,
    caption = "2017 and 2026 are partial years."
  ) +
  coord_cartesian(ylim = c(0, 1.08), clip = "off") +
  theme_sleep()

print(p_over_time)

save_plot <- function(plot, filename, width = 10, height = 6) {
  ggsave(
    file.path(figure_dir, filename),
    plot,
    width = width,
    height = height,
    dpi = 300
  )
}

save_plot_versions <- function(plot, filenames, width = 10, height = 6) {
  purrr::walk(
    filenames,
    \(filename) save_plot(plot, filename, width = width, height = height)
  )
}

save_plot_versions(
  p_distribution,
  c("bedtime_figureS4_distribution.png"),
  width = 8,
  height = 6
)
save_plot_versions(
  p_duration,
  c("bedtime_figureS5_sleep_duration_boxplot.png"),
  width = 8,
  height = 6
)
save_plot_versions(
  p_over_time,
  c("bedtime_figureS3_over_time.png"),
  width = 10,
  height = 6
)
save_plot_versions(
  p_bedtime_context,
  c("bedtime_figureS6_coffee_exercise_context.png"),
  width = 12,
  height = 6.5
)

# =============================================================================
# MODEL DATA
# =============================================================================

dat_model <- dat_bedtime |>
  mutate(
    bedtime = factor(bedtime, levels = levels(bedtime), ordered = FALSE),
    coffee = factor(coffee, levels = levels(coffee), ordered = FALSE),
    stress = factor(stress, levels = levels(stress), ordered = FALSE),
    health = factor(health, levels = levels(health), ordered = FALSE),
    exercise = factor(exercise, levels = levels(exercise), ordered = FALSE),
    day_of_week = fct_drop(day_of_week),
    year_month = fct_drop(year_month)
  ) |>
  drop_na(bedtime, duration, coffee, stress, health, exercise, day_of_week, year_month) |>
  prepare_nw_data()

reference_bedtime <- pick_reference(dat_model$bedtime, "Before 23:00")
reference_coffee <- pick_reference(dat_model$coffee, "None")
reference_stress <- pick_reference(dat_model$stress, "No")
reference_health <- pick_reference(dat_model$health, "Healthy")
reference_exercise <- pick_reference(dat_model$exercise, "None")
reference_day <- pick_reference(dat_model$day_of_week, "Mon")

cat("\n========== BEDTIME MODELLING SAMPLE ==========\n")
cat("Observations:", nrow(dat_model), "\n")
cat("Reference bedtime:", reference_bedtime, "\n")
cat("Reference coffee:", reference_coffee, "\n")
cat("Reference stress:", reference_stress, "\n")
cat("Reference health:", reference_health, "\n")
cat("Reference exercise:", reference_exercise, "\n")
cat("Reference weekday:", reference_day, "\n")

# =============================================================================
# SLEEP DURATION MODELS
# =============================================================================

models_duration <- list(
  "Raw" = feols(
    duration ~ i(bedtime, ref = reference_bedtime),
    data = dat_model,
    vcov = NW(7) ~ series_id + date
  ),
  "Calendar adjusted" = feols(
    duration ~
      i(bedtime, ref = reference_bedtime) +
      i(day_of_week, ref = reference_day) |
      year_month,
    data = dat_model,
    vcov = NW(7) ~ series_id + date
  ),
  "Fully adjusted" = feols(
    duration ~
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(exercise, ref = reference_exercise) +
      i(day_of_week, ref = reference_day) |
      year_month,
    data = dat_model,
    vcov = NW(7) ~ series_id + date
  )
)

purrr::iwalk(
  models_duration,
  \(model, model_name) {
    cat("\n==========", toupper(model_name), "BEDTIME MODEL: SLEEP DURATION ==========\n")
    print(summary(model))
  }
)

duration_model_comparison <- tibble(
  model = names(models_duration),
  n = purrr::map_int(models_duration, nobs),
  rmse = purrr::map_dbl(models_duration, \(model) sqrt(mean(resid(model)^2))),
  r2 = purrr::map_dbl(models_duration, \(model) fitstat(model, "r2") |> as.numeric())
) |>
  mutate(across(c(rmse, r2), \(x) round(x, 3)))

cat("\n========== SLEEP DURATION MODEL COMPARISON ==========\n")
print(duration_model_comparison, n = Inf, width = Inf)

# =============================================================================
# INSOMNIA MODELS
# =============================================================================

models_insomnia <- list(
  "Difficulty falling asleep" = list(
    "Raw" = safe_feglm(
      insomnia_onset ~ i(bedtime, ref = reference_bedtime),
      data = dat_model,
      model_name = "Difficulty falling asleep: raw"
    ),
    "Calendar adjusted" = safe_feglm(
      insomnia_onset ~
        i(bedtime, ref = reference_bedtime) +
        i(day_of_week, ref = reference_day) |
        year_month,
      data = dat_model,
      model_name = "Difficulty falling asleep: calendar adjusted"
    ),
    "Fully adjusted" = safe_feglm(
      insomnia_onset ~
        i(bedtime, ref = reference_bedtime) +
        i(coffee, ref = reference_coffee) +
        i(stress, ref = reference_stress) +
        i(health, ref = reference_health) +
        i(exercise, ref = reference_exercise) +
        i(day_of_week, ref = reference_day) |
        year_month,
      data = dat_model,
      model_name = "Difficulty falling asleep: fully adjusted"
    )
  ) |>
    purrr::compact(),
  "Stress-related early waking" = list(
    "Raw" = safe_feglm(
      insomnia_early_waking ~ i(bedtime, ref = reference_bedtime),
      data = dat_model,
      model_name = "Stress-related early waking: raw"
    ),
    "Calendar adjusted" = safe_feglm(
      insomnia_early_waking ~
        i(bedtime, ref = reference_bedtime) +
        i(day_of_week, ref = reference_day) |
        year_month,
      data = dat_model,
      model_name = "Stress-related early waking: calendar adjusted"
    ),
    "Fully adjusted" = safe_feglm(
      insomnia_early_waking ~
        i(bedtime, ref = reference_bedtime) +
        i(coffee, ref = reference_coffee) +
        i(stress, ref = reference_stress) +
        i(health, ref = reference_health) +
        i(exercise, ref = reference_exercise) +
        i(day_of_week, ref = reference_day) |
        year_month,
      data = dat_model,
      model_name = "Stress-related early waking: fully adjusted"
    )
  ) |>
    purrr::compact()
) |>
  purrr::keep(\(models) length(models) > 0)

if (length(models_insomnia) > 0) {
  purrr::iwalk(
    models_insomnia,
    \(models, insomnia_type) {
      purrr::iwalk(
        models,
        \(model, model_name) {
          cat(
            "\n==========", toupper(model_name), "BEDTIME MODEL:",
            toupper(insomnia_type), "==========\n"
          )
          print(summary(model))
        }
      )
    }
  )

  insomnia_model_comparison <- models_insomnia |>
    purrr::imap_dfr(
      \(models, insomnia_type) {
        tibble(
          insomnia_type = insomnia_type,
          model = names(models),
          n = purrr::map_int(models, nobs),
          log_likelihood = purrr::map_dbl(models, \(model) logLik(model) |> as.numeric()),
          aic = purrr::map_dbl(models, AIC),
          bic = purrr::map_dbl(models, BIC)
        )
      }
    ) |>
    mutate(across(c(log_likelihood, aic, bic), \(x) round(x, 2)))

  cat("\n========== INSOMNIA-TYPE MODEL COMPARISON ==========\n")
  print(insomnia_model_comparison, n = Inf, width = Inf)
}

# =============================================================================
# REGRESSION COEFFICIENT PLOTS
# =============================================================================

clean_bedtime_term <- function(x) {
  x |>
    str_remove_all("`") |>
    str_replace("^bedtime::", "")
}

get_duration_results <- function(model_results) {
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
        filter(str_detect(term, "^bedtime::")) |>
        transmute(
          model = model_name,
          bedtime = clean_bedtime_term(term),
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

bedtime_duration_results <- get_duration_results(models_duration) |>
  mutate(
    model = factor(model, levels = c("Raw", "Calendar adjusted", "Fully adjusted")),
    bedtime = factor(bedtime, levels = rev(levels(dat_model$bedtime)))
  )

fully_adjusted_duration_results <- bedtime_duration_results |>
  filter(model == "Fully adjusted")

p_duration_coef <- bedtime_duration_results |>
  ggplot(
    aes(
      y = bedtime,
      x = estimate_minutes,
      color = model
    )
  ) +
  geom_segment(
    aes(x = ci_low_minutes, xend = ci_high_minutes, y = bedtime, yend = bedtime),
    linewidth = 1.1,
    alpha = 0.65,
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
      "Calendar adjusted" = col_steel,
      "Fully adjusted" = col_orange
    )
  ) +
  scale_x_continuous(
    labels = \(x) paste0(round(x), " min"),
    breaks = scales::breaks_pretty(n = 6)
  ) +
  labs(
    title = "Model comparison for sleep-duration differences",
    subtitle = paste0("Estimates relative to ", reference_bedtime, "; negative values indicate shorter sleep"),
    x = "Difference in sleep duration (minutes)",
    y = NULL,
    color = NULL
  ) +
  theme_sleep() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(color = "grey90")
  )

print(p_duration_coef)
save_plot_versions(
  p_duration_coef,
  c("bedtime_figureS1_duration_model_comparison.png", "bedtime_duration_coefficients.png"),
  width = 10,
  height = 6
)

if (length(models_insomnia) > 0) {
  get_insomnia_results <- function(model_results, insomnia_type) {
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
          filter(str_detect(term, "^bedtime::")) |>
          transmute(
            insomnia_type = insomnia_type,
            model = model_name,
            bedtime = clean_bedtime_term(term),
            odds_ratio = exp(estimate),
            ci_low = exp(estimate - 1.96 * std_error),
            ci_high = exp(estimate + 1.96 * std_error)
          )
      }
    )
  }

  bedtime_insomnia_results <- models_insomnia |>
    purrr::imap_dfr(
      \(models, insomnia_type) get_insomnia_results(models, insomnia_type)
    ) |>
    mutate(
      insomnia_type = factor(
        insomnia_type,
        levels = c("Difficulty falling asleep", "Stress-related early waking")
      ),
      model = factor(model, levels = c("Raw", "Calendar adjusted", "Fully adjusted")),
      bedtime = factor(bedtime, levels = rev(levels(dat_model$bedtime)))
    )

  p_insomnia_coef <- bedtime_insomnia_results |>
    ggplot(
      aes(
        y = bedtime,
        color = model
      )
    ) +
    geom_vline(xintercept = 1, linewidth = 0.3, linetype = "dashed") +
    geom_pointrange(
      aes(x = odds_ratio, xmin = ci_low, xmax = ci_high),
      position = position_dodge(width = 0.55),
      linewidth = 0.7
    ) +
    scale_x_log10(
      labels = scales::number_format(accuracy = 0.1),
      breaks = c(0.25, 0.5, 1, 2, 4, 8)
    ) +
    scale_color_manual(
      values = c(
        "Raw" = col_light_blue,
        "Calendar adjusted" = col_steel,
        "Fully adjusted" = col_orange
      ),
      na.translate = FALSE
    ) +
    facet_wrap(vars(insomnia_type), ncol = 1) +
    labs(
      title = "Model comparison for insomnia-type odds ratios",
      subtitle = paste0(
        "Odds ratios relative to ",
        reference_bedtime,
        "; values above 1 indicate higher odds"
      ),
      x = "Odds ratio, log scale",
      y = NULL,
      color = NULL
    ) +
    theme_sleep() +
    theme(
      legend.position = "bottom",
      panel.grid.major.x = element_line(color = "grey90")
    )

  print(p_insomnia_coef)
  save_plot_versions(
    p_insomnia_coef,
    c(
      "bedtime_figureS2_insomnia_type_model_comparison.png",
      "bedtime_insomnia_type_odds_ratios.png"
    ),
    width = 10,
    height = 8
  )
}

# =============================================================================
# PUBLICATION-FACING FOUR-PANEL FIGURE
# =============================================================================

duration_main_results <- tibble(
  bedtime = levels(dat_model$bedtime)
) |>
  left_join(
    fully_adjusted_duration_results |>
      mutate(bedtime = as.character(bedtime)) |>
      select(
        bedtime,
        estimate_minutes,
        ci_low_minutes,
        ci_high_minutes
      ),
    by = "bedtime"
  ) |>
  mutate(
    across(
      c(estimate_minutes, ci_low_minutes, ci_high_minutes),
      \(x) replace_na(x, 0)
    ),
    duration_label = if_else(
      bedtime == reference_bedtime,
      "Reference",
      paste0(if_else(estimate_minutes >= 0, "+", ""), round(estimate_minutes), " min")
    ),
    bedtime = factor(bedtime, levels = rev(levels(dat_model$bedtime)))
  )

p_duration_main <- duration_main_results |>
  ggplot(
    aes(
      y = bedtime,
      x = estimate_minutes,
      xmin = ci_low_minutes,
      xmax = ci_high_minutes
    )
  ) +
  geom_segment(
    aes(x = ci_low_minutes, xend = ci_high_minutes, yend = bedtime),
    linewidth = 1.1,
    color = col_dark_blue
  ) +
  geom_point(size = 2.7, color = col_orange) +
  geom_text(
    aes(x = ci_high_minutes, label = duration_label),
    nudge_x = 5,
    nudge_y = 0.12,
    color = col_dark_text,
    size = 3.1,
    fontface = "bold"
  ) +
  geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") +
  scale_x_continuous(
    labels = \(x) paste0(round(x), " min"),
    breaks = scales::breaks_pretty(n = 6),
    expand = expansion(mult = c(0.08, 0.22))
  ) +
  labs(
    title = "Adjusted sleep differences are largest after midnight",
    subtitle = "Fully adjusted estimates relative to before 23:00; negative values mean shorter sleep",
    x = "Difference in sleep duration (minutes)",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

p_main <- (p_distribution + p_duration) /
  (p_duration_main + p_insomnia_types) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Later bedtimes are associated with shorter sleep and different insomnia patterns",
    subtitle = "Descriptive sleep-diary patterns and fully adjusted sleep-duration estimates",
    caption = paste(
      "Panel C adjusts for coffee, stress, health, exercise, weekday, and month;",
      "95% CIs use a 7-day Newey-West estimator. Panels A, B, and D are descriptive.",
      "Associations are not causal."
    ),
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(size = 17, face = "bold"),
      plot.subtitle = element_text(size = 11, color = col_grey),
      plot.caption = element_text(size = 8.5, color = "grey45", hjust = 0)
    )
  ) &
  theme(legend.position = "bottom")

print(p_main)
save_plot_versions(
  p_main,
  c("bedtime_figure1_main.png"),
  width = 10,
  height = 12.5
)

save_plot_versions(
  p_insomnia_types,
  c("bedtime_figureS7_insomnia_type_rates.png"),
  width = 8,
  height = 6
)

# =============================================================================
# REPORTING SUMMARY
# =============================================================================

cat("\n========== REPORTING SUMMARY ==========\n")
cat(
  "The script describes bedtime patterns and estimates raw, calendar-adjusted,",
  "and fully adjusted models for sleep duration and two recorded insomnia types.\n"
)
cat("Reference bedtime:", reference_bedtime, "\n")
cat("Main figure saved to:", file.path(figure_dir, "bedtime_figure1_main.png"), "\n")
cat("Supporting duration coefficient figure saved to:", file.path(figure_dir, "bedtime_duration_coefficients.png"), "\n")
cat("Supporting insomnia odds-ratio figure saved to:", file.path(figure_dir, "bedtime_insomnia_type_odds_ratios.png"), "\n")
cat("Supporting bedtime distribution figure saved to:", file.path(figure_dir, "bedtime_figureS4_distribution.png"), "\n")
cat("Supporting sleep-duration figure saved to:", file.path(figure_dir, "bedtime_figureS5_sleep_duration_boxplot.png"), "\n")
cat("Supporting bedtime-over-time figure saved to:", file.path(figure_dir, "bedtime_figureS3_over_time.png"), "\n")
cat("Supporting coffee/exercise figure saved to:", file.path(figure_dir, "bedtime_figureS6_coffee_exercise_context.png"), "\n")
cat("Supporting insomnia-rate figure saved to:", file.path(figure_dir, "bedtime_figureS7_insomnia_type_rates.png"), "\n")
