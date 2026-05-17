# =============================================================================
# exercise.R
#
# Purpose: Analyze the association between exercise and sleep outcomes
#
# Research questions:
#   How often is exercise recorded in the sleep diary?
#   How is exercise timing/intensity associated with sleep duration?
#   How is exercise timing/intensity associated with recorded insomnia?
#
# Input:
#   df_clean from scripts/01_load_main_data.R
#
# Outputs:
#   - descriptive summaries printed to console
#   - numbered variable-specific figures saved to figures/variable_specific/exercise/
#   - raw, adjusted, and month fixed-effect models for reporting
#
# Notes for interpretation:
#   - Exercise is treated as an ordered categorical diary exposure.
#   - Sleep duration and any recorded insomnia are treated as outcomes.
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

figure_dir <- here("figures", "variable_specific", "exercise")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SETTINGS AND HELPERS
# =============================================================================

variable_name <- "exercise"
variable_label <- "Exercise"
outcome_name <- "duration"
outcome_label <- "Sleep duration (hours)"
variable_type <- "ordered categorical exposure"

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
  ggsave(
    file.path(figure_dir, filename),
    plot,
    width = width,
    height = height,
    dpi = 300
  )
}

pick_reference <- function(x, preferred) {
  c(intersect(preferred, levels(x)), levels(x)[1]) |>
    purrr::pluck(1)
}

fmt_pct <- function(x, accuracy = 1) scales::percent(x, accuracy = accuracy)
fmt_min <- function(x) paste0(if_else(x > 0, "+", ""), round(x), " min")

safe_feglm <- function(fml, data, model_name) {
  tryCatch(
    feglm(
      fml = fml,
      data = data,
      family = binomial(link = "logit"),
      vcov = "hetero"
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

dat_exercise <- df_clean |>
  mutate(
    year = factor(format(date, "%Y")),
    year_month = factor(format(date, "%Y-%m")),
    insomnia_any = as.integer(insomnia_num > 0)
  ) |>
  select(
    date,
    year,
    year_month,
    day_of_week,
    duration,
    insomnia_num,
    insomnia_any,
    exercise_code,
    exercise,
    bedtime,
    coffee,
    stress,
    health
  ) |>
  drop_na(exercise, duration, insomnia_num)

n_total <- nrow(dat_exercise)

cat("\n========== EXERCISE ANALYSIS SAMPLE ==========\n")
cat("Observations:", n_total, "\n")
cat(
  "Date range:", format(min(dat_exercise$date), "%Y-%m-%d"), "to",
  format(max(dat_exercise$date), "%Y-%m-%d"), "\n"
)

# =============================================================================
# DESCRIPTIVE SUMMARIES
# =============================================================================

exercise_summary <- dat_exercise |>
  group_by(exercise) |>
  summarise(
    n = n(),
    share = n / nrow(dat_exercise),
    mean_sleep = mean(duration, na.rm = TRUE),
    median_sleep = median(duration, na.rm = TRUE),
    sd_sleep = sd(duration, na.rm = TRUE),
    se_sleep = sd_sleep / sqrt(n),
    ci_low = mean_sleep - 1.96 * se_sleep,
    ci_high = mean_sleep + 1.96 * se_sleep,
    insomnia_n = sum(insomnia_any == 1, na.rm = TRUE),
    insomnia_rate = mean(insomnia_any == 1, na.rm = TRUE),
    insomnia_se = sqrt(insomnia_rate * (1 - insomnia_rate) / n),
    insomnia_ci_low = pmax(insomnia_rate - 1.96 * insomnia_se, 0),
    insomnia_ci_high = pmin(insomnia_rate + 1.96 * insomnia_se, 1),
    .groups = "drop"
  ) |>
  mutate(
    distribution_label = paste0(fmt_pct(share, accuracy = 1), "\n(n=", n, ")"),
    median_label = paste0("Median: ", median_sleep, " h"),
    insomnia_label = fmt_pct(insomnia_rate, accuracy = 1),
    across(
      c(
        share, mean_sleep, median_sleep, sd_sleep, se_sleep, ci_low, ci_high,
        insomnia_rate, insomnia_se, insomnia_ci_low, insomnia_ci_high
      ),
      \(x) round(x, 3)
    )
  )

yearly_exercise_summary <- dat_exercise |>
  group_by(year, exercise) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(year) |>
  mutate(
    year_n = sum(n),
    share = n / year_n
  ) |>
  ungroup() |>
  mutate(share = round(share, 3))

yearly_n <- yearly_exercise_summary |>
  distinct(year, year_n) |>
  mutate(n_label = paste0("n=", year_n))

cat("\n========== SLEEP OUTCOMES BY EXERCISE ==========\n")
print(exercise_summary, n = Inf, width = Inf)

cat("\n========== EXERCISE BY YEAR ==========\n")
print(yearly_exercise_summary, n = Inf, width = Inf)

# =============================================================================
# DESCRIPTIVE VISUALIZATIONS
# =============================================================================

exercise_palette <- make_palette(n_distinct(dat_exercise$exercise))
label_y <- quantile(dat_exercise$duration, 0.97, na.rm = TRUE)
max_share <- max(exercise_summary$share, na.rm = TRUE)
insomnia_range_pp <- diff(range(exercise_summary$insomnia_rate, na.rm = TRUE)) * 100

p_distribution <- exercise_summary |>
  ggplot(aes(x = exercise, y = share, fill = exercise)) +
  geom_col(alpha = 0.85, width = 0.72) +
  geom_text(aes(label = distribution_label), vjust = -0.35, size = 3.2, color = col_dark_text) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max_share + 0.12)) +
  scale_fill_manual(values = exercise_palette, guide = "none") +
  labs(
    title = "Most nights have no recorded exercise",
    subtitle = paste0("Share of observed nights by exercise category (N = ", n_total, ")"),
    x = NULL,
    y = "Share of nights"
  ) +
  theme_sleep()

p_duration <- dat_exercise |>
  ggplot(aes(x = exercise, y = duration, fill = exercise)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.62) +
  geom_jitter(width = 0.12, alpha = 0.08, size = 1.05, color = col_dark_text) +
  geom_label(
    data = exercise_summary,
    aes(x = exercise, y = label_y, label = median_label),
    inherit.aes = FALSE,
    size = 2.8,
    label.size = 0.12,
    fill = "white",
    color = col_dark_text
  ) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  scale_fill_manual(values = exercise_palette, guide = "none") +
  labs(
    title = "Sleep duration differs little across exercise categories",
    subtitle = paste0("Boxplots, individual nights, and median sleep durations (N = ", n_total, ")"),
    x = NULL,
    y = outcome_label
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_mean_ci <- exercise_summary |>
  ggplot(aes(x = exercise, y = mean_sleep, group = 1)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, color = col_dark_blue, alpha = 0.8) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 3, color = col_orange) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  labs(
    title = "Average sleep duration by exercise category",
    subtitle = "Means with approximate 95% confidence intervals",
    x = NULL,
    y = "Mean sleep duration (hours)"
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_insomnia <- exercise_summary |>
  ggplot(aes(x = exercise, y = insomnia_rate, group = 1)) +
  geom_errorbar(aes(ymin = insomnia_ci_low, ymax = insomnia_ci_high), width = 0.12, color = col_dark_blue, alpha = 0.8) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 3, color = col_orange) +
  geom_text(aes(label = insomnia_label), vjust = -0.9, size = 3.1, color = col_dark_text) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, min(1, max(exercise_summary$insomnia_ci_high, na.rm = TRUE) + 0.08))
  ) +
  labs(
    title = paste0("Observed insomnia rates differ by about ", round(insomnia_range_pp), " percentage points"),
    subtitle = "Share of nights with any recorded insomnia; approximate 95% confidence intervals",
    x = NULL,
    y = "Insomnia rate"
  ) +
  theme_sleep()

p_over_time <- yearly_exercise_summary |>
  ggplot(aes(x = year, y = share, fill = exercise)) +
  geom_col(alpha = 0.9, width = 0.72) +
  geom_text(
    data = yearly_n,
    aes(x = year, y = 1.035, label = n_label),
    inherit.aes = FALSE,
    size = 2.8,
    color = col_dark_text
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.10),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(values = exercise_palette) +
  labs(
    title = "Exercise patterns vary over time",
    subtitle = "Yearly share of nights by exercise category; labels show total nights per year",
    x = NULL,
    y = "Share of nights",
    fill = NULL
  ) +
  theme_sleep()

# =============================================================================
# MODEL DATA
# =============================================================================

dat_model <- dat_exercise |>
  mutate(
    exercise = factor(exercise, levels = levels(exercise), ordered = FALSE),
    bedtime = factor(bedtime, levels = levels(bedtime), ordered = FALSE),
    coffee = factor(coffee, levels = levels(coffee), ordered = FALSE),
    stress = factor(stress, levels = levels(stress), ordered = FALSE),
    health = factor(health, levels = levels(health), ordered = FALSE),
    day_of_week = fct_drop(day_of_week),
    year_month = fct_drop(year_month)
  ) |>
  drop_na(exercise, duration, insomnia_any, bedtime, coffee, stress, health, day_of_week, year_month)

reference_exercise <- pick_reference(dat_model$exercise, "None")
reference_bedtime <- pick_reference(dat_model$bedtime, "Before 23:00")
reference_coffee <- pick_reference(dat_model$coffee, "None")
reference_stress <- pick_reference(dat_model$stress, "No")
reference_health <- pick_reference(dat_model$health, "Healthy")
reference_day <- pick_reference(dat_model$day_of_week, c("Mon", "Monday"))

cat("\n========== EXERCISE MODELLING SAMPLE ==========\n")
cat("Observations:", nrow(dat_model), "\n")
cat("Reference exercise:", reference_exercise, "\n")
cat("Reference bedtime:", reference_bedtime, "\n")
cat("Reference coffee:", reference_coffee, "\n")
cat("Reference stress:", reference_stress, "\n")
cat("Reference health:", reference_health, "\n")
cat("Reference weekday:", reference_day, "\n")

# =============================================================================
# SLEEP DURATION MODELS
# =============================================================================

models_duration <- list(
  "Raw" = feols(
    duration ~ i(exercise, ref = reference_exercise),
    data = dat_model,
    vcov = "hetero"
  ),
  "Adjusted" = feols(
    duration ~
      i(exercise, ref = reference_exercise) +
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(day_of_week, ref = reference_day),
    data = dat_model,
    vcov = "hetero"
  ),
  "Month FE" = feols(
    duration ~
      i(exercise, ref = reference_exercise) +
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(day_of_week, ref = reference_day) |
      year_month,
    data = dat_model,
    vcov = "hetero"
  )
)

purrr::iwalk(
  models_duration,
  \(model, model_name) {
    cat("\n==========", toupper(model_name), "EXERCISE MODEL: SLEEP DURATION ==========\n")
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
  "Raw" = safe_feglm(
    insomnia_any ~ i(exercise, ref = reference_exercise),
    data = dat_model,
    model_name = "Raw"
  ),
  "Adjusted" = safe_feglm(
    insomnia_any ~
      i(exercise, ref = reference_exercise) +
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(day_of_week, ref = reference_day),
    data = dat_model,
    model_name = "Adjusted"
  ),
  "Month FE" = safe_feglm(
    insomnia_any ~
      i(exercise, ref = reference_exercise) +
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(day_of_week, ref = reference_day) |
      year_month,
    data = dat_model,
    model_name = "Month FE"
  )
) |>
  purrr::compact()

if (length(models_insomnia) > 0) {
  purrr::iwalk(
    models_insomnia,
    \(model, model_name) {
      cat("\n==========", toupper(model_name), "EXERCISE MODEL: INSOMNIA ==========\n")
      print(summary(model))
    }
  )

  insomnia_model_comparison <- tibble(
    model = names(models_insomnia),
    n = purrr::map_int(models_insomnia, nobs),
    log_likelihood = purrr::map_dbl(models_insomnia, \(model) logLik(model) |> as.numeric()),
    aic = purrr::map_dbl(models_insomnia, AIC),
    bic = purrr::map_dbl(models_insomnia, BIC)
  ) |>
    mutate(across(c(log_likelihood, aic, bic), \(x) round(x, 2)))

  cat("\n========== INSOMNIA MODEL COMPARISON ==========\n")
  print(insomnia_model_comparison, n = Inf, width = Inf)
}

# =============================================================================
# MODEL RESULTS FOR FIGURES
# =============================================================================

clean_exercise_term <- function(x) {
  x |>
    str_remove_all("`") |>
    str_remove("^exercise::")
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
        filter(str_detect(term, "^exercise::")) |>
        transmute(
          model = model_name,
          exercise = clean_exercise_term(term),
          estimate_hours = estimate,
          ci_low_hours = estimate - 1.96 * std_error,
          ci_high_hours = estimate + 1.96 * std_error,
          estimate_minutes = estimate_hours * 60,
          ci_low_minutes = ci_low_hours * 60,
          ci_high_minutes = ci_high_hours * 60,
          label = fmt_min(estimate_minutes)
        )
    }
  )
}

exercise_duration_results <- get_duration_results(models_duration) |>
  mutate(
    model = factor(model, levels = c("Raw", "Adjusted", "Month FE")),
    exercise = factor(exercise, levels = rev(setdiff(levels(dat_model$exercise), reference_exercise)))
  )

cat("\n========== EXERCISE DURATION COEFFICIENTS ==========\n")
print(exercise_duration_results, n = Inf, width = Inf)

month_fe_duration_results <- exercise_duration_results |>
  filter(model == "Month FE")

max_abs_month_fe <- month_fe_duration_results |>
  summarise(max_abs = max(abs(estimate_minutes), na.rm = TRUE)) |>
  pull(max_abs)

p_duration_coef_main <- month_fe_duration_results |>
  ggplot(aes(y = exercise, x = estimate_minutes, xmin = ci_low_minutes, xmax = ci_high_minutes)) +
  geom_linerange(linewidth = 1.2, color = col_dark_blue, alpha = 0.9) +
  geom_point(size = 2.5, color = col_orange) +
  geom_text(aes(label = label), nudge_y = 0.12, size = 3, color = col_dark_text) +
  geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") +
  scale_y_discrete(labels = scales::label_wrap(18)) +
  scale_x_continuous(labels = \(x) paste0(round(x), " min"), breaks = scales::breaks_pretty(n = 6)) +
  labs(
    title = paste0("Adjusted sleep differences are at most about ", round(max_abs_month_fe), " minutes"),
    subtitle = paste0("Month fixed-effect estimates relative to ", reference_exercise, "; negative values mean shorter sleep"),
    x = "Difference in sleep duration (minutes)",
    y = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

p_duration_model_comparison <- exercise_duration_results |>
  ggplot(aes(y = exercise, x = estimate_minutes, xmin = ci_low_minutes, xmax = ci_high_minutes, color = model)) +
  geom_linerange(linewidth = 1.1, alpha = 0.75, position = position_dodge(width = 0.55)) +
  geom_point(size = 2.1, position = position_dodge(width = 0.55)) +
  geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") +
  scale_y_discrete(labels = scales::label_wrap(18)) +
  scale_color_manual(values = c("Raw" = col_light_blue, "Adjusted" = col_steel, "Month FE" = col_orange)) +
  scale_x_continuous(labels = \(x) paste0(round(x), " min"), breaks = scales::breaks_pretty(n = 6)) +
  labs(
    title = "Model comparison for exercise sleep-duration differences",
    subtitle = paste0("Estimates relative to ", reference_exercise, "; negative values indicate shorter sleep"),
    x = "Difference in sleep duration",
    y = NULL,
    color = NULL
  ) +
  theme_sleep() +
  theme(legend.position = "bottom", panel.grid.major.x = element_line(color = "grey90"))

if (length(models_insomnia) > 0) {
  get_insomnia_results <- function(model_results) {
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
          filter(str_detect(term, "^exercise::")) |>
          transmute(
            model = model_name,
            exercise = clean_exercise_term(term),
            odds_ratio = exp(estimate),
            ci_low = exp(estimate - 1.96 * std_error),
            ci_high = exp(estimate + 1.96 * std_error)
          )
      }
    )
  }

  exercise_insomnia_results <- get_insomnia_results(models_insomnia) |>
    mutate(
      model = factor(model, levels = c("Raw", "Adjusted", "Month FE")),
      exercise = factor(exercise, levels = rev(setdiff(levels(dat_model$exercise), reference_exercise)))
    )

  cat("\n========== EXERCISE INSOMNIA ODDS RATIOS ==========\n")
  print(exercise_insomnia_results, n = Inf, width = Inf)

  p_insomnia_model_comparison <- exercise_insomnia_results |>
    ggplot(aes(y = exercise, x = odds_ratio, xmin = ci_low, xmax = ci_high, color = model)) +
    geom_vline(xintercept = 1, linewidth = 0.3, linetype = "dashed") +
    geom_linerange(linewidth = 1.1, alpha = 0.75, position = position_dodge(width = 0.55)) +
    geom_point(size = 2.1, position = position_dodge(width = 0.55)) +
    scale_y_discrete(labels = scales::label_wrap(18)) +
    scale_x_log10(labels = scales::number_format(accuracy = 0.1), breaks = c(0.5, 1, 2, 4)) +
    scale_color_manual(values = c("Raw" = col_light_blue, "Adjusted" = col_steel, "Month FE" = col_orange), na.translate = FALSE) +
    labs(
      title = "Model comparison for exercise insomnia odds ratios",
      subtitle = paste0("Odds ratios relative to ", reference_exercise, "; values above 1 indicate higher odds"),
      x = "Odds ratio, log scale",
      y = NULL,
      color = NULL
    ) +
    theme_sleep() +
    theme(legend.position = "bottom", panel.grid.major.x = element_line(color = "grey90"))
}

# =============================================================================
# MAIN FIGURE AND SUPPORTING FIGURES
# =============================================================================

p_main <- (p_distribution + p_duration) / (p_duration_coef_main + p_insomnia) +
  plot_annotation(
    title = "Exercise shows limited association with sleep duration in this diary",
    subtitle = "Sleep diary associations across exercise categories; uncertainty intervals shown for model-based and rate estimates",
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(size = 14, face = "bold"))

print(p_main)
print(p_distribution)
print(p_duration)
print(p_duration_coef_main)
print(p_duration_model_comparison)
print(p_mean_ci)
print(p_insomnia)
print(p_over_time)

save_plot(p_main, "exercise_figure1_main.png", width = 14, height = 10)
save_plot(p_duration_model_comparison, "exercise_figureS1_duration_model_comparison.png", width = 12, height = 8)
if (exists("p_insomnia_model_comparison")) {
  print(p_insomnia_model_comparison)
  save_plot(p_insomnia_model_comparison, "exercise_figureS2_insomnia_model_comparison.png", width = 12, height = 8)
}
save_plot(p_over_time, "exercise_figureS3_over_time.png", width = 12, height = 6)
save_plot(p_distribution, "exercise_figureS4_distribution.png", width = 8, height = 6)
save_plot(p_duration, "exercise_figureS5_sleep_duration_boxplot.png", width = 8, height = 6)
save_plot(p_mean_ci, "exercise_figureS6_mean_sleep_ci.png", width = 8, height = 6)
save_plot(p_insomnia, "exercise_figureS7_insomnia_rate.png", width = 8, height = 6)

# Backward-compatible file names
save_plot(p_main, "exercise_overview.png", width = 14, height = 10)
save_plot(p_duration_model_comparison, "exercise_duration_coefficients.png", width = 12, height = 8)
save_plot(p_insomnia, "insomnia_by_exercise.png", width = 8, height = 6)
save_plot(p_duration, "sleep_duration_by_exercise.png", width = 8, height = 6)
save_plot(p_over_time, "exercise_over_time.png", width = 12, height = 6)

# =============================================================================
# REPORTING SUMMARY
# =============================================================================

cat("\n========== REPORTING SUMMARY ==========\n")
cat(
  "The script estimates raw, adjusted, and month fixed-effect exercise differences",
  "in sleep duration and insomnia relative to", reference_exercise, ".\n"
)
cat("Recommended main figure saved to:", file.path(figure_dir, "exercise_figure1_main.png"), "\n")
cat("Duration model-comparison figure saved to:", file.path(figure_dir, "exercise_figureS1_duration_model_comparison.png"), "\n")
cat("Other figures saved to:", figure_dir, "\n")
