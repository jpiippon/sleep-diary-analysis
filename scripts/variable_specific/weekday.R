# =============================================================================
# weekday.R
#
# Purpose: Analyze the association between day of week and sleep outcomes
#
# Research questions:
#   Does sleep duration vary systematically by day of week?
#   Does recorded insomnia vary systematically by day of week?
#
# Input:
#   df_clean from scripts/01_load_main_data.R
#
# Outputs:
#   - descriptive summaries printed to console
#   - numbered variable-specific figures saved to figures/variable_specific/weekday/
#   - model summaries and tables saved to outputs/variable_specific/weekday/
#   - raw, adjusted, and month fixed-effect models for reporting
#
# Notes for interpretation:
#   - Day of week is treated as a categorical exposure.
#   - Sleep duration and any recorded insomnia are treated as outcomes.
#   - Results should be interpreted as associations, not causal effects.
#   - Month fixed effects compare weekdays within the same year-month period.
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
output_dir <- here("outputs", "variable_specific", "weekday")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SETTINGS
# =============================================================================

variable_name <- "day_of_week"
variable_label <- "Day of week"
outcome_name <- "duration"
outcome_label <- "Sleep duration (hours)"
variable_type <- "categorical exposure"

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

save_plot <- function(plot, filename, width = 10, height = 6) {
  ggsave(
    file.path(figure_dir, filename),
    plot,
    width = width,
    height = height,
    dpi = 300
  )
}

get_mode <- function(x) {
  x_no_na <- x[!is.na(x)]
  x_levels <- unique(x_no_na)
  x_levels[which.max(tabulate(match(x_no_na, x_levels)))]
}

pick_reference <- function(x, preferred) {
  preferred_found <- preferred[preferred %in% levels(x)]
  c(preferred_found, levels(x)[1]) |>
    purrr::pluck(1)
}

fmt_pct <- function(x, accuracy = 1) {
  scales::percent(x, accuracy = accuracy)
}

fmt_min <- function(x) {
  paste0(if_else(x > 0, "+", ""), round(x), " min")
}

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

binomial_summary <- function(data, group_vars) {
  data |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      n = n(),
      insomnia_n = sum(insomnia_any == 1, na.rm = TRUE),
      insomnia_rate = mean(insomnia_any == 1, na.rm = TRUE),
      se = sqrt(insomnia_rate * (1 - insomnia_rate) / n),
      ci_low = pmax(insomnia_rate - 1.96 * se, 0),
      ci_high = pmin(insomnia_rate + 1.96 * se, 1),
      .groups = "drop"
    ) |>
    mutate(across(c(insomnia_rate, se, ci_low, ci_high), \(x) round(x, 3)))
}

# =============================================================================
# ANALYSIS DATA
# =============================================================================

dat_weekday <- df_clean |>
  mutate(
    year_month = factor(format(date, "%Y-%m")),
    insomnia_any = as.integer(insomnia_num > 0)
  ) |>
  select(
    date,
    year_month,
    day_of_week,
    duration,
    insomnia_num,
    insomnia_any,
    bedtime,
    coffee,
    stress,
    health,
    exercise
  ) |>
  drop_na(day_of_week, duration, insomnia_num)

n_total <- nrow(dat_weekday)

cat("\n========== WEEKDAY ANALYSIS SAMPLE ==========\n")
cat("Observations:", n_total, "\n")
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
    share = n / nrow(dat_weekday),
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

cat("\n========== SLEEP DURATION BY WEEKDAY ==========\n")
print(weekday_summary, n = Inf, width = Inf)

write_csv(weekday_summary, file.path(output_dir, "weekday_summary.csv"))

# =============================================================================
# DESCRIPTIVE VISUALIZATIONS
# =============================================================================

p_distribution <- weekday_summary |>
  ggplot(aes(x = day_of_week, y = share, fill = day_of_week)) +
  geom_col(alpha = 0.85, width = 0.72) +
  geom_text(
    aes(label = distribution_label),
    vjust = -0.35,
    size = 3,
    color = col_dark_text
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, max(weekday_summary$share) + 0.06)
  ) +
  scale_fill_manual(
    values = make_palette(n_distinct(dat_weekday$day_of_week)),
    guide = "none"
  ) +
  labs(
    title = "The sleep diary covers weekdays fairly evenly",
    subtitle = paste0("Share of observed nights (N = ", n_total, ")"),
    x = NULL,
    y = "Share of nights"
  ) +
  theme_sleep()

p_duration <- dat_weekday |>
  ggplot(aes(x = day_of_week, y = duration, fill = day_of_week)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.62) +
  geom_jitter(width = 0.12, alpha = 0.08, size = 1.05, color = col_dark_text) +
  scale_fill_manual(
    values = make_palette(n_distinct(dat_weekday$day_of_week)),
    guide = "none"
  ) +
  geom_label(
    data = weekday_summary,
    aes(x = day_of_week, y = 9.4, label = median_label),
    inherit.aes = FALSE,
    size = 2.5,
    label.size = 0.12,
    fill = "white",
    color = col_dark_text
  ) +
  labs(
    title = "Sleep duration varies modestly across weekdays",
    subtitle = paste0("Boxplots, individual nights, and median sleep durations (N = ", n_total, ")"),
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
    title = "Average sleep duration by weekday",
    subtitle = "Means with approximate 95% confidence intervals",
    x = NULL,
    y = "Mean sleep duration (hours)"
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_insomnia <- weekday_summary |>
  ggplot(aes(x = day_of_week, y = insomnia_rate, group = 1)) +
  geom_errorbar(
    aes(ymin = insomnia_ci_low, ymax = insomnia_ci_high),
    width = 0.12,
    color = col_dark_blue,
    alpha = 0.8
  ) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 3, color = col_orange) +
  geom_text(
    aes(label = insomnia_label),
    vjust = -0.9,
    size = 3,
    color = col_dark_text
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, min(1, max(weekday_summary$insomnia_ci_high, na.rm = TRUE) + 0.08))
  ) +
  labs(
    title = "Insomnia rates differ by weekday",
    subtitle = "Share of nights with any recorded insomnia; approximate 95% confidence intervals",
    x = NULL,
    y = "Insomnia rate"
  ) +
  theme_sleep()

# =============================================================================
# MODEL DATA
# =============================================================================

reference_day <- pick_reference(dat_weekday$day_of_week, c("Mon", "Monday"))

dat_model <- dat_weekday |>
  mutate(
    day_of_week = fct_relevel(day_of_week, reference_day),
    bedtime = factor(bedtime, levels = levels(bedtime), ordered = FALSE),
    coffee = factor(coffee, levels = levels(coffee), ordered = FALSE),
    stress = factor(stress, levels = levels(stress), ordered = FALSE),
    health = factor(health, levels = levels(health), ordered = FALSE),
    exercise = factor(exercise, levels = levels(exercise), ordered = FALSE),
    year_month = fct_drop(year_month)
  ) |>
  drop_na(bedtime, coffee, stress, health, exercise, year_month)

reference_bedtime <- pick_reference(dat_model$bedtime, c("Before 23:00"))
reference_coffee <- pick_reference(dat_model$coffee, c("None"))
reference_stress <- pick_reference(dat_model$stress, c("No"))
reference_health <- pick_reference(dat_model$health, c("Healthy"))
reference_exercise <- pick_reference(dat_model$exercise, c("None"))

cat("\n========== WEEKDAY MODELLING SAMPLE ==========\n")
cat("Observations:", nrow(dat_model), "\n")
cat("Reference weekday:", reference_day, "\n")
cat("Reference bedtime:", reference_bedtime, "\n")
cat("Reference coffee:", reference_coffee, "\n")
cat("Reference stress:", reference_stress, "\n")
cat("Reference health:", reference_health, "\n")
cat("Reference exercise:", reference_exercise, "\n")

# =============================================================================
# SLEEP DURATION MODELS
# =============================================================================

models_duration <- list(
  "Raw" = feols(
    duration ~ i(day_of_week, ref = reference_day),
    data = dat_weekday,
    vcov = "hetero"
  ),
  "Adjusted" = feols(
    duration ~
      i(day_of_week, ref = reference_day) +
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(exercise, ref = reference_exercise),
    data = dat_model,
    vcov = "hetero"
  ),
  "Month FE" = feols(
    duration ~
      i(day_of_week, ref = reference_day) +
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(exercise, ref = reference_exercise) |
      year_month,
    data = dat_model,
    vcov = "hetero"
  )
)

purrr::iwalk(
  models_duration,
  \(model, model_name) {
    cat("\n==========", toupper(model_name), "WEEKDAY MODEL: SLEEP DURATION ==========\n")
    print(summary(model))
  }
)

duration_model_comparison <- tibble(
  model = names(models_duration),
  n = purrr::map_int(models_duration, nobs),
  rmse = purrr::map_dbl(models_duration, \(model) sqrt(mean(resid(model)^2)))
) |>
  mutate(rmse = round(rmse, 3))

cat("\n========== SLEEP DURATION MODEL COMPARISON ==========\n")
print(duration_model_comparison, n = Inf, width = Inf)

write_csv(duration_model_comparison, file.path(output_dir, "weekday_duration_model_comparison.csv"))

# =============================================================================
# INSOMNIA MODELS
# =============================================================================

models_insomnia <- list(
  "Raw" = safe_feglm(
    insomnia_any ~ i(day_of_week, ref = reference_day),
    data = dat_model,
    model_name = "Raw"
  ),
  "Adjusted" = safe_feglm(
    insomnia_any ~
      i(day_of_week, ref = reference_day) +
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(exercise, ref = reference_exercise),
    data = dat_model,
    model_name = "Adjusted"
  ),
  "Month FE" = safe_feglm(
    insomnia_any ~
      i(day_of_week, ref = reference_day) +
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(exercise, ref = reference_exercise) |
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
      cat("\n==========", toupper(model_name), "WEEKDAY MODEL: INSOMNIA ==========\n")
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

  write_csv(insomnia_model_comparison, file.path(output_dir, "weekday_insomnia_model_comparison.csv"))
}

# =============================================================================
# MODEL RESULTS FOR FIGURES
# =============================================================================

clean_weekday_term <- function(x) {
  x |>
    str_remove_all("`") |>
    str_remove("^day_of_week::")
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
        filter(str_detect(term, "^day_of_week::")) |>
        transmute(
          model = model_name,
          weekday = clean_weekday_term(term),
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

weekday_duration_results <- get_duration_results(models_duration) |>
  mutate(
    model = factor(model, levels = c("Raw", "Adjusted", "Month FE")),
    weekday = factor(weekday, levels = rev(setdiff(levels(dat_model$day_of_week), reference_day)))
  )

write_csv(weekday_duration_results, file.path(output_dir, "weekday_duration_coefficients.csv"))

month_fe_duration_results <- weekday_duration_results |>
  filter(model == "Month FE")

max_abs_month_fe <- month_fe_duration_results |>
  summarise(max_abs = max(abs(estimate_minutes), na.rm = TRUE)) |>
  pull(max_abs)

p_duration_coef_main <- month_fe_duration_results |>
  ggplot(aes(y = weekday, x = estimate_minutes, xmin = ci_low_minutes, xmax = ci_high_minutes)) +
  geom_linerange(linewidth = 1.2, color = col_dark_blue, alpha = 0.9) +
  geom_point(size = 2.5, color = col_orange) +
  geom_label(
    data = month_fe_duration_results,
    aes(y = weekday, x = ci_high_minutes + 2, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    label.size = 0.15,
    fill = "white",
    color = col_dark_text
  ) +
  geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") +
  scale_x_continuous(
    labels = \(x) paste0(round(x), " min"),
    breaks = scales::breaks_pretty(n = 6),
    expand = expansion(mult = c(0.03, 0.22))
  ) +
  labs(
    title = paste0("Largest adjusted weekday difference is about ", round(max_abs_month_fe), " minutes"),
    subtitle = paste0("Month fixed-effect estimates relative to ", reference_day, "; negative values mean shorter sleep"),
    x = "Difference in sleep duration (minutes)",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

p_duration_model_comparison <- weekday_duration_results |>
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
    linewidth = 1.1,
    alpha = 0.75,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(
    size = 2.1,
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
    title = "Model comparison for weekday sleep-duration differences",
    subtitle = paste0("Estimates relative to ", reference_day, "; negative values indicate shorter sleep"),
    x = "Difference in sleep duration",
    y = NULL,
    color = NULL
  ) +
  theme_sleep() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(color = "grey90")
  )

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
          filter(str_detect(term, "^day_of_week::")) |>
          transmute(
            model = model_name,
            weekday = clean_weekday_term(term),
            odds_ratio = exp(estimate),
            ci_low = exp(estimate - 1.96 * std_error),
            ci_high = exp(estimate + 1.96 * std_error)
          )
      }
    )
  }

  weekday_insomnia_results <- get_insomnia_results(models_insomnia) |>
    mutate(
      model = factor(model, levels = c("Raw", "Adjusted", "Month FE")),
      weekday = factor(weekday, levels = rev(setdiff(levels(dat_model$day_of_week), reference_day)))
    )

  write_csv(weekday_insomnia_results, file.path(output_dir, "weekday_insomnia_odds_ratios.csv"))

  p_insomnia_model_comparison <- weekday_insomnia_results |>
    ggplot(
      aes(
        y = weekday,
        x = odds_ratio,
        xmin = ci_low,
        xmax = ci_high,
        color = model
      )
    ) +
    geom_vline(xintercept = 1, linewidth = 0.3, linetype = "dashed") +
    geom_linerange(
      linewidth = 1.1,
      alpha = 0.75,
      position = position_dodge(width = 0.55)
    ) +
    geom_point(
      size = 2.1,
      position = position_dodge(width = 0.55)
    ) +
    scale_x_log10(
      labels = scales::number_format(accuracy = 0.1),
      breaks = c(0.5, 1, 2, 4)
    ) +
    scale_color_manual(
      values = c(
        "Raw" = col_light_blue,
        "Adjusted" = col_steel,
        "Month FE" = col_orange
      ),
      na.translate = FALSE
    ) +
    labs(
      title = "Model comparison for weekday insomnia odds ratios",
      subtitle = paste0("Odds ratios relative to ", reference_day, "; values above 1 indicate higher odds"),
      x = "Odds ratio, log scale",
      y = NULL,
      color = NULL
    ) +
    theme_sleep() +
    theme(
      legend.position = "bottom",
      panel.grid.major.x = element_line(color = "grey90")
    )
}

# =============================================================================
# MAIN FIGURE AND SUPPORTING FIGURES
# =============================================================================

p_main <- (p_distribution + p_duration) / (p_duration_coef_main + p_insomnia) +
  plot_annotation(
    title = "Weekday differences in sleep are visible but modest",
    subtitle = "Sleep diary associations across weekdays; uncertainty intervals shown for model-based and rate estimates",
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

save_plot(p_main, "weekday_figure1_main.png", width = 14, height = 10)
save_plot(p_duration_model_comparison, "weekday_figureS1_duration_model_comparison.png", width = 12, height = 8)
if (exists("p_insomnia_model_comparison")) {
  print(p_insomnia_model_comparison)
  save_plot(p_insomnia_model_comparison, "weekday_figureS2_insomnia_model_comparison.png", width = 12, height = 8)
}
save_plot(p_distribution, "weekday_figureS3_distribution.png", width = 10, height = 6)
save_plot(p_duration, "weekday_figureS4_sleep_duration_boxplot.png", width = 10, height = 6)
save_plot(p_mean_ci, "weekday_figureS5_mean_sleep_ci.png", width = 10, height = 6)
save_plot(p_insomnia, "weekday_figureS6_insomnia_rate.png", width = 10, height = 6)

# Bedtime-consistent numbered aliases for core descriptive figures.
save_plot(p_distribution, "weekday_figureS4_distribution.png", width = 10, height = 6)
save_plot(p_duration, "weekday_figureS5_sleep_duration_boxplot.png", width = 10, height = 6)

# Backward-compatible file names
save_plot(p_main, "weekday_combined.png", width = 14, height = 10)
save_plot(p_duration_model_comparison, "weekday_regression_coefficients.png", width = 12, height = 8)
save_plot(p_duration_coef_main, "weekday_month_fe_duration_coefficients.png", width = 10, height = 6)
save_plot(p_insomnia, "weekday_insomnia_rate.png", width = 10, height = 6)

# =============================================================================
# KEY FINDINGS OUTPUT
# =============================================================================

key_findings_distribution <- weekday_summary |>
  transmute(
    section = "distribution",
    category = as.character(day_of_week),
    metric = "share_of_nights",
    value = share
  )

key_findings_median <- weekday_summary |>
  transmute(
    section = "sleep_duration",
    category = as.character(day_of_week),
    metric = "median_sleep_hours",
    value = median_sleep
  )

key_findings_insomnia <- weekday_summary |>
  transmute(
    section = "insomnia",
    category = as.character(day_of_week),
    metric = "observed_insomnia_rate",
    value = insomnia_rate
  )

key_findings_duration_model <- month_fe_duration_results |>
  transmute(
    section = "month_fe_duration_model",
    category = as.character(weekday),
    metric = "difference_minutes_relative_to_reference_day",
    value = estimate_minutes
  )

key_findings <- bind_rows(
  tibble(section = "sample", category = "all", metric = "total_nights", value = n_total),
  tibble(section = "reference", category = "duration_model", metric = "reference_day", value = NA_real_, note = reference_day),
  key_findings_distribution |> mutate(note = NA_character_),
  key_findings_median |> mutate(note = NA_character_),
  key_findings_insomnia |> mutate(note = NA_character_),
  key_findings_duration_model |> mutate(note = NA_character_)
) |>
  mutate(value = round(value, 3))

write_csv(key_findings, file.path(output_dir, "weekday_key_findings.csv"))

# =============================================================================
# REPORTING SUMMARY
# =============================================================================

cat("\n========== REPORTING SUMMARY ==========\n")
cat(
  "The script estimates raw, adjusted, and month fixed-effect weekday differences",
  "in sleep duration and insomnia relative to", reference_day, ".\n"
)
cat("Recommended main figure saved to:", file.path(figure_dir, "weekday_figure1_main.png"), "\n")
cat("Duration model-comparison figure saved to:", file.path(figure_dir, "weekday_figureS1_duration_model_comparison.png"), "\n")
cat("Key findings saved to:", file.path(output_dir, "weekday_key_findings.csv"), "\n")
cat("Tables saved to:", output_dir, "\n")
