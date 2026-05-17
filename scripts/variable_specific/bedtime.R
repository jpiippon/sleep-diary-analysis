# =============================================================================
# bedtime.R
#
# Purpose: Analyze the association between bedtime and sleep outcomes
#
# Research questions:
#   How often do I go to bed before 23:00, between 23:00 and 00:00, or after 00:00?
#   How is bedtime associated with sleep duration?
#   How is bedtime associated with the probability of recorded insomnia?
#
# Input:
#   df_clean from scripts/01_load_main_data.R
#
# Outputs:
#   - descriptive summaries printed to console
#   - variable-specific figures saved to figures/variable_specific/bedtime/
#   - model summaries and tables saved to outputs/variable_specific/bedtime/
#   - raw, adjusted, and month fixed-effect models for reporting
#
# Notes for interpretation:
#   - Bedtime is treated as a diary exposure variable.
#   - Sleep duration and any recorded insomnia are treated as outcomes.
#   - Results should be interpreted as associations, not causal effects.
#   - Month fixed effects compare nights within the same year-month period.
# =============================================================================

library(tidyverse)
library(broom)
library(fixest)
library(here)
library(patchwork)
library(zoo)

source(here("scripts", "01_load_main_data.R"))

if (!exists("df_clean")) {
  stop("df_clean not found. Run 01_load_main_data.R first.")
}

figure_dir <- here("figures", "variable_specific", "bedtime")
output_dir <- here("outputs", "variable_specific", "bedtime")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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

get_mode <- function(x) {
  x_no_na <- x[!is.na(x)]
  x_levels <- unique(x_no_na)
  x_levels[which.max(tabulate(match(x_no_na, x_levels)))]
}

pick_reference <- function(x, preferred) {
  c(intersect(preferred, levels(x)), levels(x)[1]) |>
    purrr::pluck(1)
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

# =============================================================================
# ANALYSIS DATA
# =============================================================================

dat_bedtime <- df_clean |>
  mutate(
    year_month = factor(format(date, "%Y-%m")),
    insomnia_any = as.integer(insomnia_num > 0),
    sleep_band = cut(
      duration,
      breaks = c(0, 6, 7, 8, Inf),
      labels = c("<6 h", "6-7 h", "7-8 h", "8+ h"),
      right = FALSE
    )
  ) |>
  select(
    date,
    year_month,
    day_of_week,
    duration,
    sleep_band,
    insomnia_num,
    insomnia_any,
    bedtime_code,
    bedtime,
    coffee,
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
    insomnia_rate = mean(insomnia_any == 1, na.rm = TRUE),
    short_sleep_rate = mean(duration < 6, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(
        share, mean_sleep, median_sleep, sd_sleep, se_sleep, ci_low, ci_high,
        insomnia_rate, short_sleep_rate
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

write_csv(bedtime_summary, file.path(output_dir, "bedtime_summary.csv"))
write_csv(weekday_bedtime_summary, file.path(output_dir, "bedtime_by_weekday.csv"))
write_csv(monthly_bedtime_summary, file.path(output_dir, "bedtime_by_month.csv"))
write_csv(yearly_bedtime_summary, file.path(output_dir, "bedtime_by_year.csv"))

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

p_duration <- dat_bedtime |>
  ggplot(aes(x = bedtime, y = duration, fill = bedtime)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA) +
  geom_jitter(width = 0.14, alpha = 0.05, size = 0.85, color = col_dark_text) +
  geom_label(
    data = bedtime_summary,
    aes(x = bedtime, y = 9.2, label = paste0("Median: ", median_sleep, " h")),
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
  coord_cartesian(ylim = c(0, 11.2), clip = "off") +
  theme_sleep()

p_mean_ci <- bedtime_summary |>
  ggplot(aes(x = bedtime, y = mean_sleep, group = 1)) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.12,
    color = col_dark_blue,
    alpha = 0.8
  ) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 3, color = col_orange) +
  labs(
    title = "Mean sleep duration by bedtime",
    subtitle = "Means with approximate 95% confidence intervals",
    x = NULL,
    y = "Mean sleep duration (hours)"
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_insomnia <- bedtime_summary |>
  ggplot(aes(x = bedtime, y = insomnia_rate, group = 1)) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 3, color = col_orange) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Insomnia rate by bedtime",
    subtitle = "Share of nights with any recorded insomnia",
    x = NULL,
    y = "Insomnia rate"
  ) +
  theme_sleep()

p_short_sleep <- bedtime_summary |>
  ggplot(aes(x = bedtime, y = short_sleep_rate, group = 1)) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 3, color = col_orange) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Short sleep by bedtime",
    subtitle = "Share of nights with sleep duration below 6 hours",
    x = NULL,
    y = "Short sleep rate"
  ) +
  theme_sleep()

p_weekday_composition <- weekday_bedtime_summary |>
  ggplot(aes(x = day_of_week, y = share, fill = bedtime)) +
  geom_col(alpha = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = make_palette(n_distinct(dat_bedtime$bedtime))) +
  labs(
    title = "Bedtime composition by weekday",
    subtitle = "Share of nights in each bedtime category",
    x = NULL,
    y = "Share of nights",
    fill = NULL
  ) +
  theme_sleep()

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
  drop_na(bedtime, duration, insomnia_any, coffee, stress, health, exercise, day_of_week, year_month)

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
    vcov = "hetero"
  ),
  "Adjusted" = feols(
    duration ~
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(exercise, ref = reference_exercise) +
      i(day_of_week, ref = reference_day),
    data = dat_model,
    vcov = "hetero"
  ),
  "Month FE" = feols(
    duration ~
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(exercise, ref = reference_exercise) +
      i(day_of_week, ref = reference_day) |
      year_month,
    data = dat_model,
    vcov = "hetero"
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

write_csv(duration_model_comparison, file.path(output_dir, "bedtime_duration_model_comparison.csv"))

# =============================================================================
# INSOMNIA MODELS
# =============================================================================

models_insomnia <- list(
  "Raw" = safe_feglm(
    insomnia_any ~ i(bedtime, ref = reference_bedtime),
    data = dat_model,
    model_name = "Raw"
  ),
  "Adjusted" = safe_feglm(
    insomnia_any ~
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(exercise, ref = reference_exercise) +
      i(day_of_week, ref = reference_day),
    data = dat_model,
    model_name = "Adjusted"
  ),
  "Month FE" = safe_feglm(
    insomnia_any ~
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(exercise, ref = reference_exercise) +
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
      cat("\n==========", toupper(model_name), "BEDTIME MODEL: INSOMNIA ==========\n")
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

  write_csv(insomnia_model_comparison, file.path(output_dir, "bedtime_insomnia_model_comparison.csv"))
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
    model = factor(model, levels = c("Raw", "Adjusted", "Month FE")),
    bedtime = factor(bedtime, levels = rev(levels(dat_model$bedtime)))
  )

month_fe_duration_results <- bedtime_duration_results |>
  filter(model == "Month FE") |>
  mutate(
    duration_label = paste0(if_else(estimate_minutes >= 0, "+", ""), round(estimate_minutes), " min")
  )

write_csv(bedtime_duration_results, file.path(output_dir, "bedtime_duration_coefficients.csv"))

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
      "Adjusted" = col_steel,
      "Month FE" = col_orange
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
          filter(str_detect(term, "^bedtime::")) |>
          transmute(
            model = model_name,
            bedtime = clean_bedtime_term(term),
            odds_ratio = exp(estimate),
            ci_low = exp(estimate - 1.96 * std_error),
            ci_high = exp(estimate + 1.96 * std_error)
          )
      }
    )
  }

  bedtime_insomnia_results <- get_insomnia_results(models_insomnia) |>
    mutate(
      model = factor(model, levels = c("Raw", "Adjusted", "Month FE")),
      bedtime = factor(bedtime, levels = rev(levels(dat_model$bedtime)))
    )

  write_csv(bedtime_insomnia_results, file.path(output_dir, "bedtime_insomnia_odds_ratios.csv"))

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
        "Adjusted" = col_steel,
        "Month FE" = col_orange
      ),
      na.translate = FALSE
    ) +
    labs(
      title = "Model comparison for insomnia odds ratios",
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
    c("bedtime_figureS2_insomnia_model_comparison.png", "bedtime_insomnia_odds_ratios.png"),
    width = 10,
    height = 6
  )
}

# =============================================================================
# ADJUSTED PREDICTIONS
# =============================================================================

prediction_grid <- tibble(bedtime = levels(dat_model$bedtime)) |>
  mutate(
    bedtime = factor(bedtime, levels = levels(dat_model$bedtime)),
    coffee = factor(get_mode(dat_model$coffee), levels = levels(dat_model$coffee)),
    stress = factor(get_mode(dat_model$stress), levels = levels(dat_model$stress)),
    health = factor(get_mode(dat_model$health), levels = levels(dat_model$health)),
    exercise = factor(get_mode(dat_model$exercise), levels = levels(dat_model$exercise)),
    day_of_week = factor(get_mode(dat_model$day_of_week), levels = levels(dat_model$day_of_week)),
    year_month = factor(get_mode(dat_model$year_month), levels = levels(dat_model$year_month))
  )

preferred_duration_model_name <- "Month FE"
preferred_duration_model <- models_duration[[preferred_duration_model_name]]

pred_duration <- prediction_grid |>
  mutate(
    predicted_duration = predict(
      preferred_duration_model,
      newdata = as.data.frame(pick(everything()))
    ) |>
      as.numeric(),
    model = preferred_duration_model_name
  ) |>
  select(bedtime, predicted_duration, model)

write_csv(pred_duration, file.path(output_dir, "bedtime_predicted_sleep_duration.csv"))

p_insomnia_main <- p_insomnia
pred_insomnia <- NULL

if (length(models_insomnia) > 0) {
  preferred_insomnia_model_name <- c(
    intersect("Month FE", names(models_insomnia)),
    intersect("Adjusted", names(models_insomnia)),
    names(models_insomnia)[1]
  ) |>
    purrr::pluck(1)

  preferred_insomnia_model <- models_insomnia[[preferred_insomnia_model_name]]

  insomnia_pred_link <- tryCatch(
    predict(
      preferred_insomnia_model,
      newdata = as.data.frame(prediction_grid),
      type = "link"
    ),
    error = \(e) {
      warning("Insomnia prediction failed: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(insomnia_pred_link)) {
    insomnia_design <- model.matrix(
      ~ bedtime + coffee + stress + health + exercise + day_of_week,
      data = prediction_grid
    )
    colnames(insomnia_design) <- colnames(insomnia_design) |>
      str_replace("^bedtime", "bedtime::") |>
      str_replace("^coffee", "coffee::") |>
      str_replace("^stress", "stress::") |>
      str_replace("^health", "health::") |>
      str_replace("^exercise", "exercise::") |>
      str_replace("^day_of_week", "day_of_week::")

    insomnia_beta <- coef(preferred_insomnia_model)
    insomnia_vcov <- vcov(preferred_insomnia_model)

    shared_terms <- intersect(colnames(insomnia_design), names(insomnia_beta))

    if (length(shared_terms) > 0) {
      insomnia_design <- insomnia_design[, shared_terms, drop = FALSE]
      insomnia_beta <- insomnia_beta[shared_terms]
      insomnia_vcov <- insomnia_vcov[shared_terms, shared_terms, drop = FALSE]

      insomnia_link <- as.numeric(insomnia_pred_link)
      insomnia_se_link <- sqrt(
        rowSums((insomnia_design %*% insomnia_vcov) * insomnia_design)
      )

      pred_insomnia <- tibble(
        bedtime = prediction_grid$bedtime,
        predicted_insomnia = plogis(insomnia_link),
        ci_low = plogis(insomnia_link - 1.96 * insomnia_se_link),
        ci_high = plogis(insomnia_link + 1.96 * insomnia_se_link),
        model = preferred_insomnia_model_name
      ) |>
        select(
          bedtime,
          predicted_insomnia,
          ci_low,
          ci_high,
          model
        )
    } else {
      pred_insomnia <- NULL
    }
  } else {
    pred_insomnia <- NULL
  }

  if (!is.null(pred_insomnia)) {
    p_insomnia_main <- pred_insomnia |>
      ggplot(aes(x = bedtime, y = predicted_insomnia)) +
      geom_linerange(aes(ymin = ci_low, ymax = ci_high), linewidth = 0.9, color = col_dark_blue) +
      geom_point(size = 2.7, color = col_orange) +
      geom_text(
        aes(label = scales::percent(predicted_insomnia, accuracy = 1)),
        vjust = -1.1,
        nudge_x = 0.18,
        color = col_dark_text,
        size = 3.1,
        fontface = "bold"
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = "Insomnia is more common after later bedtimes",
        subtitle = "Model-based predicted probabilities; uncertainty remains substantial",
        x = NULL,
        y = "Predicted probability"
      ) +
      coord_cartesian(clip = "off") +
      theme_sleep()
  } else {
    pred_insomnia <- prediction_grid |>
      mutate(
        predicted_insomnia = tryCatch(
          predict(
            preferred_insomnia_model,
            newdata = as.data.frame(pick(everything())),
            type = "response"
          ) |>
            as.numeric(),
          error = \(e) {
            warning("Insomnia prediction failed: ", conditionMessage(e))
            rep(NA_real_, nrow(prediction_grid))
          }
        ),
        ci_low = NA_real_,
        ci_high = NA_real_,
        model = preferred_insomnia_model_name
      ) |>
      select(bedtime, predicted_insomnia, ci_low, ci_high, model)

    p_insomnia_main <- pred_insomnia |>
      ggplot(aes(x = bedtime, y = predicted_insomnia)) +
      geom_point(size = 2.7, color = col_orange) +
      geom_text(
        aes(label = scales::percent(predicted_insomnia, accuracy = 1)),
        vjust = -0.8,
        nudge_x = 0.18,
        color = col_dark_text,
        size = 3.1,
        fontface = "bold"
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = "Insomnia is more common after later bedtimes",
        subtitle = paste0(
          "Model-based predicted probabilities; uncertainty remains substantial",
          " (point estimates shown; confidence intervals unavailable)"
        ),
        x = NULL,
        y = "Predicted probability"
      ) +
      coord_cartesian(clip = "off") +
      theme_sleep()
  }

  write_csv(pred_insomnia, file.path(output_dir, "bedtime_predicted_insomnia.csv"))
}

p_duration_main <- bedtime_duration_results |>
  filter(model == "Month FE") |>
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
    data = month_fe_duration_results,
    aes(label = duration_label),
    nudge_x = 9,
    nudge_y = 0.12,
    color = col_dark_text,
    size = 3.1,
    fontface = "bold"
  ) +
  geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") +
  scale_x_continuous(
    labels = \(x) paste0(round(x), " min"),
    breaks = scales::breaks_pretty(n = 6)
  ) +
  labs(
    title = "After-midnight bedtimes show the largest adjusted sleep loss",
    subtitle = "Month fixed-effect estimates relative to before 23:00; negative values mean shorter sleep",
    x = "Difference in sleep duration (minutes)",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

top_row <- p_distribution + p_duration + plot_spacer() +
  plot_layout(widths = c(1, 1, 0.08))

bottom_row <- p_duration_main + p_insomnia_main + plot_spacer() +
  plot_layout(widths = c(1, 1, 0.08))

p_main <- top_row / bottom_row +
  plot_annotation(
    title = "Later bedtimes are associated with shorter sleep and more insomnia",
    subtitle = "Sleep diary associations across bedtime categories; uncertainty intervals shown for model-based estimates",
    tag_levels = "A"
  )

print(p_main)
save_plot_versions(
  p_main,
  c("bedtime_figure1_main.png"),
  width = 14,
  height = 10
)

# Compact reporting values for publication-facing summaries.
bedtime_share_map <- bedtime_summary |>
  transmute(
    bedtime,
    metric = case_when(
      bedtime == "Before 23:00" ~ "share_before_23_00",
      bedtime == "23:00-00:00" ~ "share_23_00_00_00",
      bedtime == "After 00:00" ~ "share_after_00_00",
      TRUE ~ paste0("share_", str_replace_all(str_to_lower(bedtime), "[^a-z0-9]+", "_"))
    ),
    value = share
  )

median_by_bedtime <- bedtime_summary |>
  transmute(
    metric = "median_sleep_duration_hours",
    category = bedtime,
    value = median_sleep
  )

duration_diff_by_bedtime <- tibble(
  bedtime = levels(dat_model$bedtime)
) |>
  left_join(
    month_fe_duration_results |>
      select(bedtime, estimate_minutes),
    by = "bedtime"
  ) |>
  mutate(
    estimate_minutes = replace_na(estimate_minutes, 0),
    metric = "month_fe_sleep_duration_difference_minutes",
    category = bedtime,
    value = round(estimate_minutes, 2)
  ) |>
  select(metric, category, value)

insomnia_prob_by_bedtime <- if (!is.null(pred_insomnia)) {
  pred_insomnia |>
    transmute(
      metric = "predicted_insomnia_probability",
      category = bedtime,
      value = round(predicted_insomnia, 4)
    )
} else {
  tibble(
    metric = "predicted_insomnia_probability",
    category = levels(dat_model$bedtime),
    value = NA_real_
  )
}

key_findings <- bind_rows(
  tibble(metric = "total_n", category = "all", value = nrow(dat_bedtime)),
  bedtime_share_map |>
    mutate(category = bedtime) |>
    select(metric, category, value),
  median_by_bedtime,
  duration_diff_by_bedtime,
  insomnia_prob_by_bedtime
)

write_csv(key_findings, file.path(output_dir, "bedtime_key_findings.csv"))

# =============================================================================
# REPORTING SUMMARY
# =============================================================================

cat("\n========== REPORTING SUMMARY ==========\n")
cat(
  "The script describes bedtime patterns and estimates raw, adjusted, and month",
  "fixed-effect models for sleep duration and any recorded insomnia.\n"
)
cat("Reference bedtime:", reference_bedtime, "\n")
cat("Main figure saved to:", file.path(figure_dir, "bedtime_figure1_main.png"), "\n")
cat("Supporting duration coefficient figure saved to:", file.path(figure_dir, "bedtime_duration_coefficients.png"), "\n")
cat("Supporting insomnia odds-ratio figure saved to:", file.path(figure_dir, "bedtime_insomnia_odds_ratios.png"), "\n")
cat("Supporting bedtime distribution figure saved to:", file.path(figure_dir, "bedtime_figureS4_distribution.png"), "\n")
cat("Supporting sleep-duration figure saved to:", file.path(figure_dir, "bedtime_figureS5_sleep_duration_boxplot.png"), "\n")
cat("Supporting bedtime-over-time figure saved to:", file.path(figure_dir, "bedtime_figureS3_over_time.png"), "\n")
cat("Tables saved to:", output_dir, "\n")
