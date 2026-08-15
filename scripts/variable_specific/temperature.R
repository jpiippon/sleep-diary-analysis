# =============================================================================
# temperature.R
#
# Purpose: Analyze the association between bedroom temperature and sleep outcomes
#
# Research questions:
#   How is bedroom temperature distributed across sensor nights?
#   Is high bedroom temperature, especially above 25 °C, associated with sleep duration?
#   Is high bedroom temperature associated with recorded insomnia?
#
# Input:
#   sleep_mittari_sensor from scripts/03_join_relevant_data.R
#
# Outputs:
#   - descriptive summaries printed to console
#   - numbered variable-specific figures saved to outputs/figures/variable_specific/temperature/
#   - raw, adjusted, and month fixed-effect models for reporting
#
# Notes for interpretation:
#   - Temperature is measured from bedroom sensor data and summarized as nightly mean temperature.
#   - The main categorical contrast highlights nights above 25 °C.
#   - Sleep duration and any recorded insomnia are treated as outcomes.
#   - Results should be interpreted as associations, not causal effects.
#   - Month fixed effects compare nights within the same year-month period.
# =============================================================================

library(tidyverse)
library(fixest)
library(here)
library(patchwork)

source(here("scripts", "03_join_relevant_data.R"))

if (!exists("sleep_mittari_sensor")) {
  stop("sleep_mittari_sensor not found. Run 03_join_relevant_data.R first.")
}

figure_dir <- here("outputs", "figures", "variable_specific", "temperature")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SETTINGS AND HELPERS
# =============================================================================

variable_name <- "ka_temp"
variable_label <- "Bedroom temperature"
outcome_name <- "duration"
outcome_label <- "Sleep duration (hours)"
variable_type <- "numeric sensor exposure"

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

dat_temperature <- sleep_mittari_sensor |>
  mutate(
    year = factor(format(date, "%Y")),
    year_month = factor(format(date, "%Y-%m")),
    insomnia_any = as.integer(insomnia_num > 0),
    temp_high25 = factor(
      if_else(ka_temp > 25, "Above 25 °C", "25 °C or below"),
      levels = c("25 °C or below", "Above 25 °C")
    ),
    temp_band = cut(
      ka_temp,
      breaks = c(-Inf, 20, 22, 24, 25, Inf),
      labels = c("<20 °C", "20-22 °C", "22-24 °C", "24-25 °C", ">25 °C"),
      right = TRUE
    )
  ) |>
  select(
    date,
    series_id,
    year,
    year_month,
    day_of_week,
    duration,
    insomnia_num,
    insomnia_any,
    bedtime,
    coffee,
    stress,
    health,
    exercise,
    n_obs,
    ka_temp,
    temp_high25,
    temp_band
  ) |>
  drop_na(ka_temp, duration, insomnia_num)

n_total <- nrow(dat_temperature)
high25_n <- sum(dat_temperature$ka_temp > 25, na.rm = TRUE)
high25_share <- mean(dat_temperature$ka_temp > 25, na.rm = TRUE)

cat("\n========== TEMPERATURE ANALYSIS SAMPLE ==========\n")
cat("Sensor nights:", n_total, "\n")
cat("Nights above 25 °C:", high25_n, "(", fmt_pct(high25_share, accuracy = 0.1), ")\n", sep = "")
cat(
  "Date range:", format(min(dat_temperature$date), "%Y-%m-%d"), "to",
  format(max(dat_temperature$date), "%Y-%m-%d"), "\n"
)
cat("\nBedroom temperature summary:\n")
print(summary(dat_temperature$ka_temp))

# =============================================================================
# DESCRIPTIVE SUMMARIES
# =============================================================================

temp_band_summary <- dat_temperature |>
  group_by(temp_band) |>
  summarise(
    n = n(),
    share = n / nrow(dat_temperature),
    mean_temp = mean(ka_temp, na.rm = TRUE),
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
        share, mean_temp, mean_sleep, median_sleep, sd_sleep, se_sleep, ci_low, ci_high,
        insomnia_rate, insomnia_se, insomnia_ci_low, insomnia_ci_high
      ),
      \(x) round(x, 3)
    )
  )

temp_high_summary <- dat_temperature |>
  group_by(temp_high25) |>
  summarise(
    n = n(),
    share = n / nrow(dat_temperature),
    mean_temp = mean(ka_temp, na.rm = TRUE),
    mean_sleep = mean(duration, na.rm = TRUE),
    median_sleep = median(duration, na.rm = TRUE),
    insomnia_n = sum(insomnia_any == 1, na.rm = TRUE),
    insomnia_rate = mean(insomnia_any == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(c(share, mean_temp, mean_sleep, median_sleep, insomnia_rate), \(x) round(x, 3)))

yearly_temperature_summary <- dat_temperature |>
  group_by(year) |>
  summarise(
    n = n(),
    mean_temp = mean(ka_temp, na.rm = TRUE),
    share_above_25 = mean(ka_temp > 25, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    n_label = paste0("n=", n),
    across(c(mean_temp, share_above_25), \(x) round(x, 3))
  )

cat("\n========== SLEEP OUTCOMES BY TEMPERATURE BAND ==========\n")
print(temp_band_summary, n = Inf, width = Inf)

cat("\n========== SLEEP OUTCOMES ABOVE VS BELOW 25 °C ==========\n")
print(temp_high_summary, n = Inf, width = Inf)

cat("\n========== TEMPERATURE BY YEAR ==========\n")
print(yearly_temperature_summary, n = Inf, width = Inf)

# =============================================================================
# DESCRIPTIVE VISUALIZATIONS
# =============================================================================

temp_palette <- make_palette(n_distinct(dat_temperature$temp_band))
label_y <- quantile(dat_temperature$duration, 0.97, na.rm = TRUE)
max_share <- max(temp_band_summary$share, na.rm = TRUE)

p_distribution <- temp_band_summary |>
  ggplot(aes(x = temp_band, y = share, fill = temp_band)) +
  geom_col(alpha = 0.85, width = 0.72) +
  geom_text(aes(label = distribution_label), vjust = -0.35, size = 3.1, color = col_dark_text) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max_share + 0.12)) +
  scale_fill_manual(values = temp_palette, guide = "none") +
  labs(
    title = paste0(fmt_pct(high25_share, accuracy = 1), " of sensor nights are above 25 °C"),
    subtitle = paste0("Nightly mean bedroom temperature bands (N = ", n_total, " sensor nights)"),
    x = NULL,
    y = "Share of sensor nights"
  ) +
  theme_sleep()

p_duration <- dat_temperature |>
  ggplot(aes(x = temp_band, y = duration, fill = temp_band)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.62) +
  geom_jitter(width = 0.12, alpha = 0.10, size = 1.05, color = col_dark_text) +
  geom_label(
    data = temp_band_summary,
    aes(x = temp_band, y = label_y, label = median_label),
    inherit.aes = FALSE,
    size = 2.6,
    label.size = 0.12,
    fill = "white",
    color = col_dark_text
  ) +
  scale_fill_manual(values = temp_palette, guide = "none") +
  labs(
    title = "Sleep duration can be compared across temperature bands",
    subtitle = paste0("Boxplots, individual nights, and median sleep durations (N = ", n_total, ")"),
    x = NULL,
    y = outcome_label
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_scatter <- dat_temperature |>
  ggplot(aes(x = ka_temp, y = duration)) +
  geom_point(alpha = 0.18, size = 1.4, color = col_dark_text) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, linewidth = 1.0, color = col_navy) +
  geom_vline(xintercept = 25, linewidth = 0.35, linetype = "dashed", color = col_orange) +
  annotate("text", x = 25, y = Inf, label = "25 °C", vjust = 1.5, hjust = -0.1, color = col_orange, size = 3.2) +
  labs(
    title = "The 25 °C threshold is a useful visual reference point",
    subtitle = "Each point is a sensor night; smooth line is descriptive",
    x = "Nightly mean bedroom temperature (°C)",
    y = outcome_label
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

p_insomnia <- temp_band_summary |>
  ggplot(aes(x = temp_band, y = insomnia_rate, group = 1)) +
  geom_errorbar(aes(ymin = insomnia_ci_low, ymax = insomnia_ci_high), width = 0.12, color = col_dark_blue, alpha = 0.8) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 3, color = col_orange) +
  geom_text(aes(label = insomnia_label), vjust = -0.9, size = 3.0, color = col_dark_text) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, min(1, max(temp_band_summary$insomnia_ci_high, na.rm = TRUE) + 0.08))
  ) +
  labs(
    title = "Insomnia rates can be compared across temperature bands",
    subtitle = "Share of nights with any recorded insomnia; approximate 95% confidence intervals",
    x = NULL,
    y = "Insomnia rate"
  ) +
  theme_sleep()

p_over_time <- yearly_temperature_summary |>
  ggplot(aes(x = year, y = share_above_25)) +
  geom_col(fill = col_steel, alpha = 0.85, width = 0.72) +
  geom_text(aes(y = share_above_25 + 0.04, label = n_label), size = 2.8, color = col_dark_text) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, min(1, max(yearly_temperature_summary$share_above_25, na.rm = TRUE) + 0.16))
  ) +
  labs(
    title = "Hot sensor nights vary across years",
    subtitle = "Share of sensor nights above 25 °C; labels show sensor nights per year",
    x = NULL,
    y = "Share above 25 °C"
  ) +
  theme_sleep()

# =============================================================================
# MODEL DATA
# =============================================================================

dat_model <- dat_temperature |>
  mutate(
    temp_high25 = factor(
      if_else(ka_temp > 25, "Above 25C", "25C or below"),
      levels = c("25C or below", "Above 25C")
    ),
    temp_band = fct_drop(temp_band),
    bedtime = factor(bedtime, levels = levels(bedtime), ordered = FALSE),
    coffee = factor(coffee, levels = levels(coffee), ordered = FALSE),
    stress = factor(stress, levels = levels(stress), ordered = FALSE),
    health = factor(health, levels = levels(health), ordered = FALSE),
    exercise = factor(exercise, levels = levels(exercise), ordered = FALSE),
    day_of_week = fct_drop(day_of_week),
    year_month = fct_drop(year_month)
  ) |>
  drop_na(
    ka_temp,
    temp_high25,
    duration,
    insomnia_any,
    bedtime,
    coffee,
    stress,
    health,
    exercise,
    day_of_week,
    year_month
  ) |>
  prepare_nw_data()

reference_temp <- pick_reference(dat_model$temp_high25, "25C or below")
reference_bedtime <- pick_reference(dat_model$bedtime, "Before 23:00")
reference_coffee <- pick_reference(dat_model$coffee, "None")
reference_stress <- pick_reference(dat_model$stress, "No")
reference_health <- pick_reference(dat_model$health, "Healthy")
reference_exercise <- pick_reference(dat_model$exercise, "None")
reference_day <- pick_reference(dat_model$day_of_week, c("Mon", "Monday"))

cat("\n========== TEMPERATURE MODELLING SAMPLE ==========\n")
cat("Observations:", nrow(dat_model), "\n")
cat("Reference temperature group:", reference_temp, "\n")
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
    duration ~ i(temp_high25, ref = reference_temp),
    data = dat_model,
    vcov = NW(7) ~ series_id + date
  ),
  "Adjusted" = feols(
    duration ~
      i(temp_high25, ref = reference_temp) +
      i(bedtime, ref = reference_bedtime) +
      i(coffee, ref = reference_coffee) +
      i(stress, ref = reference_stress) +
      i(health, ref = reference_health) +
      i(exercise, ref = reference_exercise) +
      i(day_of_week, ref = reference_day),
    data = dat_model,
    vcov = NW(7) ~ series_id + date
  ),
  "Month FE" = feols(
    duration ~
      i(temp_high25, ref = reference_temp) +
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
    cat("\n==========", toupper(model_name), "TEMPERATURE MODEL: SLEEP DURATION ==========\n")
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
    insomnia_any ~ i(temp_high25, ref = reference_temp),
    data = dat_model,
    model_name = "Raw"
  ),
  "Adjusted" = safe_feglm(
    insomnia_any ~
      i(temp_high25, ref = reference_temp) +
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
      i(temp_high25, ref = reference_temp) +
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
      cat("\n==========", toupper(model_name), "TEMPERATURE MODEL: INSOMNIA ==========\n")
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

clean_temp_term <- function(x) {
  x |>
    str_remove_all("`") |>
    str_remove("^temp_high25::")
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
        filter(str_detect(term, "^temp_high25::")) |>
        transmute(
          model = model_name,
          temperature = clean_temp_term(term),
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

temperature_duration_results <- get_duration_results(models_duration) |>
  mutate(model = factor(model, levels = c("Raw", "Adjusted", "Month FE")))

cat("\n========== TEMPERATURE DURATION COEFFICIENTS ==========\n")
print(temperature_duration_results, n = Inf, width = Inf)

month_fe_duration_results <- temperature_duration_results |>
  filter(model == "Month FE")

p_duration_coef_main <- month_fe_duration_results |>
  ggplot(aes(y = temperature, x = estimate_minutes, xmin = ci_low_minutes, xmax = ci_high_minutes)) +
  geom_linerange(linewidth = 1.2, color = col_dark_blue, alpha = 0.9) +
  geom_point(size = 2.5, color = col_orange) +
  geom_label(
    aes(x = ci_high_minutes + 3, label = label),
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
    expand = expansion(mult = c(0.05, 0.28))
  ) +
  labs(
    title = "The main model compares nights above 25 °C with cooler nights",
    subtitle = paste0("Month fixed-effect estimate relative to ", reference_temp, "; negative values mean shorter sleep"),
    x = "Difference in sleep duration (minutes)",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

p_duration_model_comparison <- temperature_duration_results |>
  ggplot(aes(y = temperature, x = estimate_minutes, xmin = ci_low_minutes, xmax = ci_high_minutes, color = model)) +
  geom_linerange(linewidth = 1.1, alpha = 0.75, position = position_dodge(width = 0.55)) +
  geom_point(size = 2.1, position = position_dodge(width = 0.55)) +
  geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") +
  scale_color_manual(values = c("Raw" = col_light_blue, "Adjusted" = col_steel, "Month FE" = col_orange)) +
  scale_x_continuous(labels = \(x) paste0(round(x), " min"), breaks = scales::breaks_pretty(n = 6)) +
  labs(
    title = "Model comparison for high-temperature sleep-duration differences",
    subtitle = paste0("Estimates relative to ", reference_temp, "; negative values indicate shorter sleep"),
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
          filter(str_detect(term, "^temp_high25::")) |>
          transmute(
            model = model_name,
            temperature = clean_temp_term(term),
            odds_ratio = exp(estimate),
            ci_low = exp(estimate - 1.96 * std_error),
            ci_high = exp(estimate + 1.96 * std_error)
          )
      }
    )
  }

  temperature_insomnia_results <- get_insomnia_results(models_insomnia) |>
    mutate(model = factor(model, levels = c("Raw", "Adjusted", "Month FE")))

  cat("\n========== TEMPERATURE INSOMNIA ODDS RATIOS ==========\n")
  print(temperature_insomnia_results, n = Inf, width = Inf)

  p_insomnia_model_comparison <- temperature_insomnia_results |>
    ggplot(aes(y = temperature, x = odds_ratio, xmin = ci_low, xmax = ci_high, color = model)) +
    geom_vline(xintercept = 1, linewidth = 0.3, linetype = "dashed") +
    geom_linerange(linewidth = 1.1, alpha = 0.75, position = position_dodge(width = 0.55)) +
    geom_point(size = 2.1, position = position_dodge(width = 0.55)) +
    scale_x_log10(labels = scales::number_format(accuracy = 0.1), breaks = c(0.5, 1, 2, 4)) +
    scale_color_manual(values = c("Raw" = col_light_blue, "Adjusted" = col_steel, "Month FE" = col_orange), na.translate = FALSE) +
    labs(
      title = "Model comparison for high-temperature insomnia odds ratios",
      subtitle = paste0("Odds ratios relative to ", reference_temp, "; values above 1 indicate higher odds"),
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
    title = "Hot bedroom nights can be evaluated with a 25 °C threshold",
    subtitle = "Sensor-night associations between bedroom temperature, sleep duration, and insomnia",
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(size = 14, face = "bold"))

print(p_main)
print(p_distribution)
print(p_duration)
print(p_scatter)
print(p_duration_coef_main)
print(p_duration_model_comparison)
print(p_insomnia)
print(p_over_time)

save_plot(p_main, "temperature_figure1_main.png", width = 14, height = 10)
save_plot(p_duration_model_comparison, "temperature_figureS1_duration_model_comparison.png", width = 12, height = 6)
if (exists("p_insomnia_model_comparison")) {
  print(p_insomnia_model_comparison)
  save_plot(p_insomnia_model_comparison, "temperature_figureS2_insomnia_model_comparison.png", width = 12, height = 6)
}
save_plot(p_over_time, "temperature_figureS3_over_time.png", width = 12, height = 6)
save_plot(p_distribution, "temperature_figureS4_distribution.png", width = 8, height = 6)
save_plot(p_duration, "temperature_figureS5_sleep_duration_boxplot.png", width = 8, height = 6)
save_plot(p_scatter, "temperature_figureS6_sleep_duration_scatter.png", width = 9, height = 6)
save_plot(p_insomnia, "temperature_figureS7_insomnia_rate.png", width = 8, height = 6)

# Backward-compatible file names
save_plot(p_main, "temperature_overview.png", width = 14, height = 10)
save_plot(p_duration_model_comparison, "temperature_duration_coefficients.png", width = 12, height = 6)
save_plot(p_insomnia, "insomnia_by_temperature.png", width = 8, height = 6)
save_plot(p_duration, "sleep_duration_by_temperature.png", width = 8, height = 6)
save_plot(p_scatter, "sleep_duration_temperature_scatter.png", width = 9, height = 6)
save_plot(p_over_time, "temperature_over_time.png", width = 12, height = 6)

# =============================================================================
# REPORTING SUMMARY
# =============================================================================

cat("\n========== REPORTING SUMMARY ==========\n")
cat(
  "The script estimates raw, adjusted, and month fixed-effect differences for",
  "nights above 25 °C relative to cooler sensor nights.\n"
)
cat("Recommended main figure saved to:", file.path(figure_dir, "temperature_figure1_main.png"), "\n")
cat("Duration model-comparison figure saved to:", file.path(figure_dir, "temperature_figureS1_duration_model_comparison.png"), "\n")
cat("Other figures saved to:", figure_dir, "\n")
