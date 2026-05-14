# =============================================================================
# insomnia.R
#
# Purpose: Analyze insomnia patterns in the sleep diary data
#
# Research questions:
#   How common is recorded insomnia?
#   How does insomnia vary across time, weekdays, and behavioral factors?
#   Which observed factors are associated with the probability of insomnia?
#
# Input:
#   df_clean from scripts/01_load_main_data.R
#
# Outputs:
#   - descriptive summaries printed to console
#   - variable-specific figures saved to figures/variable_specific/insomnia/
#   - model summaries and tables saved to outputs/variable_specific/insomnia/
#   - raw, adjusted, and month fixed-effect logistic models for reporting
#
# Notes for adapting this script:
#   - Insomnia is treated as a binary outcome: any recorded insomnia vs none.
#   - The original insomnia code is preserved as insomnia_num and insomnia.
#   - Duration is used descriptively to show co-occurrence with sleep loss.
#   - Regression results should be interpreted as associations, not causal effects.
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

figure_dir <- here("figures", "variable_specific", "insomnia")
output_dir <- here("outputs", "variable_specific", "insomnia")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SETTINGS
# =============================================================================

variable_name <- "insomnia"
variable_label <- "Insomnia"
outcome_name <- "insomnia_any"
outcome_label <- "Any recorded insomnia"

# Variable type guidance:
#   insomnia_num is an ordered diary code, but this script primarily uses a
#   binary outcome to keep the interpretation suitable for reporting.
variable_type <- "binary outcome"

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
      mean_sleep = mean(duration, na.rm = TRUE),
      median_sleep = median(duration, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      across(
        c(insomnia_rate, se, ci_low, ci_high, mean_sleep, median_sleep),
        \(x) round(x, 3)
      )
    )
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

dat_insomnia <- df_clean |>
  mutate(
    year_month = factor(format(date, "%Y-%m")),
    insomnia_any = as.integer(insomnia_num > 0),
    insomnia_status = factor(
      if_else(insomnia_any == 1, "Any insomnia", "No insomnia"),
      levels = c("No insomnia", "Any insomnia")
    ),
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
    insomnia,
    insomnia_any,
    insomnia_status,
    bedtime,
    coffee,
    stress,
    health,
    exercise
  ) |>
  drop_na(insomnia_num, insomnia_any, duration)

cat("\n========== INSOMNIA ANALYSIS SAMPLE ==========\n")
cat("Observations:", nrow(dat_insomnia), "\n")
cat("Insomnia nights:", sum(dat_insomnia$insomnia_any == 1), "\n")
cat(
  "Insomnia rate:",
  scales::percent(mean(dat_insomnia$insomnia_any == 1), accuracy = 0.1),
  "\n"
)
cat(
  "Date range:", format(min(dat_insomnia$date), "%Y-%m-%d"), "to",
  format(max(dat_insomnia$date), "%Y-%m-%d"), "\n"
)

# =============================================================================
# DESCRIPTIVE SUMMARIES
# =============================================================================

insomnia_summary <- dat_insomnia |>
  summarise(
    n = n(),
    insomnia_n = sum(insomnia_any == 1, na.rm = TRUE),
    insomnia_rate = mean(insomnia_any == 1, na.rm = TRUE),
    mean_sleep_all = mean(duration, na.rm = TRUE),
    mean_sleep_no_insomnia = mean(duration[insomnia_any == 0], na.rm = TRUE),
    mean_sleep_insomnia = mean(duration[insomnia_any == 1], na.rm = TRUE),
    median_sleep_all = median(duration, na.rm = TRUE),
    median_sleep_no_insomnia = median(duration[insomnia_any == 0], na.rm = TRUE),
    median_sleep_insomnia = median(duration[insomnia_any == 1], na.rm = TRUE)
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

insomnia_code_summary <- dat_insomnia |>
  group_by(insomnia) |>
  summarise(
    n = n(),
    share = n / nrow(dat_insomnia),
    mean_sleep = mean(duration, na.rm = TRUE),
    median_sleep = median(duration, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(c(share, mean_sleep, median_sleep), \(x) round(x, 3)))

weekday_summary <- binomial_summary(dat_insomnia, "day_of_week")

sleep_band_summary <- binomial_summary(dat_insomnia, "sleep_band") |>
  drop_na(sleep_band)

factor_rate_summary <- dat_insomnia |>
  mutate(
    across(
      c(bedtime, coffee, stress, health, exercise),
      as.character
    )
  ) |>
  select(insomnia_any, duration, bedtime, coffee, stress, health, exercise) |>
  pivot_longer(
    cols = c(bedtime, coffee, stress, health, exercise),
    names_to = "factor",
    values_to = "category"
  ) |>
  drop_na(category) |>
  group_by(factor, category) |>
  summarise(
    n = n(),
    insomnia_n = sum(insomnia_any == 1, na.rm = TRUE),
    insomnia_rate = mean(insomnia_any == 1, na.rm = TRUE),
    mean_sleep = mean(duration, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    factor_label = recode(
      factor,
      bedtime = "Bedtime",
      coffee = "Coffee",
      stress = "Stress",
      health = "Health",
      exercise = "Exercise"
    ),
    insomnia_rate = round(insomnia_rate, 3),
    mean_sleep = round(mean_sleep, 3)
  )

cat("\n========== OVERALL INSOMNIA SUMMARY ==========\n")
print(insomnia_summary, n = Inf, width = Inf)

cat("\n========== INSOMNIA CODES ==========\n")
print(insomnia_code_summary, n = Inf, width = Inf)

cat("\n========== INSOMNIA BY WEEKDAY ==========\n")
print(weekday_summary, n = Inf, width = Inf)

cat("\n========== INSOMNIA BY SLEEP DURATION BAND ==========\n")
print(sleep_band_summary, n = Inf, width = Inf)

write_csv(insomnia_summary, file.path(output_dir, "insomnia_summary.csv"))
write_csv(insomnia_code_summary, file.path(output_dir, "insomnia_code_summary.csv"))
write_csv(weekday_summary, file.path(output_dir, "insomnia_by_weekday.csv"))
write_csv(sleep_band_summary, file.path(output_dir, "insomnia_by_sleep_band.csv"))
write_csv(factor_rate_summary, file.path(output_dir, "insomnia_by_behavioral_factors.csv"))

# =============================================================================
# VISUALIZATIONS
# =============================================================================

dat_time <- dat_insomnia |>
  arrange(date) |>
  mutate(
    insomnia_ma_30 = zoo::rollapply(
      insomnia_any,
      width = 30,
      FUN = mean,
      fill = NA,
      align = "right",
      partial = TRUE
    )
  )

p_time <- dat_time |>
  ggplot(aes(x = date)) +
  geom_point(
    aes(y = insomnia_any),
    alpha = 0.18,
    size = 1.4,
    color = col_dark_text
  ) +
  geom_line(
    aes(y = insomnia_ma_30),
    linewidth = 1.1,
    color = col_navy
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  labs(
    title = "Insomnia over time",
    subtitle = "Dots show nightly observations; line shows 30-night moving average",
    x = NULL,
    y = "Insomnia rate"
  ) +
  theme_sleep()

p_weekday <- weekday_summary |>
  ggplot(aes(x = day_of_week, y = insomnia_rate, group = 1)) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.12,
    color = col_dark_blue,
    alpha = 0.8
  ) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 3, color = col_orange) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Insomnia by day of week",
    subtitle = "Means with approximate 95% confidence intervals",
    x = NULL,
    y = "Insomnia rate"
  ) +
  theme_sleep()

p_duration <- dat_insomnia |>
  ggplot(aes(x = insomnia_status, y = duration, fill = insomnia_status)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.12, size = 1.5, color = col_dark_text) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = col_orange) +
  scale_fill_manual(
    values = c("No insomnia" = col_steel, "Any insomnia" = col_light_blue),
    guide = "none"
  ) +
  labs(
    title = "Sleep duration by insomnia status",
    subtitle = "Dots show individual nights; diamonds show means",
    x = NULL,
    y = "Sleep duration (hours)"
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_sleep_band <- sleep_band_summary |>
  ggplot(aes(x = sleep_band, y = insomnia_rate, fill = sleep_band)) +
  geom_col(alpha = 0.85) +
  geom_text(
    aes(label = scales::percent(insomnia_rate, accuracy = 1)),
    vjust = -0.35,
    size = 3.5,
    color = col_dark_text
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = make_palette(n_distinct(sleep_band_summary$sleep_band)),
    guide = "none"
  ) +
  labs(
    title = "Insomnia by sleep duration band",
    subtitle = "Same-night co-occurrence of short sleep and recorded insomnia",
    x = NULL,
    y = "Insomnia rate"
  ) +
  theme_sleep()

p_behavior <- factor_rate_summary |>
  ggplot(aes(x = category, y = insomnia_rate)) +
  geom_col(fill = col_steel, alpha = 0.85) +
  geom_text(
    aes(label = scales::percent(insomnia_rate, accuracy = 1)),
    vjust = -0.35,
    size = 3.2,
    color = col_dark_text
  ) +
  facet_wrap(~ factor_label, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Insomnia by behavioral and health factors",
    subtitle = "Unadjusted rates by diary category",
    x = NULL,
    y = "Insomnia rate"
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p_overview <- (p_time / (p_weekday + p_duration) / (p_sleep_band + p_behavior)) +
  plot_layout(heights = c(1, 1, 1.15)) +
  plot_annotation(
    title = "Insomnia patterns in the sleep diary",
    subtitle = "Time trends, weekday variation, sleep duration, and diary factors"
  )

print(p_overview)
print(p_weekday)
print(p_duration)
print(p_sleep_band)
print(p_behavior)

save_plot <- function(plot, filename, width = 10, height = 6) {
  ggsave(
    file.path(figure_dir, filename),
    plot,
    width = width,
    height = height,
    dpi = 300
  )
}

save_plot(p_overview, "insomnia_overview.png", width = 14, height = 14)
save_plot(p_time, "insomnia_over_time.png", width = 10, height = 6)
save_plot(p_weekday, "insomnia_by_weekday.png", width = 10, height = 6)
save_plot(p_duration, "sleep_duration_by_insomnia.png", width = 8, height = 6)
save_plot(p_sleep_band, "insomnia_by_sleep_band.png", width = 8, height = 6)
save_plot(p_behavior, "insomnia_by_behavioral_factors.png", width = 12, height = 8)

# =============================================================================
# MODEL DATA
# =============================================================================

dat_model <- dat_insomnia |>
  mutate(
    day_of_week = fct_drop(day_of_week),
    bedtime = factor(bedtime, levels = levels(bedtime), ordered = FALSE),
    coffee = factor(coffee, levels = levels(coffee), ordered = FALSE),
    stress = factor(stress, levels = levels(stress), ordered = FALSE),
    health = factor(health, levels = levels(health), ordered = FALSE),
    exercise = factor(exercise, levels = levels(exercise), ordered = FALSE),
    year_month = fct_drop(year_month)
  ) |>
  drop_na(day_of_week, bedtime, coffee, stress, health, exercise, year_month)

reference_day <- pick_reference(dat_model$day_of_week, "Mon")
reference_bedtime <- pick_reference(dat_model$bedtime, "Before 23:00")
reference_coffee <- pick_reference(dat_model$coffee, "None")
reference_stress <- pick_reference(dat_model$stress, "No")
reference_health <- pick_reference(dat_model$health, "Healthy")
reference_exercise <- pick_reference(dat_model$exercise, "None")

cat("\n========== INSOMNIA MODELLING SAMPLE ==========\n")
cat("Observations:", nrow(dat_model), "\n")
cat("Insomnia nights:", sum(dat_model$insomnia_any == 1), "\n")
cat(
  "Insomnia rate:",
  scales::percent(mean(dat_model$insomnia_any == 1), accuracy = 0.1),
  "\n"
)
cat("Reference weekday:", reference_day, "\n")
cat("Reference bedtime:", reference_bedtime, "\n")
cat("Reference coffee:", reference_coffee, "\n")
cat("Reference stress:", reference_stress, "\n")
cat("Reference health:", reference_health, "\n")
cat("Reference exercise:", reference_exercise, "\n")

# =============================================================================
# LOGISTIC MODELS
# =============================================================================

models_insomnia <- list(
  "Raw weekday" = safe_feglm(
    insomnia_any ~ i(day_of_week, ref = reference_day),
    data = dat_model,
    model_name = "Raw weekday"
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

if (length(models_insomnia) == 0) {
  stop("No insomnia models were estimated successfully.")
}

purrr::iwalk(
  models_insomnia,
  \(model, model_name) {
    cat("\n==========", toupper(model_name), "INSOMNIA MODEL ==========\n")
    print(summary(model))
  }
)

model_comparison <- tibble(
  model = names(models_insomnia),
  n = purrr::map_int(models_insomnia, nobs),
  log_likelihood = purrr::map_dbl(models_insomnia, \(model) logLik(model) |> as.numeric()),
  aic = purrr::map_dbl(models_insomnia, AIC),
  bic = purrr::map_dbl(models_insomnia, BIC)
) |>
  mutate(across(c(log_likelihood, aic, bic), \(x) round(x, 2)))

cat("\n========== MODEL COMPARISON ==========\n")
print(model_comparison, n = Inf, width = Inf)

write_csv(model_comparison, file.path(output_dir, "insomnia_model_comparison.csv"))

# =============================================================================
# ODDS RATIOS
# =============================================================================

clean_model_term <- function(x) {
  x |>
    str_remove_all("`") |>
    str_replace("^day_of_week::", "Weekday: ") |>
    str_replace("^bedtime::", "Bedtime: ") |>
    str_replace("^coffee::", "Coffee: ") |>
    str_replace("^stress::", "Stress: ") |>
    str_replace("^health::", "Health: ") |>
    str_replace("^exercise::", "Exercise: ")
}

get_odds_ratio_results <- function(model_results) {
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
        mutate(
          model = model_name,
          term_label = clean_model_term(term),
          variable_group = case_when(
            str_detect(term, "^day_of_week::") ~ "Weekday",
            TRUE ~ "Behavioral and health factors"
          ),
          odds_ratio = exp(estimate),
          ci_low = exp(estimate - 1.96 * std_error),
          ci_high = exp(estimate + 1.96 * std_error)
        ) |>
        select(
          model,
          variable_group,
          term,
          term_label,
          estimate,
          std_error,
          odds_ratio,
          ci_low,
          ci_high
        )
    }
  )
}

odds_ratio_results <- get_odds_ratio_results(models_insomnia) |>
  mutate(
    model = factor(model, levels = c("Raw weekday", "Adjusted", "Month FE")),
    term_label = factor(term_label, levels = rev(unique(term_label)))
  )

cat("\n========== ODDS RATIOS ==========\n")
print(
  odds_ratio_results |>
    mutate(across(c(estimate, std_error, odds_ratio, ci_low, ci_high), \(x) round(x, 3))),
  n = Inf,
  width = Inf
)

write_csv(odds_ratio_results, file.path(output_dir, "insomnia_odds_ratios.csv"))

p_odds <- odds_ratio_results |>
  ggplot(
    aes(
      x = odds_ratio,
      y = term_label,
      xmin = ci_low,
      xmax = ci_high,
      color = model
    )
  ) +
  geom_vline(xintercept = 1, linewidth = 0.3, linetype = "dashed") +
  geom_pointrange(
    position = position_dodge(width = 0.55),
    linewidth = 0.7
  ) +
  scale_x_log10(
    labels = scales::number_format(accuracy = 0.1),
    breaks = c(0.25, 0.5, 1, 2, 4, 8)
  ) +
  scale_color_manual(
    values = c(
      "Raw weekday" = col_light_blue,
      "Adjusted" = col_steel,
      "Month FE" = col_orange
    ),
    na.translate = FALSE
  ) +
  facet_wrap(~ variable_group, scales = "free_y", ncol = 1) +
  labs(
    title = "Logistic regression estimates for insomnia",
    subtitle = "Odds ratios with approximate 95% confidence intervals",
    x = "Odds ratio, log scale",
    y = NULL,
    color = NULL
  ) +
  theme_sleep() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(color = "grey90")
  )

print(p_odds)
save_plot(p_odds, "insomnia_odds_ratios.png", width = 11, height = 9)

# =============================================================================
# ADJUSTED PREDICTED PROBABILITIES
# =============================================================================

preferred_model_name <- c(
  intersect("Month FE", names(models_insomnia)),
  intersect("Adjusted", names(models_insomnia)),
  names(models_insomnia)[1]
) |>
  purrr::pluck(1)

preferred_model <- models_insomnia[[preferred_model_name]]

reference_values <- dat_model |>
  summarise(
    day_of_week = get_mode(day_of_week),
    bedtime = get_mode(bedtime),
    coffee = get_mode(coffee),
    stress = get_mode(stress),
    health = get_mode(health),
    exercise = get_mode(exercise),
    year_month = get_mode(year_month)
  )

predict_response <- function(model, newdata) {
  tryCatch(
    predict(model, newdata = newdata, type = "response") |>
      as.numeric(),
    error = \(e) {
      warning("Prediction failed: ", conditionMessage(e))
      rep(NA_real_, nrow(newdata))
    }
  )
}

make_prediction_data <- function(var_name, var_label) {
  tibble(category = levels(dat_model[[var_name]])) |>
    mutate(
      day_of_week = factor(reference_values$day_of_week, levels = levels(dat_model$day_of_week)),
      bedtime = factor(reference_values$bedtime, levels = levels(dat_model$bedtime)),
      coffee = factor(reference_values$coffee, levels = levels(dat_model$coffee)),
      stress = factor(reference_values$stress, levels = levels(dat_model$stress)),
      health = factor(reference_values$health, levels = levels(dat_model$health)),
      exercise = factor(reference_values$exercise, levels = levels(dat_model$exercise)),
      year_month = factor(reference_values$year_month, levels = levels(dat_model$year_month)),
      "{var_name}" := factor(category, levels = levels(dat_model[[var_name]])),
      predicted_probability = predict_response(preferred_model, cur_data())
    ) |>
    transmute(
      variable = var_label,
      category = category,
      predicted_probability = predicted_probability,
      model = preferred_model_name
    )
}

prediction_variables <- tibble(
  variable = c("day_of_week", "bedtime", "coffee", "stress", "health", "exercise"),
  variable_label = c("Weekday", "Bedtime", "Coffee", "Stress", "Health", "Exercise")
)

prediction_results <- purrr::map2_dfr(
  prediction_variables$variable,
  prediction_variables$variable_label,
  make_prediction_data
)

cat("\n========== ADJUSTED PREDICTED PROBABILITIES ==========\n")
cat("Prediction model:", preferred_model_name, "\n")
print(
  prediction_results |>
    mutate(predicted_probability = round(predicted_probability, 3)),
  n = Inf,
  width = Inf
)

write_csv(prediction_results, file.path(output_dir, "insomnia_predicted_probabilities.csv"))

p_predicted <- prediction_results |>
  ggplot(aes(x = category, y = predicted_probability, group = 1)) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 2.5, color = col_orange) +
  facet_wrap(~ variable, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Adjusted predicted probability of insomnia",
    subtitle = paste0("Predictions from the ", preferred_model_name, " model; other variables held at modal values"),
    x = NULL,
    y = "Predicted probability"
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_predicted)
save_plot(p_predicted, "insomnia_predicted_probabilities.png", width = 12, height = 8)

# =============================================================================
# REPORTING SUMMARY
# =============================================================================

cat("\n========== REPORTING SUMMARY ==========\n")
cat(
  "The script describes insomnia patterns and estimates raw, adjusted, and month",
  "fixed-effect logistic models for any recorded insomnia.\n"
)
cat("Main overview figure saved to:", file.path(figure_dir, "insomnia_overview.png"), "\n")
cat("Odds ratio figure saved to:", file.path(figure_dir, "insomnia_odds_ratios.png"), "\n")
cat("Predicted probability figure saved to:", file.path(figure_dir, "insomnia_predicted_probabilities.png"), "\n")
cat("Tables saved to:", output_dir, "\n")
