# =============================================================================
# 06_fixed_effects_models.R
#
# Purpose: Fit time fixed-effects models for sleep duration and visualize
#          within-period associations in the sleep diary data.
#
# Strategy:
#   Model 1 (pooled):      duration ~ bedtime + coffee + stress + health +
#                          exercise + day_of_week
#   Model 2 (time FE):     duration ~ bedtime + coffee + stress + health +
#                          exercise | day_of_week + year_month
#   Model 3 (time FE + sensor):
#                          duration ~ bedtime + coffee + stress + health +
#                          exercise + ns(CO2) + ns(temp) + humidity |
#                          day_of_week + year_month
#
# Outputs:
#   - Model summaries written to outputs/
#   - Coefficient comparison plot
#   - Partial-residual plot for CO2 and sleep duration
# =============================================================================

library(tidyverse)
library(fixest)
library(here)
library(splines)

source(here("scripts", "03_join_relevant_data.R"))

if (!exists("sleep_mittari")) {
  stop("sleep_mittari not found. Run 03_join_relevant_data.R first.")
}

if (!exists("sleep_mittari_sensor")) {
  stop("sleep_mittari_sensor not found. Run 03_join_relevant_data.R first.")
}

dir.create(here("figures"), showWarnings = FALSE)
dir.create(here("outputs"), showWarnings = FALSE)

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

# =============================================================================
# HELPERS
# =============================================================================

get_mode <- function(x) {
  x_no_na <- x[!is.na(x)]
  x_levels <- unique(x_no_na)
  x_levels[which.max(tabulate(match(x_no_na, x_levels)))]
}

tidy_fixest <- function(model, model_name) {
  coefs <- summary(model)$coeftable |>
    as.data.frame() |>
    rownames_to_column("term")
  
  cis <- confint(model) |>
    as.data.frame() |>
    rownames_to_column("term")
  
  coefs |>
    left_join(cis, by = "term") |>
    transmute(
      model = model_name,
      term = term,
      estimate = Estimate,
      std_error = `Std. Error`,
      conf_low = `2.5 %`,
      conf_high = `97.5 %`
    )
}

clean_term <- function(x) {
  x |>
    str_replace("^bedtime", "Bedtime: ") |>
    str_replace("^coffee", "Coffee: ") |>
    str_replace("^stress", "Stress: ") |>
    str_replace("^health", "Health: ") |>
    str_replace("^exercise", "Exercise: ") |>
    str_replace_all("`", "")
}

# =============================================================================
# PREPARE DATA
# =============================================================================

dat_fe <- sleep_mittari |>
  mutate(
    year_month = factor(format(date, "%Y-%m")),
    day_of_week = fct_drop(day_of_week),
    bedtime = fct_drop(bedtime),
    coffee = fct_drop(coffee),
    stress = fct_drop(stress),
    health = fct_drop(health),
    exercise = fct_drop(exercise)
  ) |>
  select(
    date, duration, bedtime, coffee, stress, health, exercise,
    day_of_week, year_month
  ) |>
  drop_na()

dat_sensor <- sleep_mittari_sensor |>
  mutate(
    year_month = factor(format(date, "%Y-%m")),
    day_of_week = fct_drop(day_of_week),
    bedtime = fct_drop(bedtime),
    coffee = fct_drop(coffee),
    stress = fct_drop(stress),
    health = fct_drop(health),
    exercise = fct_drop(exercise)
  ) |>
  select(
    date, duration, bedtime, coffee, stress, health, exercise,
    day_of_week, year_month, ka_co2, ka_temp, ka_humid
  ) |>
  drop_na()

cat("\n========== FIXED-EFFECTS SAMPLE ==========\n")
cat("Full modelling sample:", nrow(dat_fe), "nights\n")
cat("Sensor modelling sample:", nrow(dat_sensor), "nights\n")
cat("Date range (full):", format(min(dat_fe$date), "%Y-%m-%d"), "to",
    format(max(dat_fe$date), "%Y-%m-%d"), "\n")
cat("Date range (sensor):", format(min(dat_sensor$date), "%Y-%m-%d"), "to",
    format(max(dat_sensor$date), "%Y-%m-%d"), "\n")

# =============================================================================
# MODELS
# =============================================================================

m_pool <- feols(
  duration ~ bedtime + coffee + stress + health + exercise + day_of_week,
  data = dat_fe,
  vcov = "hetero"
)

m_fe <- feols(
  duration ~ bedtime + coffee + stress + health + exercise |
    day_of_week + year_month,
  data = dat_fe,
  vcov = "hetero"
)

m_sensor <- feols(
  duration ~ bedtime + coffee + stress + health + exercise +
    ns(ka_co2, df = 3) +
    ns(ka_temp, df = 3) +
    scale(ka_humid) |
    day_of_week + year_month,
  data = dat_sensor,
  vcov = "hetero"
)

cat("\n========== MODEL SUMMARIES ==========\n")
print(summary(m_pool))
print(summary(m_fe))
print(summary(m_sensor))

model_table <- etable(
  m_pool, m_fe, m_sensor,
  headers = c("Pooled OLS", "Time FE", "Time FE + sensor"),
  fitstat = ~ n + r2 + ar2 + rmse
)

capture.output(
  model_table,
  file = here("outputs", "06_fixed_effects_models.txt")
)

# =============================================================================
# COEFFICIENT PLOT
# =============================================================================

coef_data <- bind_rows(
  tidy_fixest(m_pool, "Pooled OLS"),
  tidy_fixest(m_fe, "Time FE"),
  tidy_fixest(m_sensor, "Time FE + sensor")
) |>
  filter(str_detect(term, "^(bedtime|coffee|stress|health|exercise)")) |>
  mutate(term_clean = clean_term(term))

term_order <- coef_data |>
  filter(model == "Time FE") |>
  arrange(estimate) |>
  pull(term_clean) |>
  unique()

coef_data <- coef_data |>
  mutate(term_clean = factor(term_clean, levels = term_order))

p_coef <- ggplot(
  coef_data,
  aes(x = estimate, y = term_clean, color = model)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(
    aes(xmin = conf_low, xmax = conf_high),
    position = position_dodge(width = 0.55),
    linewidth = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Pooled OLS" = col_light_blue,
      "Time FE" = col_dark_blue,
      "Time FE + sensor" = col_orange
    )
  ) +
  labs(
    title = "Coefficient estimates across model specifications",
    subtitle = "Sleep duration (hours), 95% confidence intervals",
    x = "Estimate (hours)",
    y = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

print(p_coef)

ggsave(
  here("figures", "16_fixed_effects_coefficients.png"),
  p_coef,
  width = 10,
  height = 7,
  dpi = 300
)

# =============================================================================
# PARTIAL-RESIDUAL PLOT FOR CO2
# =============================================================================

m_y_resid <- feols(
  duration ~ bedtime + coffee + stress + health + exercise +
    ns(ka_temp, df = 3) +
    scale(ka_humid) |
    day_of_week + year_month,
  data = dat_sensor,
  vcov = "hetero"
)

m_x_resid <- feols(
  ka_co2 ~ bedtime + coffee + stress + health + exercise +
    ns(ka_temp, df = 3) +
    scale(ka_humid) |
    day_of_week + year_month,
  data = dat_sensor,
  vcov = "hetero"
)

partial_dat <- dat_sensor |>
  mutate(
    duration_resid = resid(m_y_resid),
    co2_resid = resid(m_x_resid)
  )

p_partial_co2 <- ggplot(
  partial_dat,
  aes(x = co2_resid, y = duration_resid)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_point(alpha = 0.45, size = 2.2, color = col_dark_blue) +
  geom_smooth(method = "loess", se = TRUE, color = col_orange, fill = col_pale_blue) +
  labs(
    title = "Partial relationship between CO2 and sleep duration",
    subtitle = paste(
      "Residualized for bedtime, coffee, stress, health, exercise,",
      "weekday, year-month, temperature, and humidity"
    ),
    x = "Residualized CO2 (ppm)",
    y = "Residualized sleep duration (hours)"
  ) +
  theme_sleep()

print(p_partial_co2)

ggsave(
  here("figures", "17_partial_co2_sleep.png"),
  p_partial_co2,
  width = 10,
  height = 6,
  dpi = 300
)

# =============================================================================
# ADJUSTED CO2 PREDICTIONS
# =============================================================================

reference_day <- get_mode(dat_sensor$day_of_week)
reference_month <- get_mode(dat_sensor$year_month)
reference_bedtime <- get_mode(dat_sensor$bedtime)
reference_coffee <- get_mode(dat_sensor$coffee)
reference_stress <- get_mode(dat_sensor$stress)
reference_health <- get_mode(dat_sensor$health)
reference_exercise <- get_mode(dat_sensor$exercise)

pred_grid <- tibble(
  ka_co2 = seq(
    quantile(dat_sensor$ka_co2, 0.05, na.rm = TRUE),
    quantile(dat_sensor$ka_co2, 0.95, na.rm = TRUE),
    length.out = 100
  ),
  ka_temp = median(dat_sensor$ka_temp, na.rm = TRUE),
  ka_humid = median(dat_sensor$ka_humid, na.rm = TRUE),
  day_of_week = reference_day,
  year_month = reference_month,
  bedtime = reference_bedtime,
  coffee = reference_coffee,
  stress = reference_stress,
  health = reference_health,
  exercise = reference_exercise
)

pred_values <- predict(m_sensor, newdata = pred_grid)
pred_grid$pred_duration <- if (is.data.frame(pred_values) || is.matrix(pred_values)) {
  if ("fit" %in% colnames(pred_values)) {
    pred_values[, "fit"]
  } else if (ncol(pred_values) == 1) {
    pred_values[, 1]
  } else {
    pred_values[, 1]
  }
} else {
  as.vector(pred_values)
}

if (!("pred_duration" %in% names(pred_grid))) {
  stop("Prediction failed: `pred_duration` column not found in `pred_grid`.")
}

p_pred_co2 <- ggplot(pred_grid, aes(x = ka_co2, y = pred_duration)) +
  geom_line(color = col_orange, linewidth = 1.1) +
  labs(
    title = "Adjusted prediction: CO2 and sleep duration",
    subtitle = "Other covariates fixed at modal or median values",
    x = "Nightly mean CO2 (ppm)",
    y = "Predicted sleep duration (hours)"
  ) +
  theme_sleep()

print(p_pred_co2)

ggsave(
  here("figures", "18_adjusted_prediction_co2.png"),
  p_pred_co2,
  width = 10,
  height = 6,
  dpi = 300
)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n========== FIXED-EFFECTS SUMMARY ==========\n")
cat("Pooled OLS observations:", nobs(m_pool), "\n")
cat("Time FE observations:", nobs(m_fe), "\n")
cat("Time FE + sensor observations:", nobs(m_sensor), "\n")
cat(
  "Distinct year-month fixed effects (Time FE model):",
  n_distinct(dat_fe$year_month),
  "\n"
)
cat(
  "Distinct year-month fixed effects (Time FE + sensor model):",
  n_distinct(dat_sensor$year_month),
  "\n"
)
cat("Output written to:", here("outputs", "06_fixed_effects_models.txt"), "\n")
cat("\n✓ Fixed-effects analysis complete. Figures saved to figures/.\n")
