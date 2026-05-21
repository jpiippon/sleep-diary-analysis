# =============================================================================
# coffee.R
#
# Purpose: Analyze coffee intake, previous-night sleep, and sleep outcomes.
#
# Research questions:
#   How has coffee intake varied over time?
#   Does coffee intake follow shorter previous-night sleep?
#   How is coffee intake associated with same-night sleep duration and insomnia?
#   Are coffee estimates sensitive to lagged-sleep controls or simple interactions?
#
# Input:
#   df_clean from scripts/01_load_main_data.R
#
# Outputs:
#   - descriptive summaries printed to console
#   - numbered figures saved to figures/variable_specific/coffee/
#   - raw, adjusted, month fixed-effect, and sensitivity models for reporting
#
# Notes for interpretation:
#   - The diary date is the exposure day and the night that starts on that date.
#   - Coffee is treated as a same-day diary exposure.
#   - Results are associations, not causal effects.
#   - Reverse causality is plausible: short previous-night sleep can increase coffee intake.
# =============================================================================

library(tidyverse)
library(fixest)
library(here)
library(patchwork)

source(here("scripts", "01_load_main_data.R"))

if (!exists("df_clean")) {
  stop("df_clean not found. Run 01_load_main_data.R first.")
}

figure_dir <- here("figures", "variable_specific", "coffee")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SETTINGS AND HELPERS
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
  if (n <= length(blues)) blues[seq_len(n)] else colorRampPalette(c(col_navy, col_pale_blue))(n)
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
  ggsave(file.path(figure_dir, filename), plot, width = width, height = height, dpi = 300)
}

pick_reference <- function(x, preferred) {
  c(intersect(preferred, levels(x)), levels(x)[1]) |>
    purrr::pluck(1)
}

fmt_pct <- function(x, accuracy = 1) scales::percent(x, accuracy = accuracy)
fmt_min <- function(x) paste0(if_else(x > 0, "+", ""), round(x), " min")

safe_feols <- function(fml, data, model_name) {
  tryCatch(
    feols(fml = fml, data = data, vcov = "hetero"),
    error = \(e) {
      warning("Model failed: ", model_name, ". Error: ", conditionMessage(e))
      NULL
    }
  )
}

safe_feglm <- function(fml, data, model_name) {
  tryCatch(
    feglm(fml = fml, data = data, family = binomial(link = "logit"), vcov = "hetero"),
    error = \(e) {
      warning("Model failed: ", model_name, ". Error: ", conditionMessage(e))
      NULL
    }
  )
}

clean_coffee_term <- function(x) {
  x |>
    str_remove_all("`") |>
    str_remove("^coffee::")
}

# =============================================================================
# ANALYSIS DATA
# =============================================================================

dat_coffee <- df_clean |>
  arrange(date) |>
  mutate(
    year = factor(format(date, "%Y")),
    year_month = factor(format(date, "%Y-%m")),
    insomnia_any = as.integer(insomnia_num > 0),
    prev_duration = lag(duration),
    prev2_duration = lag(duration, 2),
    two_night_sleep_sum = prev_duration + prev2_duration,
    two_night_shortfall = pmax(0, 14 - two_night_sleep_sum),
    prev_short_sleep = factor(
      case_when(
        is.na(prev_duration) ~ NA_character_,
        prev_duration < 6 ~ "Previous night <6 h",
        TRUE ~ "Previous night >=6 h"
      ),
      levels = c("Previous night >=6 h", "Previous night <6 h")
    ),
    diary_period = factor(
      case_when(
        date < as.Date("2020-03-01") ~ "Before Mar 2020",
        date < as.Date("2022-09-01") ~ "Mar 2020-Aug 2022",
        TRUE ~ "Sep 2022 onward"
      ),
      levels = c("Before Mar 2020", "Mar 2020-Aug 2022", "Sep 2022 onward")
    )
  ) |>
  select(
    date, year, year_month, diary_period, day_of_week,
    duration, prev_duration, prev2_duration, two_night_sleep_sum, two_night_shortfall,
    prev_short_sleep, insomnia_num, insomnia_any, coffee, bedtime, stress, health, exercise
  ) |>
  drop_na(coffee, duration, insomnia_num)

n_total <- nrow(dat_coffee)
coffee_palette <- make_palette(n_distinct(dat_coffee$coffee))

cat("\n========== COFFEE ANALYSIS SAMPLE ==========\n")
cat("Observations:", n_total, "\n")
cat("Date range:", format(min(dat_coffee$date), "%Y-%m-%d"), "to", format(max(dat_coffee$date), "%Y-%m-%d"), "\n")

# =============================================================================
# DESCRIPTIVE SUMMARIES
# =============================================================================

coffee_summary <- dat_coffee |>
  group_by(coffee) |>
  summarise(
    n = n(),
    share = n / nrow(dat_coffee),
    mean_sleep = mean(duration, na.rm = TRUE),
    median_sleep = median(duration, na.rm = TRUE),
    se_sleep = sd(duration, na.rm = TRUE) / sqrt(n),
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
    across(c(share, mean_sleep, median_sleep, se_sleep, ci_low, ci_high, insomnia_rate, insomnia_se, insomnia_ci_low, insomnia_ci_high), \(x) round(x, 3))
  )

prev_sleep_summary <- dat_coffee |>
  drop_na(prev_duration) |>
  group_by(coffee) |>
  summarise(
    n = n(),
    mean_prev_sleep = mean(prev_duration, na.rm = TRUE),
    median_prev_sleep = median(prev_duration, na.rm = TRUE),
    short_prev_sleep_share = mean(prev_duration < 6, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    prev_median_label = paste0("Prev. median: ", median_prev_sleep, " h"),
    across(c(mean_prev_sleep, median_prev_sleep, short_prev_sleep_share), \(x) round(x, 3))
  )

yearly_coffee_summary <- dat_coffee |>
  group_by(year, coffee) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(year) |>
  mutate(share = n / sum(n)) |>
  ungroup()

yearly_coffee_totals <- dat_coffee |>
  count(year, name = "n_year")

period_coffee_summary <- dat_coffee |>
  group_by(diary_period, coffee) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(diary_period) |>
  mutate(share = n / sum(n)) |>
  ungroup()

period_coffee_totals <- dat_coffee |>
  count(diary_period, name = "n_period")

coffee_after_prev_sleep_summary <- dat_coffee |>
  drop_na(prev_short_sleep) |>
  group_by(prev_short_sleep, coffee) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(prev_short_sleep) |>
  mutate(share = n / sum(n)) |>
  ungroup()

prev_short_duration_summary <- dat_coffee |>
  drop_na(prev_short_sleep) |>
  group_by(prev_short_sleep, coffee) |>
  summarise(
    n = n(),
    mean_sleep = mean(duration, na.rm = TRUE),
    se_sleep = sd(duration, na.rm = TRUE) / sqrt(n),
    ci_low = mean_sleep - 1.96 * se_sleep,
    ci_high = mean_sleep + 1.96 * se_sleep,
    .groups = "drop"
  )

cat("\n========== SLEEP OUTCOMES BY COFFEE CATEGORY ==========\n")
print(coffee_summary, n = Inf, width = Inf)
cat("\n========== PREVIOUS-NIGHT SLEEP BY COFFEE CATEGORY ==========\n")
print(prev_sleep_summary, n = Inf, width = Inf)
cat("\n========== COFFEE AFTER PREVIOUS-NIGHT SLEEP ==========\n")
print(coffee_after_prev_sleep_summary, n = Inf, width = Inf)

# =============================================================================
# DESCRIPTIVE VISUALIZATIONS
# =============================================================================

label_y <- quantile(dat_coffee$duration, 0.97, na.rm = TRUE)
prev_label_y <- quantile(dat_coffee$prev_duration, 0.97, na.rm = TRUE)

p_distribution <- coffee_summary |>
  ggplot(aes(x = coffee, y = share, fill = coffee)) +
  geom_col(alpha = 0.85, width = 0.72) +
  geom_text(aes(label = distribution_label), vjust = -0.35, size = 3.1, color = col_dark_text) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, min(1, max(coffee_summary$share, na.rm = TRUE) + 0.2))) +
  scale_fill_manual(values = coffee_palette, guide = "none") +
  labs(title = "Coffee categories are unevenly represented", subtitle = paste0("Nightly coffee category shares (N = ", n_total, ")"), x = NULL, y = "Share of nights") +
  theme_sleep()

p_over_time <- yearly_coffee_summary |>
  ggplot(aes(x = year, y = share, fill = coffee)) +
  geom_col(alpha = 0.92) +
  geom_text(data = yearly_coffee_totals, aes(x = year, y = 1.03, label = paste0("n=", n_year)), inherit.aes = FALSE, size = 3, color = col_dark_text) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = coffee_palette) +
  coord_cartesian(ylim = c(0, 1.08), clip = "off") +
  labs(title = "Coffee intake changed over time", subtitle = "Yearly category composition; labels show total nights per year", x = NULL, y = "Share of nights", fill = NULL) +
  theme_sleep()

p_period_composition <- period_coffee_summary |>
  ggplot(aes(x = diary_period, y = share, fill = coffee)) +
  geom_col(alpha = 0.92, width = 0.72) +
  geom_text(data = period_coffee_totals, aes(x = diary_period, y = 1.04, label = paste0("n=", n_period)), inherit.aes = FALSE, size = 3, color = col_dark_text) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = coffee_palette) +
  coord_cartesian(ylim = c(0, 1.08), clip = "off") +
  labs(title = "Coffee habits differ across diary periods", subtitle = "Period-level category composition; labels show total nights per period", x = NULL, y = "Share of nights", fill = NULL) +
  theme_sleep() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

p_prev_duration <- dat_coffee |>
  drop_na(prev_duration) |>
  ggplot(aes(x = coffee, y = prev_duration, fill = coffee)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.62) +
  geom_jitter(width = 0.12, alpha = 0.08, size = 1.0, color = col_dark_text) +
  geom_label(data = prev_sleep_summary, aes(x = coffee, y = prev_label_y, label = prev_median_label), inherit.aes = FALSE, size = 2.5, linewidth = 0.12, fill = "white", color = col_dark_text) +
  scale_fill_manual(values = coffee_palette, guide = "none") +
  labs(title = "Coffee days often follow shorter sleep", subtitle = "Previous-night sleep duration by next-day coffee category", x = NULL, y = "Previous-night sleep duration (hours)") +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_coffee_after_prev_sleep <- coffee_after_prev_sleep_summary |>
  ggplot(aes(x = prev_short_sleep, y = share, fill = coffee)) +
  geom_col(alpha = 0.92, width = 0.72) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = coffee_palette) +
  labs(title = "Coffee choices differ after short and longer nights", subtitle = "Coffee category shares by previous-night sleep duration", x = NULL, y = "Share of nights", fill = NULL) +
  theme_sleep()

p_duration <- dat_coffee |>
  ggplot(aes(x = coffee, y = duration, fill = coffee)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.62) +
  geom_jitter(width = 0.12, alpha = 0.08, size = 1.0, color = col_dark_text) +
  geom_label(data = coffee_summary, aes(x = coffee, y = label_y, label = median_label), inherit.aes = FALSE, size = 2.6, linewidth = 0.12, fill = "white", color = col_dark_text) +
  scale_fill_manual(values = coffee_palette, guide = "none") +
  labs(title = "Same-night sleep differs across coffee categories", subtitle = "Boxplots and medians by same-day coffee intake category", x = NULL, y = "Sleep duration (hours)") +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

p_insomnia <- coffee_summary |>
  ggplot(aes(x = coffee, y = insomnia_rate, group = 1)) +
  geom_errorbar(aes(ymin = insomnia_ci_low, ymax = insomnia_ci_high), width = 0.12, color = col_dark_blue, alpha = 0.8) +
  geom_line(linewidth = 1, color = col_dark_blue) +
  geom_point(size = 3, color = col_orange) +
  geom_text(aes(label = insomnia_label), vjust = -0.9, size = 3.0, color = col_dark_text) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, min(1, max(coffee_summary$insomnia_ci_high, na.rm = TRUE) + 0.08))) +
  labs(title = "Insomnia rates can be compared by coffee category", subtitle = "Share of nights with any recorded insomnia; approximate 95% confidence intervals", x = NULL, y = "Insomnia rate") +
  theme_sleep()

p_prev_short_interaction <- prev_short_duration_summary |>
  ggplot(aes(x = coffee, y = mean_sleep, color = prev_short_sleep, group = prev_short_sleep)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, position = position_dodge(width = 0.35), alpha = 0.8) +
  geom_line(linewidth = 1, position = position_dodge(width = 0.35)) +
  geom_point(size = 2.6, position = position_dodge(width = 0.35)) +
  scale_color_manual(values = c("Previous night >=6 h" = col_dark_blue, "Previous night <6 h" = col_orange)) +
  labs(title = "Coffee-sleep patterns differ after short and longer previous nights", subtitle = "Mean same-night sleep with approximate 95% confidence intervals", x = NULL, y = "Mean sleep duration (hours)", color = NULL) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

# =============================================================================
# MODEL DATA
# =============================================================================

dat_model <- dat_coffee |>
  mutate(
    coffee = factor(coffee, levels = levels(coffee), ordered = FALSE),
    bedtime = factor(bedtime, levels = levels(bedtime), ordered = FALSE),
    stress = factor(stress, levels = levels(stress), ordered = FALSE),
    health = factor(health, levels = levels(health), ordered = FALSE),
    exercise = factor(exercise, levels = levels(exercise), ordered = FALSE),
    day_of_week = fct_drop(day_of_week),
    year_month = fct_drop(year_month)
  ) |>
  drop_na(coffee, duration, insomnia_any, bedtime, stress, health, exercise, day_of_week, year_month)

reference_coffee <- pick_reference(dat_model$coffee, "None")
reference_bedtime <- pick_reference(dat_model$bedtime, "Before 23:00")
reference_stress <- pick_reference(dat_model$stress, "No")
reference_health <- pick_reference(dat_model$health, "Healthy")
reference_exercise <- pick_reference(dat_model$exercise, "None")
reference_day <- pick_reference(dat_model$day_of_week, c("Mon", "Monday"))

cat("\n========== COFFEE MODELLING SAMPLE ==========\n")
cat("Observations:", nrow(dat_model), "\n")
cat("Reference coffee:", reference_coffee, "\n")

models_duration <- list(
  "Raw" = feols(duration ~ i(coffee, ref = reference_coffee), data = dat_model, vcov = "hetero"),
  "Adjusted" = feols(
    duration ~ i(coffee, ref = reference_coffee) + i(bedtime, ref = reference_bedtime) + i(stress, ref = reference_stress) + i(health, ref = reference_health) + i(exercise, ref = reference_exercise) + i(day_of_week, ref = reference_day),
    data = dat_model,
    vcov = "hetero"
  ),
  "Month FE" = feols(
    duration ~ i(coffee, ref = reference_coffee) + i(bedtime, ref = reference_bedtime) + i(stress, ref = reference_stress) + i(health, ref = reference_health) + i(exercise, ref = reference_exercise) + i(day_of_week, ref = reference_day) | year_month,
    data = dat_model,
    vcov = "hetero"
  )
)

purrr::iwalk(models_duration, \(model, model_name) {
  cat("\n==========", toupper(model_name), "COFFEE MODEL: SLEEP DURATION ==========\n")
  print(summary(model))
})

models_insomnia <- list(
  "Raw" = safe_feglm(insomnia_any ~ i(coffee, ref = reference_coffee), data = dat_model, model_name = "Raw"),
  "Adjusted" = safe_feglm(
    insomnia_any ~ i(coffee, ref = reference_coffee) + i(bedtime, ref = reference_bedtime) + i(stress, ref = reference_stress) + i(health, ref = reference_health) + i(exercise, ref = reference_exercise) + i(day_of_week, ref = reference_day),
    data = dat_model,
    model_name = "Adjusted"
  ),
  "Month FE" = safe_feglm(
    insomnia_any ~ i(coffee, ref = reference_coffee) + i(bedtime, ref = reference_bedtime) + i(stress, ref = reference_stress) + i(health, ref = reference_health) + i(exercise, ref = reference_exercise) + i(day_of_week, ref = reference_day) | year_month,
    data = dat_model,
    model_name = "Month FE"
  )
) |>
  purrr::compact()

purrr::iwalk(models_insomnia, \(model, model_name) {
  cat("\n==========", toupper(model_name), "COFFEE MODEL: INSOMNIA ==========\n")
  print(summary(model))
})

# =============================================================================
# MODEL RESULTS AND SENSITIVITY CHECKS
# =============================================================================

get_duration_results <- function(model_results) {
  purrr::map2_dfr(model_results, names(model_results), \(model, model_name) {
    tibble(term = names(coef(model)), estimate = as.numeric(coef(model)), std_error = as.numeric(se(model))) |>
      filter(str_detect(term, "^coffee::")) |>
      transmute(
        model = model_name,
        coffee = clean_coffee_term(term),
        estimate_minutes = estimate * 60,
        ci_low_minutes = (estimate - 1.96 * std_error) * 60,
        ci_high_minutes = (estimate + 1.96 * std_error) * 60,
        label = fmt_min(estimate_minutes)
      )
  })
}

coffee_duration_results <- get_duration_results(models_duration) |>
  mutate(
    model = factor(model, levels = c("Raw", "Adjusted", "Month FE")),
    coffee = factor(coffee, levels = rev(setdiff(levels(dat_model$coffee), reference_coffee)))
  )

month_fe_duration_results <- coffee_duration_results |>
  filter(model == "Month FE")

p_duration_coef_main <- month_fe_duration_results |>
  ggplot(aes(y = coffee, x = estimate_minutes, xmin = ci_low_minutes, xmax = ci_high_minutes)) +
  geom_linerange(linewidth = 1.2, color = col_dark_blue, alpha = 0.9) +
  geom_point(size = 2.5, color = col_orange) +
  geom_label(aes(x = ci_high_minutes + 3, label = label), hjust = 0, size = 3, linewidth = 0.15, fill = "white", color = col_dark_text) +
  geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") +
  scale_x_continuous(labels = \(x) paste0(round(x), " min"), breaks = scales::breaks_pretty(n = 6), expand = expansion(mult = c(0.05, 0.28))) +
  labs(title = "Month fixed-effect estimates by coffee category", subtitle = paste0("Differences relative to ", reference_coffee, "; interpreted as associations"), x = "Difference in sleep duration (minutes)", y = NULL) +
  coord_cartesian(clip = "off") +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

p_duration_model_comparison <- coffee_duration_results |>
  ggplot(aes(y = coffee, x = estimate_minutes, xmin = ci_low_minutes, xmax = ci_high_minutes, color = model)) +
  geom_linerange(linewidth = 1.1, alpha = 0.75, position = position_dodge(width = 0.55)) +
  geom_point(size = 2.1, position = position_dodge(width = 0.55)) +
  geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") +
  scale_color_manual(values = c("Raw" = col_light_blue, "Adjusted" = col_steel, "Month FE" = col_orange)) +
  scale_x_continuous(labels = \(x) paste0(round(x), " min"), breaks = scales::breaks_pretty(n = 6)) +
  labs(title = "Duration model comparison by coffee category", subtitle = paste0("Differences relative to ", reference_coffee, "; negative values indicate shorter sleep"), x = "Difference in sleep duration", y = NULL, color = NULL) +
  theme_sleep() +
  theme(legend.position = "bottom", panel.grid.major.x = element_line(color = "grey90"))

get_insomnia_results <- function(model_results) {
  purrr::map2_dfr(model_results, names(model_results), \(model, model_name) {
    tibble(term = names(coef(model)), estimate = as.numeric(coef(model)), std_error = as.numeric(se(model))) |>
      filter(str_detect(term, "^coffee::")) |>
      transmute(model = model_name, coffee = clean_coffee_term(term), odds_ratio = exp(estimate), ci_low = exp(estimate - 1.96 * std_error), ci_high = exp(estimate + 1.96 * std_error))
  })
}

if (length(models_insomnia) > 0) {
  coffee_insomnia_results <- get_insomnia_results(models_insomnia) |>
    mutate(model = factor(model, levels = c("Raw", "Adjusted", "Month FE")), coffee = factor(coffee, levels = rev(setdiff(levels(dat_model$coffee), reference_coffee))))

  p_insomnia_model_comparison <- coffee_insomnia_results |>
    ggplot(aes(y = coffee, x = odds_ratio, xmin = ci_low, xmax = ci_high, color = model)) +
    geom_vline(xintercept = 1, linewidth = 0.3, linetype = "dashed") +
    geom_linerange(linewidth = 1.1, alpha = 0.75, position = position_dodge(width = 0.55)) +
    geom_point(size = 2.1, position = position_dodge(width = 0.55)) +
    scale_x_log10(labels = scales::number_format(accuracy = 0.1), breaks = c(0.5, 1, 2, 4, 8)) +
    scale_color_manual(values = c("Raw" = col_light_blue, "Adjusted" = col_steel, "Month FE" = col_orange), na.translate = FALSE) +
    labs(title = "Insomnia model comparison by coffee category", subtitle = paste0("Odds ratios relative to ", reference_coffee, "; values above 1 indicate higher odds"), x = "Odds ratio, log scale", y = NULL, color = NULL) +
    theme_sleep() +
    theme(legend.position = "bottom", panel.grid.major.x = element_line(color = "grey90"))
}

dat_sensitivity <- dat_model |>
  mutate(
    coffee = fct_relevel(coffee, reference_coffee),
    bedtime = fct_relevel(bedtime, reference_bedtime),
    stress = fct_relevel(stress, reference_stress),
    health = fct_relevel(health, reference_health),
    exercise = fct_relevel(exercise, reference_exercise),
    day_of_week = fct_relevel(day_of_week, reference_day),
    prev_short_sleep = fct_drop(prev_short_sleep)
  ) |>
  drop_na(prev_duration, two_night_shortfall, prev_short_sleep)

model_lagged <- safe_feols(
  duration ~ i(coffee, ref = reference_coffee) + i(bedtime, ref = reference_bedtime) + i(stress, ref = reference_stress) + i(health, ref = reference_health) + i(exercise, ref = reference_exercise) + i(day_of_week, ref = reference_day) + prev_duration | year_month,
  data = dat_sensitivity,
  model_name = "Month FE + previous-night sleep"
)

model_two_night <- safe_feols(
  duration ~ i(coffee, ref = reference_coffee) + i(bedtime, ref = reference_bedtime) + i(stress, ref = reference_stress) + i(health, ref = reference_health) + i(exercise, ref = reference_exercise) + i(day_of_week, ref = reference_day) + two_night_shortfall | year_month,
  data = dat_sensitivity,
  model_name = "Month FE + two-night shortfall"
)

model_interaction_prev <- safe_feols(
  duration ~ coffee * prev_short_sleep + bedtime + stress + health + exercise + day_of_week | year_month,
  data = dat_sensitivity,
  model_name = "Coffee x previous-night short sleep"
)

model_interaction_bedtime <- safe_feols(
  duration ~ coffee * bedtime + stress + health + exercise + day_of_week | year_month,
  data = dat_sensitivity,
  model_name = "Coffee x bedtime"
)

purrr::iwalk(list("Coffee x previous-night short sleep" = model_interaction_prev, "Coffee x bedtime" = model_interaction_bedtime) |> purrr::compact(), \(model, model_name) {
  cat("\n========== INTERACTION MODEL:", toupper(model_name), "==========\n")
  print(summary(model))
})

sensitivity_models <- list("Month FE" = models_duration[["Month FE"]], "Month FE + prev sleep" = model_lagged, "Month FE + 2-night shortfall" = model_two_night) |>
  purrr::compact()

sensitivity_duration_results <- get_duration_results(sensitivity_models) |>
  mutate(
    model = factor(model, levels = c("Month FE", "Month FE + prev sleep", "Month FE + 2-night shortfall")),
    coffee = factor(coffee, levels = rev(setdiff(levels(dat_model$coffee), reference_coffee)))
  )

p_sensitivity <- sensitivity_duration_results |>
  ggplot(aes(y = coffee, x = estimate_minutes, xmin = ci_low_minutes, xmax = ci_high_minutes, color = model)) +
  geom_linerange(linewidth = 1.1, alpha = 0.8, position = position_dodge(width = 0.65)) +
  geom_point(size = 2.1, position = position_dodge(width = 0.65)) +
  geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed") +
  scale_color_manual(values = c("Month FE" = col_orange, "Month FE + prev sleep" = col_dark_blue, "Month FE + 2-night shortfall" = col_steel)) +
  scale_x_continuous(labels = \(x) paste0(round(x), " min"), breaks = scales::breaks_pretty(n = 6)) +
  labs(title = "Lagged sleep checks test whether coffee reflects prior sleep loss", subtitle = "Coffee estimates before and after controlling for previous-night sleep or two-night shortfall", x = "Difference in sleep duration", y = NULL, color = NULL) +
  theme_sleep() +
  theme(legend.position = "bottom", panel.grid.major.x = element_line(color = "grey90"))

# =============================================================================
# MAIN FIGURE AND SUPPORTING FIGURES
# =============================================================================

p_main <- (p_over_time + p_prev_duration) / (p_duration + p_sensitivity) +
  plot_annotation(
    title = "Coffee is tied to both previous-night sleep and same-night sleep",
    subtitle = "Coffee use changes over time, often follows shorter sleep, and its sleep associations are sensitive to lagged-sleep controls",
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(size = 14, face = "bold"))

print(p_main)
print(p_distribution)
print(p_over_time)
print(p_period_composition)
print(p_prev_duration)
print(p_coffee_after_prev_sleep)
print(p_duration)
print(p_duration_coef_main)
print(p_duration_model_comparison)
print(p_insomnia)
print(p_prev_short_interaction)
print(p_sensitivity)

save_plot(p_main, "coffee_figure1_main.png", width = 14, height = 10)
save_plot(p_duration_model_comparison, "coffee_figureS1_duration_model_comparison.png", width = 12, height = 6)
if (exists("p_insomnia_model_comparison")) {
  print(p_insomnia_model_comparison)
  save_plot(p_insomnia_model_comparison, "coffee_figureS2_insomnia_model_comparison.png", width = 12, height = 6)
}
save_plot(p_over_time, "coffee_figureS3_over_time.png", width = 12, height = 6)
save_plot(p_distribution, "coffee_figureS4_distribution.png", width = 8, height = 6)
save_plot(p_duration, "coffee_figureS5_sleep_duration_boxplot.png", width = 8, height = 6)
save_plot(p_insomnia, "coffee_figureS6_insomnia_rate.png", width = 8, height = 6)
save_plot(p_sensitivity, "coffee_figureS7_sensitivity_lagged_sleep.png", width = 12, height = 7)
save_plot(p_prev_duration, "coffee_figureS8_previous_sleep_by_coffee.png", width = 8, height = 6)
save_plot(p_period_composition, "coffee_figureS9_period_composition.png", width = 10, height = 6)
save_plot(p_coffee_after_prev_sleep, "coffee_figureS10_coffee_after_previous_sleep.png", width = 8, height = 6)
save_plot(p_prev_short_interaction, "coffee_figureS11_sensitivity_prev_short_interaction.png", width = 10, height = 6)
save_plot(p_duration_coef_main, "coffee_figureS12_month_fe_duration_coefficients.png", width = 10, height = 6)

# Backward-compatible file names
save_plot(p_main, "coffee_overview.png", width = 14, height = 10)
save_plot(p_duration_model_comparison, "coffee_duration_coefficients.png", width = 12, height = 6)
save_plot(p_insomnia, "insomnia_by_coffee.png", width = 8, height = 6)
save_plot(p_duration, "sleep_duration_by_coffee.png", width = 8, height = 6)
save_plot(p_over_time, "coffee_over_time.png", width = 12, height = 6)

cat("\n========== REPORTING SUMMARY ==========\n")
cat("Coffee analysis now emphasizes both same-night associations and possible reverse causality from previous-night sleep.\n")
cat("Recommended main figure saved to:", file.path(figure_dir, "coffee_figure1_main.png"), "\n")
cat("Supporting figures saved to:", figure_dir, "\n")
cat("Interpretation note: coffee coefficients should not be interpreted causally without stronger timing assumptions.\n")
