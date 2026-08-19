# =============================================================================
# coffee.R
#
# Purpose: Examine temporal ordering and year-to-year variation in the
# association between coffee use and sleep duration.
#
# Research questions:
#   RQ1. How has the share of diary days with any coffee changed over time?
#   RQ2. Is coffee more common after an exact previous night with <6 h sleep?
#   RQ3. How is same-day coffee associated with sleep duration on the night
#        that begins on that diary date?
#   RQ4. Does the adjusted coffee-sleep association vary across complete
#        calendar years?
#   RQ5. How sensitive is the pooled association to previous-night sleep
#        context, bedtime adjustment, and the original four-level coding?
#
# Input:
#   df_clean from scripts/01_load_main_data.R
#
# Outputs:
#   - descriptive and model summaries printed to the console
#   - one four-panel main figure for public-facing reporting
#   - a small set of supporting model figures
#   - figures saved to outputs/figures/variable_specific/coffee/
#   - no CSV files
#
# Notes for interpretation:
#   - The diary date is the coffee-exposure day and the night that starts on
#     that date. Therefore, same-date sleep is the night following that day's
#     coffee use.
#   - Previous-night sleep is matched by exact calendar date. Missing diary
#     dates are never treated as consecutive nights.
#   - Coffee use is intentionally episodic and goal-directed: coffee is used
#     when tired or when an additional alertness or productivity boost is
#     desired. Regular use is intentionally avoided to preserve its perceived
#     stimulant effect.
#   - Coffee days are therefore behaviorally selected rather than random.
#     Adjusting for previous-night sleep addresses only part of this
#     confounding by indication; perceived fatigue and productivity demands
#     may remain unmeasured.
#   - Any coffee versus no coffee is the primary exposure for year-specific
#     estimates because detailed timing categories are sparse in some years.
#     The mix of those categories can itself change over time, so the annual
#     estimate describes that year's mix of coffee days rather than one fixed
#     dose or timing.
#   - Bedtime occurs after daytime coffee and may be part of the pathway from
#     coffee to sleep. It is therefore included only as a sensitivity check.
#   - Year-specific estimates are exploratory and should not be interpreted as
#     proof that the effect of coffee changed.
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

variable_name <- "coffee"
figure_dir <- here("outputs", "figures", "variable_specific", variable_name)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SETTINGS AND HELPERS
# =============================================================================

short_sleep_cutoff <- 6
minimum_exposure_days_per_year <- 20

col_navy       <- "#002d5a"
col_dark_blue  <- "#2f4a73"
col_steel      <- "#4a7ba7"
col_mid_blue   <- "#6c8eb5"
col_light_blue <- "#a3c1d9"
col_pale_blue  <- "#d0e1ef"
col_orange     <- "#CC5500"
col_dark_text  <- "#2a2a2a"
col_grey       <- "grey40"

coffee_binary_palette <- c(
  "No coffee" = col_light_blue,
  "Any coffee" = col_orange
)

coffee_category_palette <- c(
  "None" = col_pale_blue,
  "Half a cup in the morning" = col_mid_blue,
  "A cup or two before noon" = col_dark_blue,
  "Coffee after noon" = col_orange
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

empty_plot <- function(message) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message, size = 4, color = col_grey) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void()
}

extract_binary_coffee_result <- function(model, model_name) {
  if (is.null(model) || !"coffee_any_num" %in% names(coef(model))) {
    return(tibble())
  }

  estimate <- unname(coef(model)[["coffee_any_num"]])
  std_error <- unname(se(model)[["coffee_any_num"]])

  tibble(
    model = model_name,
    n = nobs(model),
    estimate_minutes = estimate * 60,
    ci_low_minutes = (estimate - 1.96 * std_error) * 60,
    ci_high_minutes = (estimate + 1.96 * std_error) * 60,
    label = fmt_min(estimate_minutes)
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
    year_num = as.integer(format(date, "%Y")),
    month_num = as.integer(format(date, "%m")),
    year_month = factor(format(date, "%Y-%m")),
    coffee_any_num = case_when(
      coffee_code == 0 ~ 0L,
      coffee_code %in% 1:3 ~ 1L,
      TRUE ~ NA_integer_
    ),
    coffee_any = factor(
      coffee_any_num,
      levels = 0:1,
      labels = c("No coffee", "Any coffee")
    ),
    prev_duration = lag_by_calendar_days(duration, date, 1),
    prev_short_num = case_when(
      is.na(prev_duration) ~ NA_integer_,
      prev_duration < short_sleep_cutoff ~ 1L,
      TRUE ~ 0L
    ),
    prev_sleep_group = factor(
      prev_short_num,
      levels = 0:1,
      labels = c("Previous night >=6 h", "Previous night <6 h")
    ),
    coffee = factor(coffee, levels = levels(coffee), ordered = FALSE),
    bedtime = factor(bedtime, levels = levels(bedtime), ordered = FALSE),
    stress = factor(stress, levels = levels(stress), ordered = FALSE),
    health = factor(health, levels = levels(health), ordered = FALSE),
    exercise = factor(exercise, levels = levels(exercise), ordered = FALSE),
    day_of_week = factor(day_of_week, levels = levels(day_of_week), ordered = FALSE)
  ) |>
  select(
    date,
    series_id,
    year_num,
    month_num,
    year_month,
    day_of_week,
    duration,
    prev_duration,
    prev_short_num,
    prev_sleep_group,
    coffee_code,
    coffee,
    coffee_any_num,
    coffee_any,
    bedtime,
    stress,
    health,
    exercise
  ) |>
  drop_na(duration, coffee_any_num, coffee_any)

n_total <- nrow(dat_coffee)

cat("\n========== COFFEE ANALYSIS SAMPLE ==========\n")
cat("Observations:", n_total, "\n")
cat(
  "Date range:", format(min(dat_coffee$date), "%Y-%m-%d"), "to",
  format(max(dat_coffee$date), "%Y-%m-%d"), "\n"
)
cat("Any-coffee days:", sum(dat_coffee$coffee_any_num), "\n")
cat("No-coffee days:", sum(dat_coffee$coffee_any_num == 0), "\n")

# =============================================================================
# DESCRIPTIVE SUMMARIES
# =============================================================================

year_coverage <- dat_coffee |>
  group_by(year_num) |>
  summarise(
    n = n(),
    coffee_days = sum(coffee_any_num == 1),
    no_coffee_days = sum(coffee_any_num == 0),
    coffee_rate = mean(coffee_any_num),
    first_date = min(date),
    last_date = max(date),
    observed_months = n_distinct(month_num),
    .groups = "drop"
  ) |>
  mutate(
    full_calendar_year = observed_months == 12,
    year_label = paste0(year_num, if_else(full_calendar_year, "", "*")),
    year_label = factor(year_label, levels = year_label),
    rate_label = fmt_pct(coffee_rate)
  )

previous_sleep_summary <- dat_coffee |>
  drop_na(prev_sleep_group) |>
  group_by(prev_sleep_group) |>
  summarise(
    n = n(),
    coffee_days = sum(coffee_any_num == 1),
    coffee_rate = mean(coffee_any_num),
    se = sqrt(coffee_rate * (1 - coffee_rate) / n),
    ci_low = pmax(0, coffee_rate - 1.96 * se),
    ci_high = pmin(1, coffee_rate + 1.96 * se),
    .groups = "drop"
  ) |>
  mutate(label = paste0(fmt_pct(coffee_rate), "\n(n=", n, ")"))

duration_summary <- dat_coffee |>
  group_by(coffee_any) |>
  summarise(
    n = n(),
    mean_sleep = mean(duration),
    median_sleep = median(duration),
    .groups = "drop"
  ) |>
  mutate(median_label = paste0("Median: ", round(median_sleep, 1), " h"))

yearly_category_summary <- dat_coffee |>
  count(year_num, coffee, name = "n") |>
  group_by(year_num) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  left_join(
    year_coverage |>
      select(year_num, year_label),
    by = "year_num"
  )

short_night_rate <- previous_sleep_summary |>
  filter(prev_sleep_group == "Previous night <6 h") |>
  pull(coffee_rate)

longer_night_rate <- previous_sleep_summary |>
  filter(prev_sleep_group == "Previous night >=6 h") |>
  pull(coffee_rate)

coffee_after_short_title <- if (
  length(short_night_rate) == 1 &&
    length(longer_night_rate) == 1 &&
    short_night_rate > longer_night_rate
) {
  "Coffee was more common after <6 h sleep"
} else {
  "Coffee use differed by previous-night sleep"
}

cat("\n========== COFFEE USE BY YEAR ==========\n")
print(year_coverage, n = Inf, width = Inf)
cat("\n========== COFFEE AFTER PREVIOUS-NIGHT SLEEP ==========\n")
print(previous_sleep_summary, n = Inf, width = Inf)
cat("\n========== SAME-NIGHT SLEEP BY COFFEE USE ==========\n")
print(duration_summary, n = Inf, width = Inf)

# =============================================================================
# POOLED MODELS
# =============================================================================

# Use one complete-case sample so changes between the pooled models reflect the
# adjustment set rather than changes in which diary days are included.
dat_pooled_model <- dat_coffee |>
  drop_na(
    duration,
    coffee,
    coffee_any_num,
    prev_duration,
    bedtime,
    stress,
    health,
    exercise,
    day_of_week,
    year_month
  ) |>
  mutate(
    coffee = fct_drop(coffee),
    bedtime = fct_drop(bedtime),
    stress = fct_drop(stress),
    health = fct_drop(health),
    exercise = fct_drop(exercise),
    day_of_week = fct_drop(day_of_week),
    year_month = fct_drop(year_month)
  )

models_pooled <- list(
  "Raw" = safe_feols(
    duration ~ coffee_any_num,
    data = dat_pooled_model,
    model_name = "Raw pooled coffee model"
  ),
  "Calendar adjusted" = safe_feols(
    duration ~ coffee_any_num + day_of_week | year_month,
    data = dat_pooled_model,
    model_name = "Calendar-adjusted pooled coffee model"
  ),
  "Previous-sleep adjusted" = safe_feols(
    duration ~
      coffee_any_num +
      prev_duration +
      stress +
      health +
      exercise +
      day_of_week |
      year_month,
    data = dat_pooled_model,
    model_name = "Previous-sleep-adjusted pooled coffee model"
  ),
  "Plus bedtime sensitivity" = safe_feols(
    duration ~
      coffee_any_num +
      prev_duration +
      bedtime +
      stress +
      health +
      exercise +
      day_of_week |
      year_month,
    data = dat_pooled_model,
    model_name = "Pooled coffee model plus bedtime"
  )
) |>
  purrr::compact()

purrr::iwalk(
  models_pooled,
  \(model, model_name) {
    cat("\n==========", toupper(model_name), "==========\n")
    print(summary(model))
  }
)

pooled_results <- purrr::imap_dfr(
  models_pooled,
  \(model, model_name) extract_binary_coffee_result(model, model_name)
) |>
  mutate(model = factor(model, levels = rev(names(models_pooled))))

# A linear probability model provides an interpretable percentage-point check
# for RQ2. Panel B remains descriptive because this model does not establish why
# coffee was consumed.
model_coffee_after_short_sleep <- safe_feols(
  coffee_any_num ~ prev_short_num + day_of_week | year_month,
  data = dat_coffee |>
    drop_na(prev_short_num, day_of_week, year_month),
  model_name = "Coffee use after previous-night short sleep"
)

if (!is.null(model_coffee_after_short_sleep)) {
  cat("\n========== COFFEE USE AFTER <6 H PREVIOUS-NIGHT SLEEP ==========\n")
  print(summary(model_coffee_after_short_sleep))
}

# This theory-driven interaction asks whether the coffee-sleep association is
# different after a short previous night. It is a sensitivity analysis rather
# than part of the main annual comparison.
model_short_sleep_interaction <- safe_feols(
  duration ~
    coffee_any_num * prev_short_num +
    prev_duration +
    stress +
    health +
    exercise +
    day_of_week |
    year_month,
  data = dat_pooled_model |>
    drop_na(prev_short_num),
  model_name = "Coffee by previous-night short-sleep interaction"
)

if (!is.null(model_short_sleep_interaction)) {
  cat("\n========== COFFEE X PREVIOUS-NIGHT SHORT-SLEEP SENSITIVITY ==========\n")
  print(summary(model_short_sleep_interaction))
}

# =============================================================================
# YEAR-SPECIFIC MODELS
# =============================================================================

# Restrict year-specific estimates to full calendar years with enough exposed
# and unexposed days. Partial years remain visible in the descriptive panel.
dat_year_model <- dat_coffee |>
  drop_na(
    duration,
    coffee_any_num,
    prev_duration,
    stress,
    health,
    exercise,
    day_of_week,
    month_num
  )

eligible_years <- dat_year_model |>
  group_by(year_num) |>
  summarise(
    model_n = n(),
    coffee_days = sum(coffee_any_num == 1),
    no_coffee_days = sum(coffee_any_num == 0),
    .groups = "drop"
  ) |>
  inner_join(
    year_coverage |>
      select(year_num, full_calendar_year),
    by = "year_num"
  ) |>
  filter(
    full_calendar_year,
    coffee_days >= minimum_exposure_days_per_year,
    no_coffee_days >= minimum_exposure_days_per_year
  ) |>
  pull(year_num)

year_model_formula <- duration ~
  coffee_any_num +
  prev_duration +
  stress +
  health +
  exercise +
  day_of_week +
  factor(month_num)

models_by_year <- eligible_years |>
  set_names() |>
  purrr::map(
    \(selected_year) {
      year_data <- dat_year_model |>
        filter(year_num == selected_year) |>
        mutate(
          stress = fct_drop(stress),
          health = fct_drop(health),
          exercise = fct_drop(exercise),
          day_of_week = fct_drop(day_of_week)
        )

      model <- safe_feols(
        year_model_formula,
        data = year_data,
        model_name = paste("Year-specific coffee model", selected_year)
      )

      if (is.null(model)) {
        return(NULL)
      }

      list(
        model = model,
        model_n = nrow(year_data),
        coffee_days = sum(year_data$coffee_any_num == 1)
      )
    }
  ) |>
  purrr::compact()

yearly_model_results <- purrr::imap_dfr(
  models_by_year,
  \(model_info, year_name) {
    model <- model_info$model

    if (!"coffee_any_num" %in% names(coef(model))) {
      return(tibble())
    }

    estimate <- unname(coef(model)[["coffee_any_num"]])
    std_error <- unname(se(model)[["coffee_any_num"]])

    tibble(
      year_num = as.integer(year_name),
      model_n = model_info$model_n,
      coffee_days = model_info$coffee_days,
      estimate_minutes = estimate * 60,
      ci_low_minutes = (estimate - 1.96 * std_error) * 60,
      ci_high_minutes = (estimate + 1.96 * std_error) * 60,
      label = fmt_min(estimate_minutes)
    )
  }
)

if (nrow(yearly_model_results) > 0) {
  cat("\n========== YEAR-SPECIFIC ADJUSTED COFFEE ESTIMATES ==========\n")
  print(yearly_model_results, n = Inf, width = Inf)
}

# =============================================================================
# ORIGINAL COFFEE-CATEGORY SENSITIVITY MODEL
# =============================================================================

reference_coffee <- if ("None" %in% levels(dat_pooled_model$coffee)) {
  "None"
} else {
  levels(dat_pooled_model$coffee)[1]
}

model_coffee_categories <- safe_feols(
  duration ~
    i(coffee, ref = reference_coffee) +
    prev_duration +
    stress +
    health +
    exercise +
    day_of_week |
    year_month,
  data = dat_pooled_model,
  model_name = "Original coffee-category sensitivity model"
)

category_results <- if (!is.null(model_coffee_categories)) {
  model_terms <- names(coef(model_coffee_categories))
  model_estimates <- as.numeric(coef(model_coffee_categories))
  model_se <- as.numeric(se(model_coffee_categories))

  tibble(
    term = model_terms,
    estimate = model_estimates,
    std_error = model_se
  ) |>
    filter(str_detect(term, "^coffee::")) |>
    transmute(
      coffee = clean_coffee_term(term),
      estimate_minutes = estimate * 60,
      ci_low_minutes = (estimate - 1.96 * std_error) * 60,
      ci_high_minutes = (estimate + 1.96 * std_error) * 60,
      label = fmt_min(estimate_minutes)
    ) |>
    mutate(
      coffee = factor(
        coffee,
        levels = rev(setdiff(levels(dat_pooled_model$coffee), reference_coffee))
      )
    )
} else {
  tibble()
}

if (!is.null(model_coffee_categories)) {
  cat("\n========== ORIGINAL COFFEE-CATEGORY SENSITIVITY MODEL ==========\n")
  print(summary(model_coffee_categories))
}

# =============================================================================
# FOUR-PANEL MAIN FIGURE
# =============================================================================

p_yearly_coffee <- year_coverage |>
  ggplot(aes(x = year_label, y = coffee_rate)) +
  geom_col(width = 0.72, fill = col_navy, alpha = 0.9) +
  geom_text(
    aes(label = rate_label),
    vjust = -0.25,
    size = 2.7,
    color = col_dark_text,
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Coffee use changed markedly over time",
    subtitle = "Share of diary days with any coffee; * = partial year",
    x = NULL,
    y = "Days with coffee"
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_coffee_after_short <- previous_sleep_summary |>
  ggplot(aes(x = prev_sleep_group, y = coffee_rate, fill = prev_sleep_group)) +
  geom_col(width = 0.65, alpha = 0.92) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.12,
    color = col_dark_text
  ) +
  geom_text(
    aes(label = label),
    vjust = -0.35,
    size = 3,
    color = col_dark_text,
    lineheight = 0.9
  ) +
  scale_fill_manual(
    values = c(
      "Previous night >=6 h" = col_light_blue,
      "Previous night <6 h" = col_orange
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = coffee_after_short_title,
    subtitle = "Exact previous-day match; bars are descriptive",
    x = NULL,
    y = "Days with coffee"
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(size = 8.5))

duration_label_y <- quantile(dat_coffee$duration, 0.97, na.rm = TRUE)

p_same_night_duration <- dat_coffee |>
  ggplot(aes(x = coffee_any, y = duration, fill = coffee_any)) +
  geom_boxplot(
    width = 0.6,
    alpha = 0.75,
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
    aes(x = coffee_any, y = duration_label_y, label = median_label),
    inherit.aes = FALSE,
    size = 2.8,
    linewidth = 0.15,
    fill = "white",
    color = col_dark_text
  ) +
  scale_fill_manual(values = coffee_binary_palette, guide = "none") +
  coord_cartesian(ylim = c(0, NA)) +
  labs(
    title = "Same-night sleep by coffee use",
    subtitle = "Raw distributions; the reason for drinking coffee is unadjusted",
    x = NULL,
    y = "Sleep duration (hours)"
  ) +
  theme_sleep()

if (nrow(yearly_model_results) > 0) {
  p_yearly_estimates <- yearly_model_results |>
    mutate(year = factor(year_num, levels = sort(unique(year_num)))) |>
    ggplot(
      aes(
        x = year,
        y = estimate_minutes,
        group = 1
      )
    ) +
    geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed") +
    geom_errorbar(
      aes(ymin = ci_low_minutes, ymax = ci_high_minutes),
      width = 0.14,
      color = col_dark_blue,
      alpha = 0.8
    ) +
    geom_line(linewidth = 0.8, color = col_dark_blue, alpha = 0.75) +
    geom_point(size = 2.5, color = col_orange) +
    scale_y_continuous(
      labels = \(x) paste0(round(x), " min"),
      breaks = scales::breaks_pretty(n = 5),
      expand = expansion(mult = c(0.1, 0.12))
    ) +
    labs(
      title = "Adjusted estimates vary across years",
      subtitle = "Below zero = shorter sleep after coffee; complete years only",
      x = NULL,
      y = "Adjusted difference"
    ) +
    theme_sleep() +
    theme(
      panel.grid.major.x = element_line(color = "grey92"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
} else {
  p_yearly_estimates <- empty_plot(
    "Not enough complete years for year-specific estimates"
  )
}

partial_years <- year_coverage |>
  filter(!full_calendar_year) |>
  pull(year_num)

partial_year_note <- if (length(partial_years) > 0) {
  paste0("Partial years in panel A: ", paste(partial_years, collapse = ", "), ".")
} else {
  "All years in panel A cover all 12 months."
}

p_main <- (p_yearly_coffee + p_coffee_after_short) /
  (p_same_night_duration + p_yearly_estimates) +
  plot_layout(guides = "collect", widths = c(1, 1), heights = c(1, 1)) +
  plot_annotation(
    title = str_wrap(
      "Coffee often followed short sleep, while its link with the next night varied over time",
      width = 72
    ),
    subtitle = str_wrap(
      "The diary separates why coffee may have been used from what happened on the following night",
      width = 105
    ),
    caption = str_wrap(
      paste(
        "Panel B uses an exact 1-day calendar lag.",
        "Panel D compares any coffee with no coffee and adjusts for previous-night sleep, stress, health, exercise, weekday, and month;",
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

p_pooled_model_comparison <- pooled_results |>
  ggplot(
    aes(
      y = model,
      x = estimate_minutes
    )
  ) +
  geom_segment(
    aes(x = ci_low_minutes, xend = ci_high_minutes, yend = model),
    linewidth = 1.1,
    color = col_dark_blue
  ) +
  geom_point(size = 2.7, color = col_orange) +
  geom_text(
    aes(x = ci_high_minutes, label = label),
    nudge_x = 2,
    hjust = 0,
    size = 3,
    color = col_dark_text,
    fontface = "bold"
  ) +
  geom_vline(xintercept = 0, linewidth = 0.35, linetype = "dashed") +
  scale_x_continuous(
    labels = \(x) paste0(round(x), " min"),
    breaks = scales::breaks_pretty(n = 6),
    expand = expansion(mult = c(0.08, 0.25))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Previous-night sleep changes the pooled coffee estimate",
    subtitle = "Any coffee versus no coffee, using the same complete-case sample",
    caption = str_wrap(
      "Bedtime may be part of the coffee-to-sleep pathway, so the bedtime-adjusted model is a sensitivity check.",
      width = 100
    ),
    x = "Difference in sleep duration",
    y = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

if (nrow(category_results) > 0) {
  p_category_sensitivity <- category_results |>
    ggplot(
      aes(
        y = coffee,
        x = estimate_minutes
      )
    ) +
    geom_segment(
      aes(x = ci_low_minutes, xend = ci_high_minutes, yend = coffee),
      linewidth = 1.1,
      color = col_dark_blue
    ) +
    geom_point(size = 2.7, color = col_orange) +
    geom_text(
      aes(x = ci_high_minutes, label = label),
      nudge_x = 2,
      hjust = 0,
      size = 3,
      color = col_dark_text,
      fontface = "bold"
    ) +
    geom_vline(xintercept = 0, linewidth = 0.35, linetype = "dashed") +
    scale_x_continuous(
      labels = \(x) paste0(round(x), " min"),
      breaks = scales::breaks_pretty(n = 6),
      expand = expansion(mult = c(0.08, 0.25))
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title = "Detailed coffee categories are a sensitivity analysis",
      subtitle = str_wrap(
        paste0(
          "Adjusted differences relative to ", reference_coffee,
          "; sparse categories produce wide intervals"
        ),
        width = 90
      ),
      x = "Difference in sleep duration",
      y = NULL
    ) +
    theme_sleep() +
    theme(panel.grid.major.x = element_line(color = "grey90"))
} else {
  p_category_sensitivity <- empty_plot(
    "Detailed coffee-category model was not estimable"
  )
}

p_category_composition <- yearly_category_summary |>
  ggplot(aes(x = year_label, y = share, fill = coffee)) +
  geom_col(width = 0.78, alpha = 0.92) +
  geom_text(
    data = year_coverage,
    aes(x = year_label, y = 1.03, label = paste0("n=", n)),
    inherit.aes = FALSE,
    size = 3,
    color = col_dark_text
  ) +
  scale_fill_manual(values = coffee_category_palette, drop = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, 1.08), clip = "off") +
  labs(
    title = "The mix of coffee categories also changed over time",
    subtitle = str_wrap(
      "Annual any-coffee estimates can reflect both a changing association and a changing mix of coffee doses or timing; * = partial year",
      width = 110
    ),
    x = NULL,
    y = "Share of diary days",
    fill = NULL
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Only the complete four-panel figure is saved for the main public-facing
# output. Its individual panels are not written as separate files.
figures_to_save <- list(
  "coffee_figure1_main.png" = list(
    plot = p_main,
    width = 10,
    height = 12.5
  ),
  "coffee_figureS1_pooled_model_comparison.png" = list(
    plot = p_pooled_model_comparison,
    width = 10,
    height = 6
  ),
  "coffee_figureS2_timing_sensitivity.png" = list(
    plot = p_category_sensitivity,
    width = 10,
    height = 6
  ),
  "coffee_figureS3_timing_composition_over_time.png" = list(
    plot = p_category_composition,
    width = 12,
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

cat("\n========== REPORTING SUMMARY ==========\n")
cat("Research questions are documented at the top of this script.\n")
cat(
  "Primary year-specific exposure: any coffee versus no coffee; detailed",
  "coffee timing is retained as a sensitivity analysis.\n"
)
cat(
  "Eligible full years for adjusted annual estimates:",
  if_else(length(eligible_years) > 0, paste(eligible_years, collapse = ", "), "none"),
  "\n"
)
cat("Main figure saved to:", file.path(figure_dir, "coffee_figure1_main.png"), "\n")
cat("Supporting figures saved to:", figure_dir, "\n")
cat("No CSV files were created.\n")
cat(
  "Interpretation note: annual estimates can differ because exposure patterns,",
  "sample composition, or other unmeasured conditions changed; they are not",
  "causal effect estimates.\n"
)
