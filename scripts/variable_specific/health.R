# =============================================================================
# health.R
#
# Purpose: Examine whether being clearly sick during the diary day is
# associated with easier sleep initiation and longer sleep on the following
# night.
#
# Research questions:
#   RQ1. How often did clearly sick days occur, and how many separate illness
#        episodes did they form?
#   RQ2. Was sleep duration longer after a clearly sick day than after a
#        healthy day?
#   RQ3. How much did the illness-sleep association change after calendar
#        adjustment or adjustment for bedtime as a possible mechanism?
#   RQ4. Was difficulty falling asleep less common after a sick day?
#   RQ5. How did sleep duration change around the start of isolated illness
#        episodes?
#   RQ6. Do the sparsely recorded child-illness nights support treating
#        nighttime child location as a child-illness proxy?
#
# Input:
#   df_clean from scripts/01_load_main_data.R
#
# Outputs:
#   - descriptive and model summaries printed to the console
#   - one four-panel main figure for public-facing reporting
#   - two supporting figures
#   - figures saved to outputs/figures/variable_specific/health/
#   - no CSV files
#
# Notes for interpretation:
#   - `health_num = 1` means that I was clearly sick during the diary day and
#     expected the illness to affect the following night. Every clearly sick
#     day in an illness episode was recorded as 1; the value returned to 0 when
#     I considered myself healthy.
#   - The primary comparison is own illness (`health_num = 1`) versus healthy
#     days (`health_num = 0`). Child-illness records (`health_num = 2`) are
#     excluded because they describe a different exposure and were recorded
#     inconsistently.
#   - Longer sleep and less difficulty falling asleep are separate outcomes.
#     The phrase "better sleep" is therefore avoided.
#   - Bedtime occurs after daytime illness and can be one pathway through which
#     illness changes sleep duration. It is excluded from the primary model and
#     added only as a mechanism sensitivity analysis.
#   - Coffee, exercise, stress, and nighttime child location can also change as
#     a consequence of illness. Their adjustment is exploratory and does not
#     define the primary estimate.
#   - Consecutive sick days are grouped into illness episodes using exact
#     calendar dates. Diary gaps are never bridged.
#   - `child_night_context` describes a sleeping arrangement. It is not a proxy
#     for child illness and is used only as context or a sensitivity covariate.
#   - Results are associations from one person's diary, not causal effects.
# =============================================================================

library(tidyverse)
library(fixest)
library(here)
library(patchwork)

source(here("scripts", "01_load_main_data.R"))

if (!exists("df_clean")) {
  stop("df_clean not found. Run 01_load_main_data.R first.")
}

required_health_cols <- c(
  "date", "series_id", "duration", "day_of_week", "bedtime",
  "coffee", "stress", "exercise", "health_num", "insomnia_num",
  "child_night_context"
)

missing_health_cols <- setdiff(required_health_cols, names(df_clean))

if (length(missing_health_cols) > 0) {
  stop(
    "Health analysis is missing required columns: ",
    paste(missing_health_cols, collapse = ", ")
  )
}

variable_name <- "health"
figure_dir <- here("outputs", "figures", "variable_specific", variable_name)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SETTINGS AND HELPERS
# =============================================================================

event_window_days <- -7:7
event_isolation_days <- 14

col_navy       <- "#002d5a"
col_dark_blue  <- "#2f4a73"
col_steel      <- "#4a7ba7"
col_light_blue <- "#a3c1d9"
col_pale_blue  <- "#d0e1ef"
col_orange     <- "#CC5500"
col_dark_text  <- "#2a2a2a"
col_grey       <- "grey40"

health_palette <- c(
  "Healthy" = col_dark_blue,
  "Self sick" = col_orange
)

model_palette <- c(
  "Raw" = col_light_blue,
  "Calendar adjusted" = col_dark_blue,
  "Plus bedtime sensitivity" = col_orange,
  "Plus behavior sensitivity" = col_steel
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
  paste0(if_else(x >= 0, "+", ""), round(x), " min")
}

fmt_pp <- function(x) {
  paste0(if_else(x >= 0, "+", ""), round(x, 1), " pp")
}

empty_plot <- function(message) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message, size = 4, color = col_grey) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void()
}

extract_numeric_result <- function(
  model,
  model_name,
  predictor,
  multiplier,
  label_function
) {
  if (is.null(model) || !predictor %in% names(coef(model))) {
    return(tibble())
  }

  estimate <- unname(coef(model)[[predictor]])
  std_error <- unname(se(model)[[predictor]])

  tibble(
    model = model_name,
    n = nobs(model),
    estimate = estimate * multiplier,
    ci_low = (estimate - 1.96 * std_error) * multiplier,
    ci_high = (estimate + 1.96 * std_error) * multiplier,
    label = label_function(estimate * multiplier)
  )
}

# =============================================================================
# ANALYSIS DATA AND ILLNESS EPISODES
# =============================================================================

dat_health_all <- df_clean |>
  arrange(date) |>
  mutate(
    year_num = as.integer(format(date, "%Y")),
    month_num = as.integer(format(date, "%m")),
    year_month = factor(format(date, "%Y-%m")),
    self_sick_num = case_when(
      health_num == 0 ~ 0L,
      health_num == 1 ~ 1L,
      TRUE ~ NA_integer_
    ),
    self_sick = factor(
      self_sick_num,
      levels = 0:1,
      labels = c("Healthy", "Self sick")
    ),
    child_sick_num = case_when(
      health_num %in% 0:1 ~ 0L,
      health_num == 2 ~ 1L,
      TRUE ~ NA_integer_
    ),
    difficulty_falling_asleep_num = case_when(
      insomnia_num == 1 ~ 1L,
      insomnia_num %in% c(0, 2) ~ 0L,
      TRUE ~ NA_integer_
    ),
    previous_self_sick_num = lag_by_calendar_days(self_sick_num, date, 1),
    episode_start = case_when(
      self_sick_num == 1L & coalesce(previous_self_sick_num, 0L) == 0L ~ TRUE,
      TRUE ~ FALSE
    ),
    episode_sequence = cumsum(as.integer(episode_start)),
    illness_episode_id = if_else(
      self_sick_num == 1L,
      episode_sequence,
      NA_integer_
    ),
    bedtime = factor(bedtime, levels = levels(bedtime), ordered = FALSE),
    coffee = factor(coffee, levels = levels(coffee), ordered = FALSE),
    stress = factor(stress, levels = levels(stress), ordered = FALSE),
    exercise = factor(exercise, levels = levels(exercise), ordered = FALSE),
    child_night_context = factor(
      child_night_context,
      levels = levels(child_night_context),
      ordered = FALSE
    ),
    day_of_week = factor(
      day_of_week,
      levels = levels(day_of_week),
      ordered = FALSE
    )
  ) |>
  select(
    date,
    series_id,
    year_num,
    month_num,
    year_month,
    day_of_week,
    duration,
    health_num,
    self_sick_num,
    self_sick,
    child_sick_num,
    previous_self_sick_num,
    episode_start,
    illness_episode_id,
    insomnia_num,
    difficulty_falling_asleep_num,
    bedtime,
    coffee,
    stress,
    exercise,
    child_night_context
  )

dat_primary <- dat_health_all |>
  filter(health_num %in% 0:1) |>
  drop_na(self_sick, self_sick_num, duration)

episode_summary <- dat_health_all |>
  filter(self_sick_num == 1L) |>
  group_by(illness_episode_id) |>
  summarise(
    start_date = min(date),
    end_date = max(date),
    sick_days = n(),
    mean_sleep = mean(duration),
    .groups = "drop"
  )

n_sick_days <- sum(dat_primary$self_sick_num)
n_healthy_days <- sum(dat_primary$self_sick_num == 0)
n_episodes <- nrow(episode_summary)
n_child_sick_days <- sum(dat_health_all$child_sick_num == 1L, na.rm = TRUE)

cat("\n========== HEALTH ANALYSIS SAMPLE ==========\n")
cat("Primary healthy/self-sick observations:", nrow(dat_primary), "\n")
cat("Healthy days:", n_healthy_days, "\n")
cat("Self-sick days:", n_sick_days, "\n")
cat("Separate self-illness episodes:", n_episodes, "\n")
cat("Recorded child-sick days excluded from the primary analysis:", n_child_sick_days, "\n")
cat(
  "Date range:", format(min(dat_primary$date), "%Y-%m-%d"), "to",
  format(max(dat_primary$date), "%Y-%m-%d"), "\n"
)

# =============================================================================
# DESCRIPTIVE SUMMARIES
# =============================================================================

health_summary <- dat_primary |>
  group_by(self_sick) |>
  summarise(
    n = n(),
    share = n / nrow(dat_primary),
    mean_sleep = mean(duration),
    median_sleep = median(duration),
    .groups = "drop"
  ) |>
  mutate(
    share_label = paste0(fmt_pct(share), "\n(n=", n, ")"),
    median_label = paste0("Median: ", round(median_sleep, 1), " h")
  )

year_summary <- dat_primary |>
  group_by(year_num) |>
  summarise(
    eligible_days = n(),
    sick_days = sum(self_sick_num),
    sick_share = mean(self_sick_num),
    episodes = sum(episode_start),
    observed_months = n_distinct(month_num),
    .groups = "drop"
  ) |>
  mutate(
    full_calendar_year = observed_months == 12,
    year_label = paste0(year_num, if_else(full_calendar_year, "", "*")),
    year_label = factor(year_label, levels = year_label),
    count_label = paste0(sick_days, " d\n", episodes, " ep")
  )

difficulty_summary <- dat_primary |>
  group_by(self_sick) |>
  summarise(
    n = sum(!is.na(difficulty_falling_asleep_num)),
    cases = sum(difficulty_falling_asleep_num == 1L, na.rm = TRUE),
    rate = mean(difficulty_falling_asleep_num, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    z = 1.96,
    denominator = 1 + z^2 / n,
    center = (rate + z^2 / (2 * n)) / denominator,
    half_width = z * sqrt(rate * (1 - rate) / n + z^2 / (4 * n^2)) /
      denominator,
    ci_low = pmax(0, center - half_width),
    ci_high = pmin(1, center + half_width),
    rate_label = paste0(fmt_pct(rate), "\n(", cases, "/", n, ")")
  )

bedtime_summary <- dat_primary |>
  group_by(self_sick) |>
  summarise(
    n = sum(!is.na(bedtime)),
    before_23_rate = mean(bedtime == "Before 23:00", na.rm = TRUE),
    .groups = "drop"
  )

child_context_summary <- dat_health_all |>
  drop_na(child_night_context, child_sick_num) |>
  group_by(child_night_context) |>
  summarise(
    n = n(),
    child_sick_days = sum(child_sick_num),
    child_sick_rate = mean(child_sick_num),
    .groups = "drop"
  ) |>
  mutate(
    context_label = paste0(
      child_sick_days,
      " recorded\n(n=",
      n,
      ")"
    )
  )

cat("\n========== SLEEP DURATION BY OWN HEALTH ==========\n")
print(health_summary, n = Inf, width = Inf)
cat("\n========== SELF-ILLNESS EPISODES ==========\n")
print(episode_summary, n = Inf, width = Inf)
cat("\n========== DIFFICULTY FALLING ASLEEP BY OWN HEALTH ==========\n")
print(difficulty_summary, n = Inf, width = Inf)
cat("\n========== BEDTIME BEFORE 23:00 BY OWN HEALTH ==========\n")
print(bedtime_summary, n = Inf, width = Inf)
cat("\n========== RECORDED CHILD ILLNESS BY NIGHTTIME CONTEXT ==========\n")
print(child_context_summary, n = Inf, width = Inf)

# =============================================================================
# SLEEP-DURATION MODELS
# =============================================================================

# The three main models use one complete-case sample. The calendar-adjusted
# model is primary. Bedtime is added separately because it may be a mechanism.
dat_duration_model <- dat_primary |>
  drop_na(
    duration,
    self_sick_num,
    bedtime,
    day_of_week,
    year_month
  ) |>
  mutate(
    bedtime = fct_relevel(fct_drop(bedtime), "Before 23:00"),
    day_of_week = fct_relevel(fct_drop(day_of_week), "Mon"),
    year_month = fct_drop(year_month)
  )

models_duration <- list(
  "Raw" = safe_feols(
    duration ~ self_sick_num,
    data = dat_duration_model,
    model_name = "Raw own-illness model"
  ),
  "Calendar adjusted" = safe_feols(
    duration ~ self_sick_num + day_of_week | year_month,
    data = dat_duration_model,
    model_name = "Calendar-adjusted own-illness model"
  ),
  "Plus bedtime sensitivity" = safe_feols(
    duration ~ self_sick_num + bedtime + day_of_week | year_month,
    data = dat_duration_model,
    model_name = "Own-illness model plus bedtime"
  )
) |>
  purrr::compact()

purrr::iwalk(
  models_duration,
  \(model, model_name) {
    cat("\n========== DURATION MODEL:", toupper(model_name), "==========\n")
    print(summary(model))
  }
)

duration_results <- purrr::imap_dfr(
  models_duration,
  \(model, model_name) {
    extract_numeric_result(
      model = model,
      model_name = model_name,
      predictor = "self_sick_num",
      multiplier = 60,
      label_function = fmt_min
    )
  }
) |>
  mutate(model = factor(model, levels = names(models_duration)))

# This exploratory sensitivity asks whether the estimate persists after
# conditioning on behaviors and sleeping arrangements that may themselves
# respond to illness. It does not replace the calendar-adjusted primary model.
dat_behavior_model <- dat_primary |>
  drop_na(
    duration,
    self_sick_num,
    bedtime,
    coffee,
    exercise,
    stress,
    child_night_context,
    day_of_week,
    year_month
  ) |>
  mutate(
    bedtime = fct_relevel(fct_drop(bedtime), "Before 23:00"),
    coffee = fct_relevel(fct_drop(coffee), "None"),
    exercise = fct_relevel(fct_drop(exercise), "None"),
    stress = fct_relevel(fct_drop(stress), "No"),
    child_night_context = fct_relevel(
      fct_drop(child_night_context),
      "No child nearby"
    ),
    day_of_week = fct_relevel(fct_drop(day_of_week), "Mon"),
    year_month = fct_drop(year_month)
  )

model_behavior_sensitivity <- safe_feols(
  duration ~
    self_sick_num +
    bedtime +
    coffee +
    exercise +
    stress +
    child_night_context +
    day_of_week |
    year_month,
  data = dat_behavior_model,
  model_name = "Own-illness model plus behavior sensitivity"
)

behavior_result <- extract_numeric_result(
  model = model_behavior_sensitivity,
  model_name = "Plus behavior sensitivity",
  predictor = "self_sick_num",
  multiplier = 60,
  label_function = fmt_min
)

if (!is.null(model_behavior_sensitivity)) {
  cat("\n========== DURATION MODEL: PLUS BEHAVIOR SENSITIVITY ==========\n")
  print(summary(model_behavior_sensitivity))
}

cat("\n========== OWN-ILLNESS DURATION ESTIMATES ==========\n")
print(bind_rows(duration_results, behavior_result), n = Inf, width = Inf)

# =============================================================================
# DIFFICULTY-FALLING-ASLEEP MODELS
# =============================================================================

dat_difficulty_model <- dat_primary |>
  drop_na(
    difficulty_falling_asleep_num,
    self_sick_num,
    day_of_week,
    year_month
  ) |>
  mutate(
    day_of_week = fct_relevel(fct_drop(day_of_week), "Mon"),
    year_month = fct_drop(year_month)
  )

# Linear probability models keep the result in percentage-point units and
# allow the same gap-aware Newey-West uncertainty estimator as duration models.
models_difficulty <- list(
  "Raw" = safe_feols(
    difficulty_falling_asleep_num ~ self_sick_num,
    data = dat_difficulty_model,
    model_name = "Raw difficulty-falling-asleep model"
  ),
  "Calendar adjusted" = safe_feols(
    difficulty_falling_asleep_num ~ self_sick_num + day_of_week | year_month,
    data = dat_difficulty_model,
    model_name = "Calendar-adjusted difficulty-falling-asleep model"
  )
) |>
  purrr::compact()

difficulty_results <- purrr::imap_dfr(
  models_difficulty,
  \(model, model_name) {
    extract_numeric_result(
      model = model,
      model_name = model_name,
      predictor = "self_sick_num",
      multiplier = 100,
      label_function = fmt_pp
    )
  }
)

purrr::iwalk(
  models_difficulty,
  \(model, model_name) {
    cat("\n========== DIFFICULTY MODEL:", toupper(model_name), "==========\n")
    print(summary(model))
  }
)

cat("\n========== DIFFICULTY-FALLING-ASLEEP ESTIMATES ==========\n")
print(difficulty_results, n = Inf, width = Inf)

# =============================================================================
# SLEEP AROUND ISOLATED ILLNESS ONSETS
# =============================================================================

if (nrow(episode_summary) > 0) {
  onset_numeric <- as.numeric(episode_summary$start_date)

  episode_summary <- episode_summary |>
    mutate(
      nearest_other_onset_days = purrr::map_dbl(
        onset_numeric,
        \(onset) {
          distances <- abs(onset_numeric - onset)
          min(c(distances[distances > 0], Inf))
        }
      ),
      isolated_onset = nearest_other_onset_days > event_isolation_days
    )
} else {
  episode_summary <- episode_summary |>
    mutate(
      nearest_other_onset_days = numeric(),
      isolated_onset = logical()
    )
}

event_onsets <- episode_summary |>
  filter(isolated_onset) |>
  select(illness_episode_id, onset_date = start_date)

if (nrow(event_onsets) > 0) {
  event_data <- tidyr::crossing(
    event_onsets,
    event_day = event_window_days
  ) |>
    mutate(event_date = onset_date + event_day) |>
    left_join(
      dat_health_all |>
        select(date, duration, health_num),
      by = c("event_date" = "date")
    ) |>
    filter(health_num %in% 0:1)

  event_summary <- event_data |>
    group_by(event_day) |>
    summarise(
      n = sum(!is.na(duration)),
      mean_sleep = mean(duration, na.rm = TRUE),
      std_error = sd(duration, na.rm = TRUE) / sqrt(n),
      ci_low = mean_sleep - 1.96 * std_error,
      ci_high = mean_sleep + 1.96 * std_error,
      .groups = "drop"
    ) |>
    filter(n > 1)
} else {
  event_summary <- tibble()
}

cat("\n========== ISOLATED ILLNESS-ONSET EVENT SUMMARY ==========\n")
cat("Isolation threshold between onsets:", event_isolation_days, "days\n")
cat("Isolated illness onsets:", nrow(event_onsets), "\n")
print(event_summary, n = Inf, width = Inf)

# =============================================================================
# FOUR-PANEL MAIN FIGURE
# =============================================================================

max_year_share <- max(year_summary$sick_share)

p_yearly_illness <- year_summary |>
  ggplot(aes(x = year_label, y = sick_share)) +
  geom_col(width = 0.72, fill = col_navy, alpha = 0.9) +
  geom_text(
    aes(label = count_label),
    vjust = -0.25,
    size = 2.7,
    color = col_dark_text,
    lineheight = 0.9
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, max_year_share + 0.07),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    title = paste0(n_episodes, " illness episodes across ", n_sick_days, " days"),
    subtitle = str_wrap(
      "Share of healthy/self-sick diary days; d = days, ep = episodes, * = partial year",
      width = 55
    ),
    x = NULL,
    y = "Self-sick days"
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

duration_label_y <- quantile(dat_primary$duration, 0.97, na.rm = TRUE)

p_duration <- dat_primary |>
  ggplot(aes(x = self_sick, y = duration, fill = self_sick)) +
  geom_boxplot(
    width = 0.62,
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
    data = health_summary,
    aes(x = self_sick, y = duration_label_y, label = median_label),
    inherit.aes = FALSE,
    size = 2.8,
    linewidth = 0.15,
    fill = "white",
    color = col_dark_text
  ) +
  scale_fill_manual(values = health_palette, guide = "none") +
  coord_cartesian(ylim = c(0, NA)) +
  labs(
    title = "Sleep was longer on clearly sick days",
    subtitle = str_wrap(
      "Raw sleep-duration distributions; child-sick days excluded",
      width = 55
    ),
    x = NULL,
    y = "Sleep duration (hours)"
  ) +
  theme_sleep()

calendar_duration_result <- duration_results |>
  filter(model == "Calendar adjusted")

duration_panel_title <- if (
  nrow(calendar_duration_result) == 1 &&
    calendar_duration_result$ci_low > 0
) {
  "The longer sleep persisted after calendar adjustment"
} else if (
  nrow(calendar_duration_result) == 1 &&
    calendar_duration_result$estimate > 0
) {
  "The adjusted estimate stayed positive but uncertain"
} else {
  "The illness-sleep estimate depended on adjustment"
}

p_duration_models <- duration_results |>
  mutate(model = fct_rev(model)) |>
  ggplot(aes(y = model, x = estimate)) +
  geom_segment(
    aes(x = ci_low, xend = ci_high, yend = model),
    linewidth = 1.1,
    color = col_dark_blue
  ) +
  geom_point(size = 2.8, color = col_orange) +
  geom_text(
    aes(x = ci_high, label = label),
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
    expand = expansion(mult = c(0.1, 0.3))
  ) +
  scale_y_discrete(labels = scales::label_wrap(18)) +
  coord_cartesian(clip = "off") +
  labs(
    title = str_wrap(duration_panel_title, width = 48),
    subtitle = str_wrap(
      "Self-sick versus healthy; bedtime is a mechanism sensitivity",
      width = 55
    ),
    x = "Difference in sleep duration",
    y = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

healthy_difficulty_rate <- difficulty_summary |>
  filter(self_sick == "Healthy") |>
  pull(rate)

sick_difficulty_rate <- difficulty_summary |>
  filter(self_sick == "Self sick") |>
  pull(rate)

difficulty_panel_title <- if (
  length(healthy_difficulty_rate) == 1 &&
    length(sick_difficulty_rate) == 1 &&
    abs(sick_difficulty_rate - healthy_difficulty_rate) < 0.03
) {
  "Difficulty falling asleep changed little while sick"
} else if (
  length(healthy_difficulty_rate) == 1 &&
    length(sick_difficulty_rate) == 1 &&
    sick_difficulty_rate < healthy_difficulty_rate
) {
  "Difficulty falling asleep was less common while sick"
} else {
  "Difficulty falling asleep differed while sick"
}

p_difficulty <- difficulty_summary |>
  ggplot(aes(x = self_sick, y = rate, fill = self_sick)) +
  geom_col(width = 0.62, alpha = 0.9) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.12,
    color = col_dark_text
  ) +
  geom_text(
    aes(label = rate_label),
    vjust = -0.35,
    size = 3,
    color = col_dark_text,
    lineheight = 0.9
  ) +
  scale_fill_manual(values = health_palette, guide = "none") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, max(difficulty_summary$ci_high) + 0.04),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = str_wrap(difficulty_panel_title, width = 48),
    subtitle = str_wrap(
      "Recorded difficulty falling asleep; raw rates with 95% Wilson intervals",
      width = 55
    ),
    x = NULL,
    y = "Share of nights"
  ) +
  theme_sleep()

if (nrow(calendar_duration_result) == 1) {
  main_estimate_text <- paste0(
    fmt_min(calendar_duration_result$estimate),
    " after calendar adjustment"
  )
} else {
  main_estimate_text <- "Calendar-adjusted estimate unavailable"
}

p_main <- (p_yearly_illness + p_duration) /
  (p_duration_models + p_difficulty) +
  plot_layout(guides = "collect", widths = c(1, 1), heights = c(1, 1)) +
  plot_annotation(
    title = "Clearly sick days coincided with longer sleep in this diary",
    subtitle = str_wrap(
      paste(
        "Illness was recorded during the day before the following night;",
        main_estimate_text
      ),
      width = 105
    ),
    caption = str_wrap(
      paste(
        "Panel C's primary estimate adjusts for weekday and year-month;",
        "95% CIs use a 7-day Newey-West estimator.",
        "The bedtime model asks whether earlier sleep timing explains part of the association.",
        "Panels A, B, and D are descriptive.",
        "Child-sick days are excluded from the primary analysis.",
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

if (nrow(event_summary) > 0) {
  p_event_profile <- event_summary |>
    ggplot(aes(x = event_day, y = mean_sleep)) +
    geom_ribbon(
      aes(ymin = ci_low, ymax = ci_high),
      fill = col_pale_blue,
      alpha = 0.7
    ) +
    geom_line(linewidth = 1, color = col_dark_blue) +
    geom_point(size = 2.2, color = col_dark_blue) +
    geom_point(
      data = event_summary |>
        filter(event_day == 0),
      size = 3.2,
      color = col_orange
    ) +
    geom_vline(xintercept = 0, linewidth = 0.35, linetype = "dashed") +
    scale_x_continuous(breaks = event_window_days) +
    scale_y_continuous(
      labels = \(x) paste0(round(x, 1), " h"),
      breaks = scales::breaks_pretty(n = 5)
    ) +
    labs(
      title = "Sleep duration around isolated illness onsets",
      subtitle = str_wrap(
        paste0(
          "Day 0 is the first clearly sick day; ",
          nrow(event_onsets),
          " onsets more than ",
          event_isolation_days,
          " days from another onset"
        ),
        width = 100
      ),
      caption = str_wrap(
        "Days after onset can represent continuing illness or recovery. Means and normal-approximation 95% intervals are descriptive.",
        width = 110
      ),
      x = "Days relative to illness onset",
      y = "Mean sleep duration"
    ) +
    theme_sleep() +
    theme(panel.grid.major.x = element_line(color = "grey92"))
} else {
  p_event_profile <- empty_plot(
    "Not enough isolated illness onsets for an event profile"
  )
}

if (nrow(child_context_summary) > 0) {
  p_child_context <- child_context_summary |>
    ggplot(
      aes(
        x = child_night_context,
        y = child_sick_rate,
        fill = child_night_context
      )
    ) +
    geom_col(width = 0.68, alpha = 0.9) +
    geom_text(
      aes(label = context_label),
      vjust = -0.3,
      size = 3,
      color = col_dark_text,
      lineheight = 0.9
    ) +
    scale_x_discrete(labels = scales::label_wrap(16)) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1),
      expand = expansion(mult = c(0, 0.2))
    ) +
    scale_fill_manual(
      values = c(col_pale_blue, col_light_blue, col_steel, col_dark_blue),
      guide = "none"
    ) +
    labs(
      title = "Nighttime child location is not a child-illness measure",
      subtitle = str_wrap(
        paste0(
          "Only ",
          n_child_sick_days,
          " child-sick days were recorded; sleeping context also occurs without illness"
        ),
        width = 95
      ),
      caption = "Descriptive only: child illnesses were recorded inconsistently.",
      x = NULL,
      y = "Recorded child-sick days"
    ) +
    theme_sleep()
} else {
  p_child_context <- empty_plot(
    "No usable child-illness and nighttime-context records"
  )
}

# Only the complete four-panel figure is saved for the main public-facing
# output. Its individual panels are not written as separate files.
figures_to_save <- list(
  "health_figure1_main.png" = list(
    plot = p_main,
    width = 10,
    height = 12.5
  ),
  "health_figureS1_illness_onset_profile.png" = list(
    plot = p_event_profile,
    width = 10,
    height = 6
  ),
  "health_figureS2_child_illness_context.png" = list(
    plot = p_child_context,
    width = 10,
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
cat("Research questions and the illness timing convention are documented at the top of this script.\n")
cat("Primary comparison: self-sick versus healthy days; child-sick days excluded.\n")
cat("Self-sick days:", n_sick_days, "across", n_episodes, "episodes.\n")
cat("Primary calendar-adjusted duration estimate:", main_estimate_text, "\n")
cat(
  "Bedtime, behaviors, and nighttime child context are sensitivity adjustments,",
  "not automatic confounder controls.\n"
)
cat("Main figure saved to:", file.path(figure_dir, "health_figure1_main.png"), "\n")
cat("Supporting figures saved to:", figure_dir, "\n")
cat("No CSV files were created.\n")
cat("Interpretation note: associations are not causal and describe one person's diary.\n")
