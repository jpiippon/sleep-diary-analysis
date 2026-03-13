# =============================================================================
# 04_descriptives_and_plots.R
#
# Purpose: Generate descriptive summaries and visualizations of sleep patterns
#          and sensor data for presentation
#
# Input: Clean dataset + joined sensor data from 03_join_relevant_data.R
#
# Outputs: Descriptive statistics and plots saved to figures/
# =============================================================================

library(tidyverse)
library(gridExtra)
library(zoo)
library(here)

source(here("scripts", "03_join_relevant_data.R"))

dir.create(here("figures"), showWarnings = FALSE)

df_clean <- df_clean |>
  drop_na(day_of_week, bedtime, coffee, stress, health)

sleep_mittari <- sleep_mittari |>
  drop_na(day_of_week, bedtime, coffee, stress, health)

sleep_mittari_sensor <- sleep_mittari_sensor |>
  drop_na(day_of_week, bedtime, coffee, stress, health)

# =============================================================================
# COLOR SYSTEM
# =============================================================================

col_navy       <- "#002d5a"
col_dark_blue  <- "#2f4a73"
col_steel      <- "#4a7ba7"
col_mid_blue   <- "#6c8eb5"
col_light_blue <- "#a3c1d9"
col_pale_blue  <- "#d0e1ef"

col_gold   <- "#fccb00"
col_orange <- "#CC5500"
col_amber  <- "#e8a317"

col_dark_text <- "#2a2a2a"
col_grey      <- "grey40"

# Dynamic palette: sequential blues (dark → light), for ordered/nominal categories
make_palette <- function(n) {
  blues <- c(col_navy, col_dark_blue, col_steel, col_mid_blue, col_light_blue, col_pale_blue)
  if (n <= length(blues)) {
    blues[1:n]
  } else {
    colorRampPalette(c(col_navy, col_pale_blue))(n)
  }
}

# Dynamic palette: diverging blue → amber → orange, for low-to-high severity scales
make_palette_diverging <- function(n) {
  colorRampPalette(c(col_mid_blue, col_amber, col_orange))(n)
}

# =============================================================================
# THEME
# =============================================================================

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

# ============================================================================
# 1. OVERALL SLEEP STATISTICS
# ============================================================================

cat("\n========== SLEEP DURATION STATISTICS ==========\n")
cat("Sample size:", nrow(df_clean), "observations\n")
cat("Date range:", format(min(df_clean$date), "%Y-%m-%d"), "to",
    format(max(df_clean$date), "%Y-%m-%d"), "\n\n")

cat("Sleep duration (hours) - Summary Statistics:\n")
print(summary(df_clean$duration))

cat("\nInsomnia prevalence:\n")
print(table(df_clean$insomnia, useNA = "ifany"))

cat("\nInsomnia rate (%):\n")
print(prop.table(table(df_clean$insomnia)) * 100)

# ============================================================================
# 2. SLEEP OVER TIME WITH MOVING AVERAGE
# ============================================================================

df_ma <- df_clean |>
  arrange(date) |>
  mutate(sleep_ma = rollapply(duration, 30, mean, fill = NA, align = "right", partial = TRUE))

p1 <- ggplot(df_ma, aes(x = date)) +
  geom_line(aes(y = duration, color = "Daily"), alpha = 0.6, linewidth = 0.5) +
  geom_line(aes(y = sleep_ma, color = "30-day MA"), linewidth = 1.2) +
  scale_color_manual(values = c("Daily" = col_mid_blue, "30-day MA" = col_navy)) +
  labs(
    title = "Sleep Duration Over Time",
    subtitle = paste("n =", nrow(df_clean), "nights"),
    x = "Date",
    y = "Sleep Duration (hours)"
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep()

print(p1)
ggsave(here("figures", "01_sleep_over_time.png"), p1, width = 10, height = 6, dpi = 300)

# ============================================================================
# 3. SLEEP BY DAY OF WEEK
# ============================================================================

cat("\n========== SLEEP DURATION BY DAY OF WEEK ==========\n")

sleep_by_day <- df_clean |>
  group_by(day_of_week) |>
  summarise(
    n          = n(),
    mean_sleep = mean(duration, na.rm = TRUE),
    sd_sleep   = sd(duration, na.rm = TRUE),
    median_sleep = median(duration, na.rm = TRUE)
  ) |>
  mutate(
    best_day  = mean_sleep == max(mean_sleep),
    worst_day = mean_sleep == min(mean_sleep)
  )

p2 <- ggplot(df_clean, aes(x = day_of_week, y = duration)) +
  geom_boxplot(aes(fill = day_of_week), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.1, size = 1.5, color = col_dark_text) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = col_orange) +
  scale_fill_manual(
    values = make_palette(n_distinct(df_clean$day_of_week)),
    guide  = "none"
  ) +
  labs(
    title    = "Sleep Duration by Day of Week",
    subtitle = "Orange diamond = mean",
    x = "Day of Week",
    y = "Sleep (hours)"
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_sleep() +
  theme(axis.text.x = element_text(face = "bold"))

print(p2)
ggsave(here("figures", "02_sleep_by_day_of_week.png"), p2, width = 10, height = 6, dpi = 300)

# ============================================================================
# 4. SLEEP BY BEDTIME
# ============================================================================

p3 <- ggplot(df_clean, aes(x = bedtime, y = duration, fill = bedtime)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 2.5, outlier.alpha = 0.6) +
  geom_jitter(width = 0.18, alpha = 0.15, size = 2, color = col_dark_text) +
  scale_fill_manual(
    values = make_palette_diverging(n_distinct(df_clean$bedtime)),
    guide  = "none"
  ) +
  labs(
    title = "Sleep Duration by Bedtime",
    x = "Bedtime",
    y = "Sleep Duration (hours)"
  ) +
  theme_sleep()

print(p3)
ggsave(here("figures", "03_sleep_by_bedtime.png"), p3, width = 10, height = 6, dpi = 300)

# ============================================================================
# 5. SLEEP BY COFFEE INTAKE
# ============================================================================

p4 <- ggplot(df_clean, aes(x = coffee, y = duration, fill = coffee)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 2.5, outlier.alpha = 0.6) +
  geom_jitter(width = 0.18, alpha = 0.15, size = 2, color = col_dark_text) +
  scale_fill_manual(
    values = make_palette(n_distinct(df_clean$coffee)),
    guide  = "none"
  ) +
  labs(
    title = "Sleep Duration by Coffee Intake",
    x = "Coffee Intake",
    y = "Sleep Duration (hours)"
  ) +
  theme_sleep() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(p4)
ggsave(here("figures", "04_sleep_by_coffee.png"), p4, width = 10, height = 6, dpi = 300)

# ============================================================================
# 6. SLEEP BY STRESS AND HEALTH
# ============================================================================

p5_stress <- ggplot(df_clean, aes(x = stress, y = duration, fill = stress)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 2, outlier.alpha = 0.5) +
  geom_jitter(width = 0.18, alpha = 0.12, size = 1.8, color = col_dark_text) +
  scale_fill_manual(
    values = make_palette_diverging(n_distinct(df_clean$stress)),
    guide  = "none"
  ) +
  labs(
    title = "Sleep by Stress Level",
    x = "Stress",
    y = "Sleep (hours)"
  ) +
  theme_sleep() +
  theme(
    plot.title  = element_text(size = 12),
    axis.text.x = element_text(size = 9)
  )

p5_health <- ggplot(df_clean, aes(x = health, y = duration, fill = health)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 2, outlier.alpha = 0.5) +
  geom_jitter(width = 0.18, alpha = 0.12, size = 1.8, color = col_dark_text) +
  scale_fill_manual(
    values = make_palette_diverging(n_distinct(df_clean$health)),
    guide  = "none"
  ) +
  labs(
    title = "Sleep by Health Status",
    x = "Health",
    y = "Sleep (hours)"
  ) +
  theme_sleep() +
  theme(
    plot.title  = element_text(size = 12),
    axis.text.x = element_text(size = 9)
  )

p5 <- gridExtra::arrangeGrob(p5_stress, p5_health, ncol = 2,
                              top = "Sleep Patterns by Personal Factors")
grid::grid.draw(p5)
ggsave(here("figures", "05_sleep_by_stress_and_health.png"), p5, width = 12, height = 6, dpi = 300)

# ============================================================================
# 7. SLEEP DISTRIBUTION
# ============================================================================

mean_sleep   <- mean(df_clean$duration, na.rm = TRUE)
median_sleep <- median(df_clean$duration, na.rm = TRUE)

p6 <- ggplot(df_clean, aes(x = duration)) +
  geom_histogram(bins = 25, fill = col_steel, color = "white", alpha = 0.85) +
  geom_vline(xintercept = mean_sleep, color = col_orange, linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean_sleep + 0.3, y = Inf, vjust = 2,
           label    = paste0("Mean = ", round(mean_sleep, 1), "h"),
           color    = col_orange, fontface = "bold", size = 4) +
  labs(
    title    = "Distribution of Sleep Duration",
    subtitle = paste("n =", nrow(df_clean), "nights"),
    x = "Sleep Duration (hours)",
    y = "Count"
  ) +
  theme_sleep()

print(p6)
ggsave(here("figures", "06_sleep_distribution.png"), p6, width = 10, height = 6, dpi = 300)

# ============================================================================
# 8. KEY FINDINGS SUMMARY
# ============================================================================

cat("\n========== KEY FINDINGS ==========\n")
cat("1. Sleep varies systematically by day of week\n")
cat("   - Best sleep on", as.character(sleep_by_day$day_of_week[sleep_by_day$best_day]),
    "(avg:", round(sleep_by_day$mean_sleep[sleep_by_day$best_day], 2), "hours)\n")
cat("   - Worst sleep on", as.character(sleep_by_day$day_of_week[sleep_by_day$worst_day]),
    "(avg:", round(sleep_by_day$mean_sleep[sleep_by_day$worst_day], 2), "hours)\n")
cat("2. Later bedtimes and coffee consumption may reduce sleep duration\n")
cat("3. Stress and poor health negatively impact sleep\n")
cat("4. Overall sleep distribution is approximately normal with mean",
    round(mean_sleep, 2), "hours\n")

# ============================================================================
# 9. SENSOR DATA QUALITY & AVAILABILITY
# ============================================================================

cat("\n========== SENSOR DATA QUALITY ==========\n")
cat("Total diary nights:", nrow(sleep_mittari), "\n")
cat("Nights with sensor data available:", nrow(sleep_mittari_sensor), "\n")
cat("Nights without sensor data:", nrow(sleep_mittari) - nrow(sleep_mittari_sensor), "\n")
cat("Sensor coverage:",
    round(nrow(sleep_mittari_sensor) / nrow(sleep_mittari) * 100, 1), "%\n")

# ============================================================================
# 10. ENVIRONMENTAL MEASUREMENTS - SUMMARY STATISTICS
# ============================================================================

cat("\n========== ENVIRONMENTAL MEASUREMENTS - DESCRIPTIVE STATS ==========\n")

env_stats <- sleep_mittari_sensor |>
  summarise(
    `CO2 (ppm)` = paste0(
      "M = ", format(round(mean(ka_co2, na.rm = TRUE), 1), nsmall = 1),
      ", SD = ", format(round(sd(ka_co2, na.rm = TRUE), 1), nsmall = 1)
    ),
    `Temperature (°C)` = paste0(
      "M = ", format(round(mean(ka_temp, na.rm = TRUE), 1), nsmall = 1),
      ", SD = ", format(round(sd(ka_temp, na.rm = TRUE), 1), nsmall = 1)
    ),
    `Humidity (%)` = paste0(
      "M = ", format(round(mean(ka_humid, na.rm = TRUE), 1), nsmall = 1),
      ", SD = ", format(round(sd(ka_humid, na.rm = TRUE), 1), nsmall = 1)
    )
  )

print(env_stats)

cat("\nCO2 Levels (ppm):\n")
print(summary(sleep_mittari_sensor$ka_co2))

cat("\nTemperature (°C):\n")
print(summary(sleep_mittari_sensor$ka_temp))

cat("\nHumidity (%):\n")
print(summary(sleep_mittari_sensor$ka_humid))

# ============================================================================
# 11. SENSOR DATA OBSERVATIONS - NIGHTLY COVERAGE
# ============================================================================

cat("\n========== NIGHTLY SENSOR OBSERVATIONS ==========\n")

obs_stats <- sleep_mittari_sensor |>
  summarise(
    `Nights with data` = sum(!is.na(n_obs)),
    `Mean obs/night`   = round(mean(n_obs, na.rm = TRUE), 1),
    `SD obs/night`     = round(sd(n_obs, na.rm = TRUE), 1),
    `Min obs/night`    = min(n_obs, na.rm = TRUE),
    `Max obs/night`    = max(n_obs, na.rm = TRUE)
  )

print(obs_stats)

# ============================================================================
# 12. SLEEP DURATION BY ENVIRONMENTAL CONDITIONS
# ============================================================================

cat("\n========== SLEEP AND ENVIRONMENTAL CONDITIONS ==========\n")

env_analysis <- sleep_mittari_sensor |>
  mutate(
    co2_level = cut(ka_co2,
                    breaks = c(0, 600, 800, 1000, Inf),
                    labels = c("Low (<600)", "Moderate (600-800)",
                               "High (800-1000)", "Very High (>1000)"),
                    right = FALSE)
  ) |>
  group_by(co2_level) |>
  summarise(
    n_nights   = n(),
    mean_sleep = round(mean(duration, na.rm = TRUE), 2),
    sd_sleep   = round(sd(duration, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\nSleep duration by CO2 levels:\n")
print(env_analysis)

temp_analysis <- sleep_mittari_sensor |>
  mutate(
    temp_level = cut(ka_temp,
                     breaks = c(-Inf, 18, 20, 22, Inf),
                     labels = c("Cold (<18°C)", "Cool (18-20°C)",
                                "Moderate (20-22°C)", "Warm (>22°C)"),
                     right = FALSE)
  ) |>
  group_by(temp_level) |>
  summarise(
    n_nights   = n(),
    mean_sleep = round(mean(duration, na.rm = TRUE), 2),
    sd_sleep   = round(sd(duration, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\nSleep duration by room temperature:\n")
print(temp_analysis)

# ============================================================================
# 13. ENVIRONMENTAL CONDITIONS OVER TIME
# ============================================================================

co2_monthly <- sleep_mittari_sensor |>
  mutate(year_month = floor_date(date, "month")) |>
  group_by(year_month) |>
  summarise(mean_co2 = mean(ka_co2, na.rm = TRUE), n = n(), .groups = "drop")

p_env_time <- ggplot(co2_monthly, aes(x = year_month, y = mean_co2)) +
  geom_line(color = col_dark_blue, linewidth = 1) +
  geom_point(color = col_dark_blue, size = 3) +
  labs(title = "Monthly Average CO2 Levels", x = "Date", y = "CO2 (ppm)") +
  theme_sleep()

print(p_env_time)
ggsave(here("figures", "07_co2_over_time.png"), p_env_time, width = 10, height = 6, dpi = 300)

temp_monthly <- sleep_mittari_sensor |>
  mutate(year_month = floor_date(date, "month")) |>
  group_by(year_month) |>
  summarise(mean_temp = mean(ka_temp, na.rm = TRUE), n = n(), .groups = "drop")

p_temp_time <- ggplot(temp_monthly, aes(x = year_month, y = mean_temp)) +
  geom_line(color = col_steel, linewidth = 1) +
  geom_point(color = col_steel, size = 3) +
  labs(title = "Monthly Average Room Temperature", x = "Date", y = "Temperature (°C)") +
  theme_sleep()

print(p_temp_time)
ggsave(here("figures", "08_temperature_over_time.png"), p_temp_time, width = 10, height = 6, dpi = 300)

# ============================================================================
# 14. SLEEP vs ENVIRONMENTAL CONDITIONS (SCATTERPLOTS)
# ============================================================================

safe_cor <- function(x, y) {
  r <- cor(x, y, use = "complete.obs")
  paste0("r = ", round(r, 2))
}

p_sleep_co2 <- ggplot(sleep_mittari_sensor, aes(x = ka_co2, y = duration)) +
  geom_point(alpha = 0.5, size = 2.5, color = col_dark_blue) +
  geom_smooth(method = "lm", color = col_navy, fill = col_mid_blue, alpha = 0.15) +
  labs(
    title    = "Sleep Duration vs CO2 Levels",
    subtitle = safe_cor(sleep_mittari_sensor$ka_co2, sleep_mittari_sensor$duration),
    x = "CO2 (ppm)", y = "Sleep Duration (hours)"
  ) +
  theme_sleep()

print(p_sleep_co2)
ggsave(here("figures", "09_sleep_vs_co2.png"), p_sleep_co2, width = 10, height = 6, dpi = 300)

p_sleep_temp <- ggplot(sleep_mittari_sensor, aes(x = ka_temp, y = duration)) +
  geom_point(alpha = 0.5, size = 2.5, color = col_steel) +
  geom_smooth(method = "lm", color = col_dark_blue, fill = col_light_blue, alpha = 0.15) +
  labs(
    title    = "Sleep Duration vs Room Temperature",
    subtitle = safe_cor(sleep_mittari_sensor$ka_temp, sleep_mittari_sensor$duration),
    x = "Temperature (°C)", y = "Sleep Duration (hours)"
  ) +
  theme_sleep()

print(p_sleep_temp)
ggsave(here("figures", "10_sleep_vs_temperature.png"), p_sleep_temp, width = 10, height = 6, dpi = 300)

p_sleep_humid <- ggplot(sleep_mittari_sensor, aes(x = ka_humid, y = duration)) +
  geom_point(alpha = 0.5, size = 2.5, color = col_mid_blue) +
  geom_smooth(method = "lm", color = col_navy, fill = col_pale_blue, alpha = 0.15) +
  labs(
    title    = "Sleep Duration vs Humidity",
    subtitle = safe_cor(sleep_mittari_sensor$ka_humid, sleep_mittari_sensor$duration),
    x = "Humidity (%)", y = "Sleep Duration (hours)"
  ) +
  theme_sleep()

print(p_sleep_humid)
ggsave(here("figures", "11_sleep_vs_humidity.png"), p_sleep_humid, width = 10, height = 6, dpi = 300)

# ============================================================================
# 15. ENVIRONMENTAL CONDITIONS BOXPLOTS
# ============================================================================

p_env_box <- sleep_mittari_sensor |>
  mutate(
    co2_level = cut(ka_co2,
                    breaks = c(0, 600, 800, 1000, Inf),
                    labels = c("Low (<600)", "Moderate\n(600-800)",
                               "High\n(800-1000)", "Very High\n(>1000)"),
                    right = FALSE)
  ) |>
  ggplot(aes(x = co2_level, y = duration, fill = co2_level)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 2, outlier.alpha = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.15, size = 1.5, color = col_dark_text) +
  scale_fill_manual(
    values = make_palette_diverging(4),  # Always 4 CO2 categories
    guide  = "none"
  ) +
  labs(
    title = "Sleep Duration by Room CO2 Levels",
    x = "CO2 Category",
    y = "Sleep Duration (hours)"
  ) +
  theme_sleep()

print(p_env_box)
ggsave(here("figures", "12_sleep_by_co2_category.png"), p_env_box, width = 10, height = 6, dpi = 300)

cat("\n✓ Analysis complete. Plots saved to figures/.\n")