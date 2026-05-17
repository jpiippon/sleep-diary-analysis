# =============================================================================
# 07_coffee_impact_analysis.R
#
# Purpose: Analyze and visualize the impact of coffee consumption on sleep.
#
# Dependencies: 01_load_main_data.R (provides sleep_diary)
#
# Output:
#   - Comprehensive visualization showing coffee impact on sleep duration
#   - Statistical summary table
# =============================================================================

library(tidyverse)
library(here)

# Load the main data
source(here("scripts", "01_load_main_data.R"))

# =============================================================================
# Analysis: Coffee Impact on Sleep
# =============================================================================

coffee_sleep_summary <- sleep_diary |>
  filter(!is.na(coffee), !is.na(duration)) |>
  group_by(coffee) |>
  summarise(
    n = n(),
    mean_duration = mean(duration, na.rm = TRUE),
    sd_duration = sd(duration, na.rm = TRUE),
    se_duration = sd_duration / sqrt(n),
    min_duration = min(duration, na.rm = TRUE),
    max_duration = max(duration, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    ci_lower = mean_duration - 1.96 * se_duration,
    ci_upper = mean_duration + 1.96 * se_duration
  )

# Print summary
cat("\n=== Coffee Impact on Sleep Duration ===\n")
print(coffee_sleep_summary)

# =============================================================================
# Visualization: Beautiful Bar Chart
# =============================================================================

coffee_colors <- c(
  "None" = "#2ecc71",           # Green
  "Half a cup in the morning" = "#f39c12",  # Orange
  "A cup or two before noon" = "#e74c3c",   # Red
  "Coffee after noon" = "#c0392b"           # Dark Red
)

plot_coffee_impact <- coffee_sleep_summary |>
  ggplot(aes(x = reorder(coffee, mean_duration), 
             y = mean_duration,
             fill = coffee)) +
  geom_col(width = 0.6, alpha = 0.8, color = "white", size = 1) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    linewidth = 0.7,
    color = "gray30"
  ) +
  geom_text(
    aes(y = mean_duration + se_duration + 0.3, 
        label = paste0(round(mean_duration, 1), "h")),
    size = 5,
    fontface = "bold",
    color = "gray20"
  ) +
  geom_text(
    aes(y = 0.3, label = paste0("n=", n)),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = coffee_colors,
    guide = "none"
  ) +
  scale_y_continuous(
    limits = c(0, max(coffee_sleep_summary$ci_upper) * 1.1),
    breaks = seq(0, 10, by = 1),
    labels = function(x) paste0(x, "h")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "gray20",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 12,
      color = "gray50",
      margin = margin(b = 15)
    ),
    axis.title = element_text(
      size = 12,
      face = "bold",
      color = "gray40"
    ),
    axis.text = element_text(
      size = 11,
      color = "gray50"
    ),
    axis.text.x = element_text(
      hjust = 0.5,
      margin = margin(t = 5)
    ),
    axis.line = element_line(
      color = "gray80",
      size = 0.5
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "gray90",
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    title = "☕ How Does Coffee Impact Your Sleep?",
    subtitle = "Average sleep duration by coffee consumption timing (with 95% confidence intervals)",
    x = "Coffee Consumption",
    y = "Sleep Duration"
  )

print(plot_coffee_impact)

# Save the plot
ggsave(
  here("outputs", "07_coffee_impact_analysis.png"),
  plot_coffee_impact,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("\n✓ Plot saved to outputs/07_coffee_impact_analysis.png\n")

# =============================================================================
# Additional Insight: Coffee Impact by Day of Week
# =============================================================================

cat("\n=== Coffee Impact by Day of Week ===\n")

coffee_dayofweek <- sleep_diary |>
  filter(!is.na(coffee), !is.na(duration), !is.na(day_of_week)) |>
  group_by(coffee, day_of_week) |>
  summarise(
    mean_duration = mean(duration, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 2) # Only show combinations with at least 2 observations

plot_coffee_weekday <- coffee_dayofweek |>
  ggplot(aes(x = day_of_week, y = mean_duration, fill = coffee)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.8, color = "white") +
  scale_fill_manual(values = coffee_colors) +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 1),
    labels = function(x) paste0(x, "h")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "gray20"),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Sleep Duration: Coffee Consumption by Day of Week",
    x = "Day of Week",
    y = "Sleep Duration"
  )

print(plot_coffee_weekday)

ggsave(
  here("outputs", "07_coffee_impact_by_weekday.png"),
  plot_coffee_weekday,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("\n✓ Weekday plot saved to outputs/07_coffee_impact_by_weekday.png\n")
