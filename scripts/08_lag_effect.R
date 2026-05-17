# =============================================================================
# 08_lag_effect.R
#
# Purpose: Examine how sleep duration of previous nights (1, 2, or 3 nights
#          back) impacts current sleep duration.
#
# Approach:
#   - Create lagged duration variables based on diary-night order
#     (data are already arranged by date in df_clean).
#   - Fit simple OLS models:
#       (1) duration_t ~ duration_{t-1}
#       (2) duration_t ~ duration_{t-2}
#       (3) duration_t ~ duration_{t-3}
#       (4) duration_t ~ duration_{t-1} + duration_{t-2} + duration_{t-3}
#   - Save coefficient table + a small coefficient plot.
#
# Input:
#   - df_clean from scripts/01_load_main_data.R
#
# Output:
#   - outputs/08_lag_effect_models.txt
#   - figures/19_lag_effect_coefficients.png
# =============================================================================

library(tidyverse)
library(broom)
library(here)

source(here("scripts", "01_load_main_data.R"))

if (!exists("df_clean")) stop("df_clean not found. Run 01_load_main_data.R first.")

dir.create(here("figures"), showWarnings = FALSE)
dir.create(here("outputs"), showWarnings = FALSE)

# =============================================================================
# DATA PREP
# =============================================================================

dat <- df_clean |>
  arrange(date) |>
  mutate(
    lag1_duration = lag(duration, 1),
    lag2_duration = lag(duration, 2),
    lag3_duration = lag(duration, 3)
  )

cat("\n========== LAG EFFECT SAMPLE SIZES ==========\n")
cat("Total nights:", nrow(dat), "\n")
cat("Complete for lag1:", sum(!is.na(dat$lag1_duration)), "\n")
cat("Complete for lag2:", sum(!is.na(dat$lag2_duration)), "\n")
cat("Complete for lag3:", sum(!is.na(dat$lag3_duration)), "\n")
cat(
  "Complete for lag1+lag2+lag3:",
  sum(complete.cases(dat[, c("lag1_duration", "lag2_duration", "lag3_duration")])),
  "\n"
)

# =============================================================================
# MODELS
# =============================================================================

dat_lag1 <- dat |> filter(!is.na(lag1_duration))
dat_lag2 <- dat |> filter(!is.na(lag2_duration))
dat_lag3 <- dat |> filter(!is.na(lag3_duration))
dat_all_lags <- dat |> filter(complete.cases(cbind(lag1_duration, lag2_duration, lag3_duration)))

m_lag1 <- lm(duration ~ lag1_duration, data = dat_lag1)
m_lag2 <- lm(duration ~ lag2_duration, data = dat_lag2)
m_lag3 <- lm(duration ~ lag3_duration, data = dat_lag3)
m_all <- lm(duration ~ lag1_duration + lag2_duration + lag3_duration, data = dat_all_lags)

cat("\n========== MODEL SUMMARIES ==========\n")
cat("\n--- duration_t ~ duration_{t-1} ---\n")
print(summary(m_lag1))
cat("\n--- duration_t ~ duration_{t-2} ---\n")
print(summary(m_lag2))
cat("\n--- duration_t ~ duration_{t-3} ---\n")
print(summary(m_lag3))
cat("\n--- duration_t ~ duration_{t-1} + duration_{t-2} + duration_{t-3} ---\n")
print(summary(m_all))

# =============================================================================
# COEFFICIENT TABLE + PLOT
# =============================================================================

tidy_model <- function(model, model_name) {
  tidy(model, conf.int = TRUE) |>
    filter(term != "(Intercept)") |>
    mutate(model = model_name)
}

term_label <- function(term) {
  term |>
    str_replace("^lag1_duration$", "Lag 1 night") |>
    str_replace("^lag2_duration$", "Lag 2 nights") |>
    str_replace("^lag3_duration$", "Lag 3 nights")
}

coef_tbl <- bind_rows(
  tidy_model(m_lag1, "Lag 1"),
  tidy_model(m_lag2, "Lag 2"),
  tidy_model(m_lag3, "Lag 3"),
  tidy_model(m_all, "All lags (1-3)")
) |>
  mutate(
    term_label = term_label(term),
    estimate = estimate,
    conf_low = conf.low,
    conf_high = conf.high
  ) |>
  select(model, term, term_label, estimate, conf_low, conf_high, p.value)

coef_tbl_out <- coef_tbl |>
  mutate(
    across(
      c(estimate, conf_low, conf_high),
      \(x) round(x, 3)
    ),
    across(p.value, \(x) round(x, 4))
  )

capture.output(
  coef_tbl_out,
  file = here("outputs", "08_lag_effect_models.txt")
)

col_navy <- "#002d5a"
col_steel <- "#4a7ba7"
col_orange <- "#CC5500"

theme_sleep_simple <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 11, color = "grey40", hjust = 0),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90")
    )
}

coef_plot <- ggplot(
  coef_tbl,
  aes(
    x = estimate,
    y = term_label,
    color = model
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(
    aes(xmin = conf_low, xmax = conf_high),
    size = 0.7,
    linewidth = 0.9
  ) +
  scale_color_manual(
    values = c(
      "Lag 1" = col_navy,
      "Lag 2" = col_steel,
      "Lag 3" = col_orange,
      "All lags (1-3)" = "#2f4a73"
    )
  ) +
  labs(
    title = "Lagged sleep duration effects",
    subtitle = "OLS: current sleep duration vs previous 1-3 nights",
    x = "Estimate (hours)",
    y = NULL,
    color = NULL
  ) +
  theme_sleep_simple()

ggsave(
  here("figures", "19_lag_effect_coefficients.png"),
  coef_plot,
  width = 9,
  height = 4.8,
  dpi = 300
)

cat("\nSaved coefficient table to outputs/08_lag_effect_models.txt\n")
cat("Saved coefficient plot to figures/19_lag_effect_coefficients.png\n")
