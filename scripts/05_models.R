# =============================================================================
# 05_models.R
#
# Purpose: Fit and evaluate regression models for sleep duration and insomnia
#
# Strategy:
#   Model 1 (base):     duration ~ bedtime + stress + health
#   Model 2 (extended): duration ~ bedtime + stress + health + coffee + day_of_week
#   Model 3 (logistic): insomnia (any vs none) ~ bedtime + stress + health + coffee
#
# Models are compared via AIC/BIC and nested F-test. Coefficient plots and
# diagnostic plots are saved to figures/.
#
# Input:  df_clean from 01_load_main_data.R
# Output: Model summaries, comparison table, figures
# =============================================================================

library(tidyverse)
library(broom)
library(here)

source(here("scripts", "01_load_main_data.R"))

if (!exists("df_clean")) stop("df_clean not found. Run 01_load_main_data.R first.")

dir.create(here("figures"), showWarnings = FALSE)

# --- Color system (same as 04_descriptives_and_plots.R) ----------------------
col_navy       <- "#002d5a"
col_dark_blue  <- "#2f4a73"
col_steel      <- "#4a7ba7"
col_mid_blue   <- "#6c8eb5"
col_light_blue <- "#a3c1d9"
col_pale_blue  <- "#d0e1ef"
col_orange     <- "#CC5500"
col_amber      <- "#e8a317"
col_dark_text  <- "#2a2a2a"

theme_sleep <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(size = 15, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 11, color = "grey40", hjust = 0),
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
# PREPARE MODELLING DATA
# =============================================================================

# Drop rows with any NA in model variables; create binary insomnia indicator
df_model <- df_clean |>
  select(duration, bedtime, coffee, stress, health, day_of_week, insomnia_num) |>
  drop_na() |>
  mutate(insomnia_bin = factor(ifelse(insomnia_num > 0, 1, 0)))

cat("\n========== MODELLING SAMPLE ==========\n")
cat("Observations available:", nrow(df_model), "\n")
cat("Insomnia prevalence:",
    round(mean(df_model$insomnia_bin == 1) * 100, 1), "%\n")

# =============================================================================
# MODEL 1 (BASE): duration ~ bedtime + stress + health
#
# Rationale: These are the primary behavioral and health factors that theory
# and prior research link most directly to sleep duration.
# =============================================================================

m1 <- lm(duration ~ bedtime + stress + health, data = df_model)

cat("\n========== MODEL 1: BASE ==========\n")
summary(m1)

# =============================================================================
# MODEL 2 (EXTENDED): + coffee + day_of_week
#
# Rationale: Coffee timing and weekly rhythm are plausible additional
# predictors. We test whether they improve fit beyond the base model.
# =============================================================================

m2 <- lm(duration ~ bedtime + stress + health + coffee + day_of_week,
          data = df_model)

cat("\n========== MODEL 2: EXTENDED ==========\n")
summary(m2)

# =============================================================================
# MODEL COMPARISON: m1 vs m2
# =============================================================================

cat("\n========== MODEL COMPARISON (OLS) ==========\n")

# Nested F-test
f_test <- anova(m1, m2)
cat("\nNested F-test (m1 vs m2):\n")
print(f_test)

# AIC / BIC comparison
comparison <- tibble(
  Model    = c("M1: Base", "M2: Extended"),
  R2       = c(summary(m1)$r.squared, summary(m2)$r.squared),
  Adj_R2   = c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared),
  AIC      = c(AIC(m1), AIC(m2)),
  BIC      = c(BIC(m1), BIC(m2)),
  RMSE     = c(sigma(m1), sigma(m2))
)

cat("\nModel fit comparison:\n")
print(comparison, n = Inf, width = Inf)

# =============================================================================
# MODEL 3 (LOGISTIC): insomnia ~ bedtime + stress + health + coffee
#
# Rationale: Insomnia is a distinct outcome from sleep duration. We model the
# probability of any insomnia occurrence (binary) as a function of the same
# behavioral predictors. Day-of-week omitted to keep the model parsimonious
# (insomnia is less likely to follow weekly rhythm than duration).
# =============================================================================

m3 <- glm(insomnia_bin ~ bedtime + stress + health + coffee,
           data = df_model, family = binomial(link = "logit"))

cat("\n========== MODEL 3: LOGISTIC (INSOMNIA) ==========\n")
summary(m3)

# Odds ratios with confidence intervals
cat("\nOdds Ratios:\n")
or_table <- tidy(m3, conf.int = TRUE, exponentiate = TRUE) |>
  filter(term != "(Intercept)") |>
  select(term, OR = estimate, lower = conf.low, upper = conf.high, p.value) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))
print(or_table, n = Inf)

# =============================================================================
# VISUALIZATION 1: COEFFICIENT PLOT — OLS MODELS
# =============================================================================

coef_data <- bind_rows(
  tidy(m1, conf.int = TRUE) |> mutate(model = "M1: Base"),
  tidy(m2, conf.int = TRUE) |> mutate(model = "M2: Extended")
) |>
  filter(term != "(Intercept)") |>
  mutate(term = fct_reorder(term, estimate))

p_coef_ols <- ggplot(coef_data, aes(x = estimate, y = term, color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.5),
    size = 0.6, linewidth = 0.8
  ) +
  scale_color_manual(values = c("M1: Base" = col_navy, "M2: Extended" = col_steel)) +
  labs(
    title = "OLS Coefficient Estimates",
    subtitle = "Sleep duration (hours) — 95% confidence intervals",
    x = "Estimate (hours)",
    y = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

print(p_coef_ols)
ggsave(here("figures", "13_ols_coefficients.png"), p_coef_ols,
       width = 10, height = 7, dpi = 300)

# =============================================================================
# VISUALIZATION 2: ODDS RATIO PLOT — LOGISTIC MODEL
# =============================================================================

or_plot_data <- tidy(m3, conf.int = TRUE, exponentiate = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(term = fct_reorder(term, estimate))

p_or <- ggplot(or_plot_data, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    color = col_orange, size = 0.7, linewidth = 0.9
  ) +
  labs(
    title = "Odds Ratios for Insomnia",
    subtitle = "Logistic regression — 95% confidence intervals",
    x = "Odds Ratio",
    y = NULL
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

print(p_or)
ggsave(here("figures", "14_insomnia_odds_ratios.png"), p_or,
       width = 10, height = 6, dpi = 300)

# =============================================================================
# VISUALIZATION 3: DIAGNOSTICS — EXTENDED OLS MODEL (m2)
# =============================================================================

diag_data <- augment(m2)

# Residuals vs Fitted
p_diag1 <- ggplot(diag_data, aes(x = .fitted, y = .resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(alpha = 0.3, size = 2, color = col_dark_blue) +
  geom_smooth(method = "loess", se = FALSE, color = col_orange, linewidth = 0.8) +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted (hours)",
    y = "Residual"
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

# Q-Q plot
p_diag2 <- ggplot(diag_data, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.4, color = col_dark_blue) +
  stat_qq_line(color = col_orange, linewidth = 0.8) +
  labs(
    title = "Normal Q-Q Plot",
    subtitle = "Standardized residuals",
    x = "Theoretical Quantiles",
    y = "Standardized Residuals"
  ) +
  theme_sleep() +
  theme(panel.grid.major.x = element_line(color = "grey90"))

p_diag <- gridExtra::arrangeGrob(p_diag1, p_diag2, ncol = 2,
                                  top = "Model 2 Diagnostics")
grid::grid.draw(p_diag)
ggsave(here("figures", "15_model_diagnostics.png"), p_diag,
       width = 14, height = 6, dpi = 300)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n========== MODELLING SUMMARY ==========\n")
cat("M1 (base):     R² =", round(summary(m1)$r.squared, 3),
    "| Adj R² =", round(summary(m1)$adj.r.squared, 3), "\n")
cat("M2 (extended): R² =", round(summary(m2)$r.squared, 3),
    "| Adj R² =", round(summary(m2)$adj.r.squared, 3), "\n")
cat("M2 vs M1 F-test p =", format.pval(f_test$`Pr(>F)`[2], digits = 3), "\n")
cat("M3 (logistic): AIC =", round(AIC(m3), 1), "\n")
cat("\n✓ Models fitted and figures saved to figures/.\n")