# =============================================================================
# Magnesium use during the diary experiment
#
# The provisional start is one day before the first recorded use, as requested
# by the diary owner. Earlier non-use nights are not a control group. The diary
# records use, not the original coin tosses, adherence, or blinding. Estimates
# therefore describe recorded use within the proposed experiment period.
# =============================================================================

library(tidyverse)
library(fixest)
library(here)
library(patchwork)
source(here("scripts", "01_load_main_data.R"))

variable_name <- "magnesium"
figure_dir <- here("outputs", "figures", "variable_specific", variable_name)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Set an explicit date here if the original experiment record establishes it.
magnesium_start_date <- NULL
first_use <- as.Date(df_raw$aika)[which(suppressWarnings(clean_numeric(df_raw$Mg)) == 1)]
if (length(first_use) == 0) stop("No magnesium-use dates are recorded.")
trial_start <- if (is.null(magnesium_start_date)) min(first_use) - 1L else as.Date(magnesium_start_date)

col_navy <- "#002d5a"
col_dark_blue <- "#2f4a73"
col_light_blue <- "#a3c1d9"
col_orange <- "#CC5500"
col_dark_text <- "#2a2a2a"
col_grey <- "grey40"
magnesium_palette <- c("No magnesium" = col_light_blue, "Magnesium" = col_orange)

theme_sleep <- function() {
  theme_minimal(base_size = 13) +
    theme(plot.title = element_text(size = 15, face = "bold", hjust = 0),
          plot.subtitle = element_text(size = 11, color = col_grey, hjust = 0),
          plot.caption = element_text(size = 9, color = "grey50", hjust = 0),
          plot.margin = margin(15, 15, 15, 15),
          axis.title = element_text(size = 12), axis.text = element_text(size = 10),
          legend.position = "bottom", legend.title = element_blank(),
          panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
}

dat_magnesium <- sleep_diary_all |>
  arrange(date) |>
  mutate(magnesium_num = Mg,
         prev_magnesium = lag_by_calendar_days(Mg, date),
         prev_duration = lag_by_calendar_days(duration, date),
         coffee_any = case_when(coffee_code == 0 ~ 0L, coffee_code %in% 1:3 ~ 1L),
         medicine_any = case_when(unilaake == 0 ~ 0L, unilaake %in% 1:2 ~ 1L),
         day_index = as.integer(date - trial_start),
         magnesium = factor(Mg, levels = 0:1, labels = names(magnesium_palette))) |>
  filter(date >= trial_start, Mg %in% 0:1, !is.na(duration))

if (nrow(dat_magnesium) < 10 || n_distinct(dat_magnesium$Mg) < 2) {
  stop("The experiment period needs at least 10 observed nights and both magnesium groups.")
}

magnesium_summary <- dat_magnesium |>
  group_by(magnesium) |>
  summarise(n = n(), mean_sleep = mean(duration), median_sleep = median(duration),
            mean_previous_sleep = mean(prev_duration, na.rm = TRUE), .groups = "drop")

dat_model <- dat_magnesium |>
  drop_na(duration, magnesium_num, prev_magnesium, prev_duration, coffee_any, medicine_any)

formulas <- list(
  "Raw" = duration ~ magnesium_num,
  "Time and previous sleep" = duration ~ magnesium_num + day_index + prev_duration,
  "Plus previous magnesium" = duration ~ magnesium_num + day_index + prev_duration + prev_magnesium,
  "Plus coffee and medication" = duration ~ magnesium_num + day_index + prev_duration +
    prev_magnesium + coffee_any + medicine_any
)
models <- purrr::map(formulas, \(fml) fit_nw(fml, dat_model))
model_results <- purrr::imap_dfr(models, function(model, name) {
  bounds <- unlist(confint(model, parm = "magnesium_num"), use.names = FALSE)
  tibble(model = name, n = nobs(.env$model),
         estimate_minutes = unname(coef(.env$model)["magnesium_num"]) * 60,
         ci_low_minutes = bounds[1] * 60, ci_high_minutes = bounds[2] * 60)
}) |>
  mutate(model = factor(model, levels = rev(names(models))))

cat("\n========== MAGNESIUM EXPERIMENT PERIOD ==========\n")
cat("Provisional start:", format(trial_start), "\n")
cat("Recorded nights:", nrow(dat_magnesium), "\n")
cat("Unobserved or excluded nights in this interval:",
    as.integer(max(dat_magnesium$date) - trial_start) + 1L - nrow(dat_magnesium), "\n")
cat("Common model sample:", nrow(dat_model), "\n")
print(magnesium_summary, n = Inf)
print(model_results, n = Inf)
purrr::iwalk(models, \(model, name) {
  cat("\n---", name, "---\n")
  print(summary(model))
})
cat("The start date is provisional. Use records do not independently verify randomized assignment.\n")
cat("Previous-use adjustment checks one-day carryover only; it does not rule out longer carryover.\n")

p_timeline <- dat_magnesium |>
  prepare_nw_data() |>
  ggplot(aes(x = date, y = duration)) +
  geom_line(aes(group = series_id), color = col_dark_blue, linewidth = 0.45, alpha = 0.6) +
  geom_point(aes(color = magnesium), size = 2) +
  scale_color_manual(values = magnesium_palette) +
  scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
  labs(title = "Sleep during the experiment period", subtitle = paste("Provisional start:", trial_start),
       x = NULL, y = "Sleep duration (hours)") + theme_sleep() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_distribution <- dat_magnesium |>
  ggplot(aes(x = magnesium, y = duration, fill = magnesium)) +
  geom_boxplot(width = 0.62, alpha = 0.75, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.3, color = col_dark_text) +
  scale_fill_manual(values = magnesium_palette, guide = "none") +
  coord_cartesian(ylim = c(0, NA)) +
  labs(title = "Observed sleep distributions", subtitle = paste0("N = ", nrow(dat_magnesium), " recorded nights"),
       x = NULL, y = "Sleep duration (hours)") + theme_sleep()

estimate_plot <- function(data) {
  ggplot(data, aes(x = estimate_minutes, y = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.35) +
    geom_segment(aes(x = ci_low_minutes, xend = ci_high_minutes, yend = model),
                 color = col_dark_blue, linewidth = 1.1) +
    geom_point(color = col_orange, size = 2.7) +
    labs(x = "Difference in sleep (minutes)", y = NULL) + theme_sleep() +
    theme(panel.grid.major.x = element_line(color = "grey90"))
}
p_estimates <- model_results |>
  filter(model != "Plus coffee and medication") |>
  estimate_plot() +
  labs(title = "Magnesium versus no magnesium", subtitle = "Same model sample; 95% Newey-West intervals")

p_previous <- dat_magnesium |>
  drop_na(prev_duration) |>
  ggplot(aes(x = magnesium, y = prev_duration, fill = magnesium)) +
  geom_boxplot(width = 0.62, alpha = 0.75, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.3, color = col_dark_text) +
  scale_fill_manual(values = magnesium_palette, guide = "none") +
  coord_cartesian(ylim = c(0, NA)) +
  labs(title = "Sleep before magnesium use", subtitle = "Previous-night context in the two groups",
       x = NULL, y = "Previous-night sleep (hours)") + theme_sleep()

p_main <- (p_timeline + p_distribution) / (p_estimates + p_previous) +
  plot_layout(guides = "collect", widths = c(1, 1), heights = c(1, 1)) +
  plot_annotation(
    title = "Recorded magnesium use and sleep",
    subtitle = "A within-person comparison during the proposed experiment period",
    caption = str_wrap(paste(
      "The start is one day before the first recorded magnesium use. Earlier years are excluded.",
      "Use was described as coin-toss based, but original assignments and adherence are unavailable.",
      "These estimates describe recorded use; the period and carryover assumptions require confirmation."
    ), 145), tag_levels = "A",
    theme = theme(plot.title = element_text(size = 17, face = "bold"),
                  plot.subtitle = element_text(size = 11, color = col_grey),
                  plot.caption = element_text(size = 8.2, color = "grey45", hjust = 0))) &
  theme(plot.title = element_text(size = 11.5, face = "bold", hjust = 0),
        plot.subtitle = element_text(size = 8.8, color = col_grey, hjust = 0),
        plot.margin = margin(8, 8, 8, 8), axis.title = element_text(size = 10),
        axis.text = element_text(size = 8.5), legend.position = "bottom",
        legend.text = element_text(size = 8.5))

p_sensitivity <- estimate_plot(model_results) +
  labs(title = "Magnesium estimates across adjustment steps",
       subtitle = "Same nights in every model; 95% Newey-West intervals",
       caption = "Coffee and medication adjustment is exploratory in this small sample.")

ggsave(file.path(figure_dir, "magnesium_figure1_main.png"), p_main,
       width = 10, height = 12.5, dpi = 300, bg = "white")
ggsave(file.path(figure_dir, "magnesium_figureS1_sensitivity.png"), p_sensitivity,
       width = 10, height = 6, dpi = 300, bg = "white")
print(p_main)
cat("\nFigures saved to:", figure_dir, "\n")
