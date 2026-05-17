# =============================================================================
# run_core_pipeline.R
#
# Purpose: Run the main sleep diary analysis pipeline in the intended order.
#
# Use this script when you want to regenerate the core analysis outputs from the
# numbered pipeline scripts. Variable-specific reports are run separately with
# scripts/run_variable_specific_reports.R.
# =============================================================================

library(here)

core_scripts <- c(
  "01_load_main_data.R",
  "02_load_co2_temp_data.R",
  "03_join_relevant_data.R",
  "04_descriptives_and_plots.R",
  "05_models.R",
  "06_fixed_effects_models.R"
)

cat("\n========== RUNNING CORE PIPELINE ==========\n")

purrr::walk(
  core_scripts,
  \(script_name) {
    script_path <- here("scripts", script_name)

    if (!file.exists(script_path)) {
      stop("Could not find pipeline script: ", script_path)
    }

    cat("\n--- Running ", script_name, " ---\n", sep = "")
    source(script_path)
  }
)

cat("\n✓ Core pipeline completed successfully.\n")
