# =============================================================================
# run_variable_specific_reports.R
#
# Purpose: Run selected variable-specific reporting scripts.
#
# By default, this script runs the current variable-specific reports one by one.
# To run only selected reports from an interactive R session, set:
#   reports_to_run <- c("bedtime", "exercise")
# before sourcing this file.
# =============================================================================

library(here)

available_reports <- c(
  "bedtime",
  "weekday",
  "insomnia",
  "exercise"
)

if (!exists("reports_to_run")) {
  reports_to_run <- available_reports
}

unknown_reports <- setdiff(reports_to_run, available_reports)

if (length(unknown_reports) > 0) {
  stop("Unknown variable-specific reports: ", paste(unknown_reports, collapse = ", "))
}

cat("\n========== RUNNING VARIABLE-SPECIFIC REPORTS ==========\n")

purrr::walk(
  reports_to_run,
  \(report_name) {
    script_path <- here("scripts", "variable_specific", paste0(report_name, ".R"))

    if (!file.exists(script_path)) {
      stop("Could not find variable-specific script: ", script_path)
    }

    cat("\n--- Running ", report_name, " ---\n", sep = "")
    source(script_path)
  }
)

cat("\n✓ Variable-specific reports completed successfully.\n")
