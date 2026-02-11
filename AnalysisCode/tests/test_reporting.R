# Phase 5 tests: Reporting and pipeline
# Run from Cachexia project root: Rscript AnalysisCode/tests/test_reporting.R

args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grepl("^--file=", args)])
script_dir <- if (length(script_path)) dirname(normalizePath(script_path[1], mustWork = FALSE)) else getwd()
analysis_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/")
cachexia_root <- normalizePath(file.path(analysis_dir, ".."), winslash = "/")
setwd(cachexia_root)
CACHEXIA_ROOT <- cachexia_root
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "R", "data_loader.R"))
source(file.path(analysis_dir, "R", "bcs_analysis.R"))
source(file.path(analysis_dir, "R", "weight_analysis.R"))
source(file.path(analysis_dir, "R", "digital_cage_analysis.R"))
source(file.path(analysis_dir, "R", "onset_detection.R"))
source(file.path(analysis_dir, "R", "sensitivity_analysis.R"))
source(file.path(analysis_dir, "R", "reporting.R"))

run_test <- function(name, expr) {
  tryCatch(
    { expr; cat("  OK:", name, "\n"); TRUE },
    error = function(e) { cat("  FAIL:", name, "-", conditionMessage(e), "\n"); FALSE }
  )
}

cat("Phase 5 tests: Reporting and pipeline\n")
all_ok <- TRUE

# Run full pipeline with small digital sample for speed
pipeline <- run_full_pipeline(digital_n_max = 20000L)
all_ok <- run_test("run_full_pipeline returns list", stopifnot(is.list(pipeline))) && all_ok
all_ok <- run_test("pipeline has merged", stopifnot("merged" %in% names(pipeline))) && all_ok
all_ok <- run_test("pipeline has bcs_onset, weight_onset, manual_onset", stopifnot(
  all(c("bcs_onset", "weight_onset", "manual_onset") %in% names(pipeline)))) && all_ok
all_ok <- run_test("pipeline has effect_sizes", stopifnot("effect_sizes" %in% names(pipeline))) && all_ok
all_ok <- run_test("effect_sizes is data.frame with outcome, cohens_d", stopifnot(
  is.data.frame(pipeline$effect_sizes),
  all(c("outcome", "cohens_d") %in% names(pipeline$effect_sizes)))) && all_ok

# Summarize
summary_list <- summarize_cachexia_results(pipeline, run_pipeline_if_null = FALSE)
all_ok <- run_test("summarize_cachexia_results returns list", stopifnot(is.list(summary_list))) && all_ok
all_ok <- run_test("summary has bcs_summary, weight_summary, effect_sizes", stopifnot(
  all(c("bcs_summary", "weight_summary", "effect_sizes") %in% names(summary_list)))) && all_ok

# Export to temp dir
out_dir <- file.path(cachexia_root, "AnalysisCode", "output_phase5_test")
if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
written <- export_results(pipeline, output_dir = out_dir, prefix = "phase5_")
all_ok <- run_test("export_results returns character vector", stopifnot(is.character(written))) && all_ok
all_ok <- run_test("export_results wrote at least one file", stopifnot(length(written) >= 1L)) && all_ok
all_ok <- run_test("exported files exist", stopifnot(all(file.exists(written)))) && all_ok
# Cleanup
unlink(out_dir, recursive = TRUE)

if (all_ok) cat("\nAll Phase 5 tests passed.\n") else stop("Some tests failed.")
