# Phase 3 tests: Digital cage (activity / behavioral) analysis
# Run from Cachexia project root: Rscript AnalysisCode/tests/test_digital_cage_analysis.R
# Uses a sample of digital data (n_max) for speed.

args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grepl("^--file=", args)])
script_dir <- if (length(script_path)) dirname(normalizePath(script_path[1], mustWork = FALSE)) else getwd()
analysis_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/")
cachexia_root <- normalizePath(file.path(analysis_dir, ".."), winslash = "/")
setwd(cachexia_root)
CACHEXIA_ROOT <- cachexia_root
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "R", "data_loader.R"))
source(file.path(analysis_dir, "R", "weight_analysis.R"))
source(file.path(analysis_dir, "R", "digital_cage_analysis.R"))

run_test <- function(name, expr) {
  tryCatch(
    { expr; cat("  OK:", name, "\n"); TRUE },
    error = function(e) { cat("  FAIL:", name, "-", conditionMessage(e), "\n"); FALSE }
  )
}

cat("Phase 3 tests: Digital cage analysis\n")
all_ok <- TRUE

if (!file.exists(PATH_DIGITAL_CSV) || !requireNamespace("data.table", quietly = TRUE)) {
  cat("  SKIP: Digital CSV not found or data.table not installed.\n")
  cat("All Phase 3 tests skipped.\n")
  quit(save = "no", status = 0)
}

# Load a sample of digital data for tests (full file is large)
digital_small <- load_digital_cage_data(n_max = 50000)
merged <- merge_all_data(
  meta = load_treatment_metadata(),
  bcs = load_bcs_data(),
  weights = load_weight_data(),
  digital = digital_small
)
dig_tx <- merged$digital_with_treatment
all_ok <- run_test("digital_with_treatment has treatment", stopifnot("treatment" %in% names(dig_tx))) && all_ok

# Available metrics
metrics <- digital_metric_columns(dig_tx)
all_ok <- run_test("digital_metric_columns returns character vector", stopifnot(is.character(metrics))) && all_ok

if (length(metrics) == 0) {
  cat("  SKIP: No numeric metric columns in digital data.\n")
  cat("All Phase 3 tests skipped (no metrics).\n")
  quit(save = "no", status = 0)
}

# Pick first available metric for tests
metric1 <- metrics[1]
daily <- aggregate_digital_by_date(dig_tx, metrics = metrics)
all_ok <- run_test("aggregate_digital_by_date returns data.frame", stopifnot(is.data.frame(daily))) && all_ok
all_ok <- run_test("daily has date and animal_id", stopifnot("date" %in% names(daily), "animal_id" %in% names(daily))) && all_ok
all_ok <- run_test("daily has treatment", stopifnot("treatment" %in% names(daily))) && all_ok
all_ok <- run_test("daily has metric column", stopifnot(metric1 %in% names(daily))) && all_ok

# Plot trajectories (ggplot or base R)
p_dig <- plot_digital_trajectories(daily, metric = metric1, by_treatment = TRUE, show_mean = TRUE)
all_ok <- run_test("plot_digital_trajectories returns", stopifnot(is.null(p_dig) || inherits(p_dig, "gg"))) && all_ok

# Group comparison (mean per animal)
comp <- digital_group_comparison(daily, metric = metric1, summary_per_animal = "mean")
all_ok <- run_test("digital_group_comparison returns list", stopifnot(is.list(comp))) && all_ok
if (is.null(comp$message)) {
  all_ok <- run_test("digital_group_comparison has summary and test", stopifnot(!is.null(comp$summary), !is.null(comp$test))) && all_ok
  all_ok <- run_test("digital_group_comparison metric field", stopifnot(comp$metric == metric1)) && all_ok
}

# Correlation with weight AUC if we have weights
weights_tx <- merged$weights_with_treatment
auc_wt <- weight_auc_analysis(weights_tx)
if (requireNamespace("data.table", quietly = TRUE)) {
  cor_res <- correlate_digital_with_outcome(daily, outcome_df = auc_wt, outcome_col = "weight_auc", metric = metric1, digital_summary = "mean")
  all_ok <- run_test("correlate_digital_with_outcome returns list", stopifnot(is.list(cor_res))) && all_ok
  if (is.null(cor_res$message)) {
    all_ok <- run_test("correlate_digital_with_outcome has correlation", stopifnot("correlation" %in% names(cor_res))) && all_ok
  }
}

if (all_ok) cat("\nAll Phase 3 tests passed.\n") else stop("Some tests failed.")
