# Phase 4 tests: Onset detection and sensitivity analysis
# Run from Cachexia project root: Rscript AnalysisCode/tests/test_phase4.R

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
source(file.path(analysis_dir, "R", "onset_detection.R"))
source(file.path(analysis_dir, "R", "sensitivity_analysis.R"))

run_test <- function(name, expr) {
  tryCatch(
    { expr; cat("  OK:", name, "\n"); TRUE },
    error = function(e) { cat("  FAIL:", name, "-", conditionMessage(e), "\n"); FALSE }
  )
}

cat("Phase 4 tests: Onset detection and sensitivity analysis\n")
all_ok <- TRUE

meta <- load_treatment_metadata()
bcs <- load_bcs_data()
weights <- load_weight_data()
merged <- merge_all_data(meta = meta, bcs = bcs, weights = weights)
bcs_tx <- merged$bcs_with_treatment
weights_tx <- merged$weights_with_treatment

# --- Onset detection ---
manual_onset <- detect_manual_onset(bcs_tx, weights_tx, methods = c("BCS", "weight"))
all_ok <- run_test("detect_manual_onset returns data.frame", stopifnot(is.data.frame(manual_onset))) && all_ok
all_ok <- run_test("detect_manual_onset has method column", stopifnot("method" %in% names(manual_onset))) && all_ok
all_ok <- run_test("detect_manual_onset has days_to_onset", stopifnot("days_to_onset" %in% names(manual_onset))) && all_ok

# Digital: use small sample for speed
if (file.exists(PATH_DIGITAL_CSV) && requireNamespace("data.table", quietly = TRUE)) {
  digital_small <- load_digital_cage_data(n_max = 50000)
  daily <- aggregate_digital_to_daily(digital_small, feature_col = NULL)
  all_ok <- run_test("aggregate_digital_to_daily returns animal_id, date, value", stopifnot(all(c("animal_id", "date", "value") %in% names(daily)))) && all_ok
  dig_onset <- detect_digital_onset(daily, baseline_days = 2, n_consecutive = 2)
  all_ok <- run_test("detect_digital_onset returns data.frame", stopifnot(is.data.frame(dig_onset))) && all_ok
  if (nrow(dig_onset) > 0 && nrow(manual_onset) > 0) {
    comp <- compare_onset_times(manual_onset, dig_onset, manual_method = "BCS")
    all_ok <- run_test("compare_onset_times returns lead_time_days", stopifnot("lead_time_days" %in% names(comp))) && all_ok
    lead <- calculate_lead_time(comp, bootstrap_n = 100, ci_level = 0.95)
    all_ok <- run_test("calculate_lead_time returns mean and optional CI", stopifnot(is.list(lead), "mean" %in% names(lead))) && all_ok
  }
} else {
  cat("  SKIP: digital onset tests (no CSV or data.table)\n")
}
# Unconditional test: compare_onset_times and calculate_lead_time with synthetic data
syn_man <- data.frame(animal_id = c("A1", "A2"), days_to_onset = c(10L, 15L), method = "BCS", stringsAsFactors = FALSE)
syn_dig <- data.frame(animal_id = c("A1", "A2"), days_to_onset = c(7L, 12L), stringsAsFactors = FALSE)
comp_syn <- compare_onset_times(syn_man, syn_dig, manual_method = "BCS")
all_ok <- run_test("compare_onset_times (synthetic) has lead_time_days", stopifnot("lead_time_days" %in% names(comp_syn), nrow(comp_syn) == 2)) && all_ok
lead_syn <- calculate_lead_time(comp_syn, bootstrap_n = 100)
all_ok <- run_test("calculate_lead_time (synthetic) returns mean", stopifnot(is.list(lead_syn), "mean" %in% names(lead_syn), lead_syn$mean > 0)) && all_ok

# --- Sensitivity analysis ---
auc_wt <- weight_auc_analysis(weights_tx)
eff <- calculate_effect_sizes(auc_wt, value_cols = "weight_auc")
all_ok <- run_test("calculate_effect_sizes returns outcome and cohens_d", stopifnot(all(c("outcome", "cohens_d") %in% names(eff)))) && all_ok

pwr <- power_analysis_comparison(cohens_d = 0.8, power = 0.8)
all_ok <- run_test("power_analysis_comparison returns n_per_group", stopifnot("n_per_group" %in% names(pwr))) && all_ok

# Cachexia status: BCS < 3 at last observation
bcs_last <- merge(
  aggregate(date ~ animal_id, data = bcs_tx, FUN = max),
  bcs_tx,
  by = c("animal_id", "date")
)
bcs_last$cachexia <- as.integer(bcs_last$bcs < 3)
# Use weight_auc as predictor (lower = more weight loss = more cachexia)
bcs_last <- merge(bcs_last, auc_wt[, c("animal_id", "weight_auc")], by = "animal_id", all.x = TRUE)
roc_res <- roc_analysis(bcs_last$cachexia, bcs_last$weight_auc, direction = "auto")
all_ok <- run_test("roc_analysis returns auc and roc_curve", stopifnot("auc" %in% names(roc_res), "roc_curve" %in% names(roc_res))) && all_ok

x <- c(1, 2, 3, 4, 5)
boot_ci <- bootstrap_confidence_intervals(x, statistic = mean, n_bootstrap = 500)
all_ok <- run_test("bootstrap_confidence_intervals returns estimate and CI", stopifnot(all(c("estimate", "ci_lower", "ci_upper") %in% names(boot_ci)))) && all_ok

if (all_ok) cat("\nAll Phase 4 tests passed.\n") else stop("Some tests failed.")
