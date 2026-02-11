# Phase 2 tests: BCS and Weight analysis
# Run from Cachexia project root: Rscript AnalysisCode/tests/test_bcs_weight_analysis.R

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

run_test <- function(name, expr) {
  tryCatch(
    { expr; cat("  OK:", name, "\n"); TRUE },
    error = function(e) { cat("  FAIL:", name, "-", conditionMessage(e), "\n"); FALSE }
  )
}

cat("Phase 2 tests: BCS and Weight analysis\n")
all_ok <- TRUE

meta <- load_treatment_metadata()
bcs <- load_bcs_data()
weights <- load_weight_data()
merged <- merge_all_data(meta = meta, bcs = bcs, weights = weights)
bcs_tx <- merged$bcs_with_treatment
weights_tx <- merged$weights_with_treatment

# BCS trajectory plot runs (no error)
p_bcs <- plot_bcs_trajectories(bcs_tx, by_treatment = TRUE, show_mean = TRUE, threshold = 3)
all_ok <- run_test("plot_bcs_trajectories returns", stopifnot(is.null(p_bcs) || inherits(p_bcs, "gg"))) && all_ok

# BCS onset calculation
onset_bcs <- calculate_bcs_onset(bcs_tx, threshold = 3)
all_ok <- run_test("calculate_bcs_onset returns data.frame", stopifnot(is.data.frame(onset_bcs))) && all_ok
all_ok <- run_test("BCS onset has expected columns", stopifnot(all(c("animal_id", "first_below_date", "days_to_onset") %in% names(onset_bcs)))) && all_ok

# BCS group comparison
comp_bcs <- bcs_group_comparison(bcs_tx, timepoint = NULL)
all_ok <- run_test("bcs_group_comparison returns summary and test", stopifnot(!is.null(comp_bcs$summary), !is.null(comp_bcs$test))) && all_ok

# BCS survival (optional survival package)
surv_bcs <- bcs_survival_analysis(bcs_tx, threshold = 3)
all_ok <- run_test("bcs_survival_analysis returns", stopifnot(is.list(surv_bcs))) && all_ok

# Weight trajectory plot
p_wt <- plot_weight_trajectories(weights_tx, percent_change = TRUE, by_treatment = TRUE, show_mean = TRUE)
all_ok <- run_test("plot_weight_trajectories returns", stopifnot(is.null(p_wt) || inherits(p_wt, "gg"))) && all_ok

# Weight loss onset
onset_wt <- calculate_weight_loss_onset(weights_tx, threshold_percent = 10)
all_ok <- run_test("calculate_weight_loss_onset returns data.frame", stopifnot(is.data.frame(onset_wt))) && all_ok
all_ok <- run_test("Weight onset has expected columns", stopifnot(all(c("animal_id", "baseline_weight") %in% names(onset_wt)))) && all_ok

# Weight group comparison
comp_wt <- weight_group_comparison(weights_tx, timepoint = NULL)
all_ok <- run_test("weight_group_comparison returns summary and test", stopifnot(!is.null(comp_wt$summary), !is.null(comp_wt$test))) && all_ok

# Weight AUC
auc_wt <- weight_auc_analysis(weights_tx)
all_ok <- run_test("weight_auc_analysis returns animal_id and weight_auc", stopifnot(all(c("animal_id", "weight_auc", "treatment") %in% names(auc_wt)))) && all_ok

if (all_ok) cat("\nAll Phase 2 tests passed.\n") else stop("Some tests failed.")
