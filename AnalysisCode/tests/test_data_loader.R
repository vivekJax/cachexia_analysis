# Phase 1 tests: Data loader and validation
# Run from Cachexia project root: Rscript AnalysisCode/tests/test_data_loader.R

options(cachexia_excel_path = NULL, cachexia_csv_path = NULL)
# Set paths: script may be run via Rscript from Cachexia root
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grepl("^--file=", args)])
script_dir <- if (length(script_path)) dirname(normalizePath(script_path[1], mustWork = FALSE)) else getwd()
analysis_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/")
cachexia_root <- normalizePath(file.path(analysis_dir, ".."), winslash = "/")
setwd(cachexia_root)
CACHEXIA_ROOT <- cachexia_root
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "R", "data_loader.R"))

run_test <- function(name, expr) {
  tryCatch(
    { expr; cat("  OK:", name, "\n"); TRUE },
    error = function(e) { cat("  FAIL:", name, "-", conditionMessage(e), "\n"); FALSE }
  )
}

cat("Phase 1 tests: Data loader\n")
all_ok <- TRUE

# All 58 animal IDs present in metadata
meta <- load_treatment_metadata()
all_ok <- run_test("58 animals in metadata", stopifnot(nrow(meta) == 58L)) && all_ok

# Treatment assignments present
all_ok <- run_test("Treatment column present", stopifnot("treatment" %in% names(meta))) && all_ok
all_ok <- run_test("Treatment counts CT-26 and Vehicle", stopifnot(all(meta$treatment %in% c("CT-26", "Vehicle")))) && all_ok

# BCS data loads and has valid structure
bcs <- load_bcs_data()
all_ok <- run_test("BCS has animal_id, date, bcs", stopifnot(all(c("animal_id", "date", "bcs") %in% names(bcs)))) && all_ok
all_ok <- run_test("BCS values in valid range 1-5", stopifnot(all(bcs$bcs >= 1 & bcs$bcs <= 5, na.rm = TRUE))) && all_ok

# Weights data loads
weights <- load_weight_data()
all_ok <- run_test("Weights has animal_id, date, weight", stopifnot(all(c("animal_id", "date", "weight") %in% names(weights)))) && all_ok
all_ok <- run_test("Weight column numeric and has records", stopifnot(is.numeric(weights$weight), sum(!is.na(weights$weight)) > 0)) && all_ok

# No duplicate timestamps per animal in BCS (one value per animal per date)
bcs_dup <- aggregate(bcs ~ animal_id + date, data = bcs, FUN = length)
all_ok <- run_test("No duplicate BCS per animal per date", stopifnot(all(bcs_dup$bcs <= 1L))) && all_ok

# Merge produces bcs_with_treatment and weights_with_treatment
merged <- merge_all_data(meta = meta, bcs = bcs, weights = weights, digital = NULL)
all_ok <- run_test("merge_all_data returns bcs_with_treatment", stopifnot("bcs_with_treatment" %in% names(merged))) && all_ok
all_ok <- run_test("merge_all_data returns weights_with_treatment", stopifnot("weights_with_treatment" %in% names(merged))) && all_ok
all_ok <- run_test("bcs_with_treatment has treatment column", stopifnot("treatment" %in% names(merged$bcs_with_treatment))) && all_ok

# Validation: animal assignments (without digital still runs)
val <- validate_animal_assignments(meta, digital = NULL)
all_ok <- run_test("validate_animal_assignments returns ok", stopifnot(val$ok)) && all_ok

# Data quality summary
qual <- summarize_data_quality(merged)
all_ok <- run_test("summarize_data_quality returns BCS and weights", stopifnot(all(c("bcs", "weights") %in% names(qual)))) && all_ok

# Optional: load digital (first 1000 rows only for speed)
if (file.exists(PATH_DIGITAL_CSV) && requireNamespace("data.table", quietly = TRUE)) {
  digital_small <- load_digital_cage_data(n_max = 5000)
  val_dig <- validate_animal_assignments(meta, digital = digital_small)
  val_cage <- validate_cage_groups(digital_small)
  all_ok <- run_test("validate_cage_groups returns ok", stopifnot(val_cage$ok)) && all_ok
  merged_dig <- merge_all_data(meta = meta, bcs = bcs, weights = weights, digital = digital_small)
  qual_dig <- summarize_data_quality(merged_dig)
  all_ok <- run_test("summarize_data_quality with digital", stopifnot("digital" %in% names(qual_dig))) && all_ok
}

if (all_ok) cat("\nAll Phase 1 tests passed.\n") else stop("Some tests failed.")
