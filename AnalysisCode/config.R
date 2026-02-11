# Configuration for Cachexia Analysis
# Paths are relative to the Cachexia project root (parent of AnalysisCode)

CACHEXIA_ROOT <- if (exists("CACHEXIA_ROOT") && nzchar(CACHEXIA_ROOT)) {
  CACHEXIA_ROOT
} else {
  # If run from AnalysisCode/, data is in parent; if run from Cachexia/, data is in current dir
  if (file.exists("Final Cachexia in CD2 F1 mice with CT-26.xlsx")) {
    normalizePath(".", winslash = "/")
  } else if (file.exists(file.path("..", "Final Cachexia in CD2 F1 mice with CT-26.xlsx"))) {
    normalizePath("..", winslash = "/")
  } else {
    stop("Cannot find Cachexia data directory. Set CACHEXIA_ROOT to the folder containing the Excel and CSV files.")
  }
}

PATH_EXCEL <- file.path(CACHEXIA_ROOT, "Final Cachexia in CD2 F1 mice with CT-26.xlsx")
PATH_DIGITAL_CSV <- file.path(CACHEXIA_ROOT, "CCX_C1_animal_1min_2026-01-28T18_21_57Z.csv")

# BCS cachexia threshold (common definition: BCS < 3 indicates concern)
BCS_CACHEXIA_THRESHOLD <- 3

# Weight loss threshold for onset (e.g., 10% from baseline)
WEIGHT_LOSS_PERCENT_THRESHOLD <- 10

# Valid BCS range (1-5 scale)
BCS_VALID_RANGE <- c(1, 5)

# Plausible weight range for adult mice (grams)
WEIGHT_VALID_RANGE <- c(15, 45)
