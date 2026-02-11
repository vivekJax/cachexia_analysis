# Source all analysis code. Run from Cachexia project root or set CACHEXIA_ROOT.
# Example from Cachexia/:  source("AnalysisCode/source_all.R")

wd <- getwd()
analysis_dir <- if (file.exists(file.path(wd, "AnalysisCode", "config.R"))) {
  file.path(wd, "AnalysisCode")
} else if (file.exists(file.path(wd, "config.R"))) {
  wd
} else {
  stop("Run from Cachexia project root (parent of AnalysisCode) or from AnalysisCode/")
}
if (!exists("CACHEXIA_ROOT") || !nzchar(CACHEXIA_ROOT)) {
  CACHEXIA_ROOT <- if (basename(analysis_dir) == "AnalysisCode") dirname(analysis_dir) else analysis_dir
  CACHEXIA_ROOT <- normalizePath(CACHEXIA_ROOT, winslash = "/")
}
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "R", "data_loader.R"))
source(file.path(analysis_dir, "R", "bcs_analysis.R"))
source(file.path(analysis_dir, "R", "weight_analysis.R"))
source(file.path(analysis_dir, "R", "onset_detection.R"))
source(file.path(analysis_dir, "R", "sensitivity_analysis.R"))
source(file.path(analysis_dir, "R", "digital_cage_analysis.R"))
source(file.path(analysis_dir, "R", "reporting.R"))