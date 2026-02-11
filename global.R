# Global setup for Cachexia Shiny app. Run from Cachexia project root.
# Ensures CACHEXIA_ROOT and sources all analysis modules.

if (!file.exists("AnalysisCode/source_all.R")) {
  if (file.exists(file.path("..", "AnalysisCode", "source_all.R"))) {
    setwd("..")
  } else {
    stop("Run the app from the Cachexia project root (parent of AnalysisCode/).")
  }
}
if (!exists("CACHEXIA_ROOT") || !nzchar(CACHEXIA_ROOT)) {
  CACHEXIA_ROOT <- getwd()
}
source("AnalysisCode/source_all.R")
