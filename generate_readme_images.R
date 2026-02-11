#!/usr/bin/env Rscript
#
# Generate README images for Cachexia Analysis
#
# Regenerates the plots shown in README.md: BCS trajectories (with SEM),
# weight trajectories (% change from baseline), and BCS Kaplan-Meier survival.
#
# Usage (from project root):
#   Rscript generate_readme_images.R
#
# Or from R:
#   setwd("/path/to/cachexia_analysis")
#   source("generate_readme_images.R")
#

# ------------------------------------------------------------------------------
# Setup: find project root and load analysis code
# ------------------------------------------------------------------------------

find_project_root <- function() {
  # When run via Rscript, use the script's directory
  args <- commandArgs(trailingOnly = FALSE)
  script_match <- grep("^--file=", args, value = TRUE)
  if (length(script_match) > 0L) {
    root <- dirname(normalizePath(sub("^--file=", "", script_match), winslash = "/"))
  } else {
    root <- getwd()
  }
  if (!file.exists(file.path(root, "app.R"))) {
    stop("Cannot find project root. Run from the cachexia_analysis directory (folder containing app.R).")
  }
  root
}

main <- function() {
  root <- find_project_root()
  setwd(root)

  # Source analysis code
  source_path <- file.path(root, "AnalysisCode", "source_all.R")
  if (!file.exists(source_path)) {
    stop("Missing AnalysisCode/source_all.R")
  }
  source(source_path)

  # Output directory
  img_dir <- file.path(root, "images")
  if (!dir.exists(img_dir)) {
    dir.create(img_dir, showWarnings = FALSE)
  }

  # Load data
  if (!file.exists(PATH_EXCEL)) {
    stop("Excel file not found: ", PATH_EXCEL)
  }
  meta <- load_treatment_metadata()
  bcs <- load_bcs_data()
  weights <- load_weight_data()
  merged <- merge_all_data(meta = meta, bcs = bcs, weights = weights, digital = NULL)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 is required. Install with: install.packages('ggplot2')")
  }

  # --------------------------------------------------------------------------
  # 1. BCS trajectories (mean ± SEM) — different style with error bars
  # --------------------------------------------------------------------------
  tryCatch({
    p <- plot_bcs_trajectories_with_sem(
      merged$bcs_with_treatment,
      threshold = BCS_CACHEXIA_THRESHOLD
    )
    out <- file.path(img_dir, "bcs_trajectories.png")
    ggplot2::ggsave(out, p, width = 8, height = 5, dpi = 120)
    message("Saved: ", out)
  }, error = function(e) {
    warning("BCS trajectories (SEM): ", conditionMessage(e))
  })

  # --------------------------------------------------------------------------
  # 2. Weight trajectories (% change from baseline)
  # --------------------------------------------------------------------------
  tryCatch({
    p <- plot_weight_trajectories(
      merged$weights_with_treatment,
      percent_change = TRUE,
      by_treatment = TRUE,
      show_mean = TRUE
    )
    if (!is.null(p) && inherits(p, "gg")) {
      out <- file.path(img_dir, "weight_trajectories.png")
      ggplot2::ggsave(out, p, width = 8, height = 5, dpi = 120)
      message("Saved: ", out)
    }
  }, error = function(e) {
    warning("Weight trajectories: ", conditionMessage(e))
  })

  # --------------------------------------------------------------------------
  # 3. BCS survival (Kaplan-Meier)
  # --------------------------------------------------------------------------
  if (requireNamespace("survival", quietly = TRUE) && exists("plot_bcs_survival_curve")) {
    tryCatch({
      out <- file.path(img_dir, "bcs_survival.png")
      grDevices::png(out, width = 800, height = 500, res = 120, bg = "white")
      on.exit(grDevices::dev.off(), add = TRUE)
      plot_bcs_survival_curve(merged$bcs_with_treatment, threshold = BCS_CACHEXIA_THRESHOLD)
      message("Saved: ", out)
    }, error = function(e) {
      warning("BCS survival: ", conditionMessage(e))
    })
  } else {
    message("Skipping BCS survival (package 'survival' required)")
  }

  message("Done. Images saved to ", img_dir)
}

main()
