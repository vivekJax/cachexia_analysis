# Generate README images for Cachexia Analysis
# Run from project root: Rscript generate_readme_images.R

# Find project root (folder containing app.R and this script)
args <- commandArgs(trailingOnly = FALSE)
script_match <- grep("^--file=", args, value = TRUE)
root <- if (length(script_match)) {
  dirname(normalizePath(sub("^--file=", "", script_match), winslash = "/"))
} else {
  getwd()
}
if (!file.exists(file.path(root, "app.R"))) {
  stop("Run from Cachexia project root (folder containing app.R). Current dir: ", getwd())
}
setwd(root)

source("AnalysisCode/source_all.R")

# Create images directory
img_dir <- "images"
if (!dir.exists(img_dir)) dir.create(img_dir, showWarnings = FALSE)

# Load data (Excel only - no digital data needed for BCS/weight plots)
meta <- load_treatment_metadata()
bcs <- load_bcs_data()
weights <- load_weight_data()
merged <- merge_all_data(meta = meta, bcs = bcs, weights = weights, digital = NULL)

# 1. BCS trajectories
p_bcs <- plot_bcs_trajectories(merged$bcs_with_treatment, by_treatment = TRUE, show_mean = TRUE, threshold = BCS_CACHEXIA_THRESHOLD)
if (!is.null(p_bcs) && inherits(p_bcs, "gg")) {
  ggplot2::ggsave(file.path(img_dir, "bcs_trajectories.png"), p_bcs, width = 8, height = 5, dpi = 120)
  message("Saved: ", file.path(img_dir, "bcs_trajectories.png"))
}

# 2. Weight trajectories (% change from baseline)
p_weight <- plot_weight_trajectories(merged$weights_with_treatment, percent_change = TRUE, by_treatment = TRUE, show_mean = TRUE)
if (!is.null(p_weight) && inherits(p_weight, "gg")) {
  ggplot2::ggsave(file.path(img_dir, "weight_trajectories.png"), p_weight, width = 8, height = 5, dpi = 120)
  message("Saved: ", file.path(img_dir, "weight_trajectories.png"))
}

# 3. BCS survival (Kaplan-Meier)
if (exists("plot_bcs_survival_curve") && requireNamespace("survival", quietly = TRUE)) {
  png(file.path(img_dir, "bcs_survival.png"), width = 800, height = 500, res = 120, bg = "white")
  tryCatch({
    plot_bcs_survival_curve(merged$bcs_with_treatment, threshold = BCS_CACHEXIA_THRESHOLD)
  }, finally = dev.off())
  message("Saved: ", file.path(img_dir, "bcs_survival.png"))
}

message("Done. Images saved to ", img_dir, "/")
