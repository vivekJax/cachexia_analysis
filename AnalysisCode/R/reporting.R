# Phase 5: Reporting and Pipeline
# Requires: Phase 1 (data_loader), Phase 2 (bcs_analysis, weight_analysis),
#           Phase 3 (digital_cage_analysis), Phase 4 (onset_detection, sensitivity_analysis)
# Runs full analysis pipeline, summarizes results, and exports tables.

#' Run the full Cachexia analysis pipeline (Phases 1–4).
#' Loads Excel + optional digital cage data, merges, runs BCS/weight/digital/onset/sensitivity.
#' @param digital_n_max Max rows to read from digital CSV (NULL = all; use a number for speed)
#' @param bcs_threshold BCS threshold for onset (default from config)
#' @param weight_loss_pct Weight loss percent for onset (default from config)
#' @param digital_metric Optional metric for digital onset; if NULL, digital onset skipped
#' @return List with merged (from merge_all_data), bcs_onset, weight_onset, digital_onset (if digital loaded),
#'   manual_onset, lead_time_comparison (if digital onset), effect_sizes, weight_auc, bcs_survival, etc.
run_full_pipeline <- function(digital_n_max = 50000L,
                             bcs_threshold = getOption("cachexia_bcs_threshold", BCS_CACHEXIA_THRESHOLD),
                             weight_loss_pct = getOption("cachexia_weight_loss_pct", WEIGHT_LOSS_PERCENT_THRESHOLD),
                             digital_metric = NULL) {
  meta <- load_treatment_metadata()
  bcs <- load_bcs_data()
  weights <- load_weight_data()
  digital <- NULL
  if (file.exists(PATH_DIGITAL_CSV) && requireNamespace("data.table", quietly = TRUE)) {
    digital <- load_digital_cage_data(n_max = digital_n_max)
  }
  merged <- merge_all_data(meta = meta, bcs = bcs, weights = weights, digital = digital)
  bcs_tx <- merged$bcs_with_treatment
  weights_tx <- merged$weights_with_treatment

  # Phase 2 outputs
  bcs_onset <- calculate_bcs_onset(bcs_tx, threshold = bcs_threshold)
  weight_onset <- calculate_weight_loss_onset(weights_tx, threshold_percent = weight_loss_pct)
  bcs_comp <- bcs_group_comparison(bcs_tx, timepoint = NULL)
  weight_comp <- weight_group_comparison(weights_tx, timepoint = NULL)
  weight_auc_df <- weight_auc_analysis(weights_tx)
  bcs_surv <- bcs_survival_analysis(bcs_tx, threshold = bcs_threshold)

  # Phase 3: digital daily and group comparison (if digital loaded)
  digital_daily <- NULL
  digital_comp <- NULL
  if (!is.null(merged$digital_with_treatment)) {
    digital_daily <- aggregate_digital_by_date(merged$digital_with_treatment)
    metrics <- digital_metric_columns(merged$digital_with_treatment)
    if (length(metrics) > 0L) {
      m1 <- if (!is.null(digital_metric) && digital_metric %in% metrics) digital_metric else metrics[1]
      digital_comp <- digital_group_comparison(digital_daily, metric = m1, summary_per_animal = "mean")
    }
  }

  # Phase 4: unified manual onset and optional digital onset + lead time
  manual_onset <- detect_manual_onset(bcs_tx, weights_tx,
                                      bcs_threshold = bcs_threshold,
                                      weight_loss_percent = weight_loss_pct,
                                      methods = c("BCS", "weight"))
  digital_onset_df <- NULL
  lead_time_comparison <- NULL
  lead_time_summary <- NULL
  if (!is.null(merged$digital) && requireNamespace("data.table", quietly = TRUE)) {
    dig_daily_feat <- aggregate_digital_to_daily(merged$digital, feature_col = digital_metric, date_col = "start.date.local")
    digital_onset_df <- detect_digital_onset(dig_daily_feat, baseline_days = 3, k_sd = 2, n_consecutive = 2)
    if (nrow(digital_onset_df) > 0L && nrow(manual_onset) > 0L) {
      lead_time_comparison <- compare_onset_times(manual_onset, digital_onset_df, manual_method = "BCS")
      if (nrow(lead_time_comparison) > 0L) {
        lead_time_summary <- calculate_lead_time(lead_time_comparison, bootstrap_n = 500, ci_level = 0.95)
      }
    }
  }

  # Phase 4: effect sizes across outcomes (per-animal summaries)
  effect_df <- data.frame(
    animal_id = unique(c(bcs_tx$animal_id, weights_tx$animal_id)),
    stringsAsFactors = FALSE
  )
  effect_df <- merge(effect_df, meta[, c("animal_id", "treatment")], by = "animal_id", all.x = TRUE)
  last_bcs <- stats::aggregate(bcs ~ animal_id, data = bcs_tx, FUN = function(x) x[length(x)])
  names(last_bcs)[2] <- "last_bcs"
  effect_df <- merge(effect_df, last_bcs, by = "animal_id", all.x = TRUE)
  effect_df <- merge(effect_df, weight_auc_df[, c("animal_id", "weight_auc")], by = "animal_id", all.x = TRUE)
  effect_sizes <- calculate_effect_sizes(effect_df, value_cols = c("last_bcs", "weight_auc"))

  out <- list(
    merged = merged,
    bcs_onset = bcs_onset,
    weight_onset = weight_onset,
    manual_onset = manual_onset,
    bcs_group_comparison = bcs_comp,
    weight_group_comparison = weight_comp,
    weight_auc = weight_auc_df,
    bcs_survival = bcs_surv,
    effect_sizes = effect_sizes,
    digital_daily = digital_daily,
    digital_group_comparison = digital_comp,
    digital_onset = digital_onset_df,
    lead_time_comparison = lead_time_comparison,
    lead_time_summary = lead_time_summary
  )
  out
}

#' Summarize pipeline results into a compact list of key tables and statistics.
#' @param pipeline List from run_full_pipeline(); or NULL to run pipeline with default args
#' @param run_pipeline_if_null If TRUE and pipeline is NULL, call run_full_pipeline()
#' @return List with summary tables: group_comparisons, onset_summary, effect_sizes, lead_time (if available)
summarize_cachexia_results <- function(pipeline = NULL, run_pipeline_if_null = TRUE) {
  if (is.null(pipeline)) {
    if (!run_pipeline_if_null) stop("pipeline is NULL and run_pipeline_if_null is FALSE.")
    pipeline <- run_full_pipeline(digital_n_max = 50000L)
  }
  out <- list(
    bcs_summary = pipeline$bcs_group_comparison$summary,
    bcs_test = pipeline$bcs_group_comparison$test,
    weight_summary = pipeline$weight_group_comparison$summary,
    weight_test = pipeline$weight_group_comparison$test,
    effect_sizes = pipeline$effect_sizes,
    bcs_onset_n = nrow(pipeline$bcs_onset),
    weight_onset_n = nrow(pipeline$weight_onset),
    manual_onset_n = nrow(pipeline$manual_onset)
  )
  if (!is.null(pipeline$digital_group_comparison) && !is.null(pipeline$digital_group_comparison$summary)) {
    out$digital_summary <- pipeline$digital_group_comparison$summary
    out$digital_test <- pipeline$digital_group_comparison$test
  }
  if (!is.null(pipeline$lead_time_summary)) {
    out$lead_time_summary <- pipeline$lead_time_summary
  }
  out
}

#' Export key pipeline results to CSV files in an output directory.
#' @param pipeline List from run_full_pipeline()
#' @param output_dir Directory to write CSVs (default: file.path(CACHEXIA_ROOT, "AnalysisCode", "output"))
#' @param prefix Optional prefix for filenames (e.g. "cachexia_")
#' @return Character vector of paths written
export_results <- function(pipeline,
                           output_dir = file.path(CACHEXIA_ROOT, "AnalysisCode", "output"),
                           prefix = "cachexia_") {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  written <- character()
  safe_write <- function(df, name) {
    if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
      path <- file.path(output_dir, paste0(prefix, name, ".csv"))
      utils::write.csv(df, path, row.names = FALSE)
      written <<- c(written, path)
    }
  }
  safe_write(pipeline$bcs_onset, "bcs_onset")
  safe_write(pipeline$weight_onset, "weight_onset")
  safe_write(pipeline$manual_onset, "manual_onset")
  safe_write(pipeline$weight_auc, "weight_auc")
  safe_write(pipeline$effect_sizes, "effect_sizes")
  if (!is.null(pipeline$digital_daily)) {
    # Export daily summary for first metric only (sample) to keep file small
    m1 <- setdiff(names(pipeline$digital_daily), c("animal_id", "date", "treatment"))[1]
    if (!is.na(m1)) safe_write(pipeline$digital_daily[, c("animal_id", "date", "treatment", m1)], "digital_daily_sample")
  }
  safe_write(pipeline$digital_onset, "digital_onset")
  safe_write(pipeline$lead_time_comparison, "lead_time_comparison")
  written
}
