# Phase 4: Onset Detection and Comparison
# Requires: data_loader (Phase 1), bcs_analysis and weight_analysis (Phase 2)
# Optional: data.table for efficient digital aggregation

#' Detect manual (BCS and/or weight) onset of cachexia with a unified interface.
#' @param bcs_with_treatment data.frame with animal_id, date, bcs, treatment
#' @param weights_with_treatment data.frame with animal_id, date, weight, treatment
#' @param bcs_threshold BCS threshold for onset (first date BCS < threshold)
#' @param weight_loss_percent Weight loss percent for onset (first date loss >= this)
#' @param baseline_date Optional; study start for days_to_onset
#' @param methods "BCS", "weight", or c("BCS", "weight") to include both
#' @return data.frame with animal_id, method, onset_date, days_to_onset, treatment
detect_manual_onset <- function(bcs_with_treatment,
                                weights_with_treatment,
                                bcs_threshold = getOption("cachexia_bcs_threshold", BCS_CACHEXIA_THRESHOLD),
                                weight_loss_percent = getOption("cachexia_weight_loss_pct", WEIGHT_LOSS_PERCENT_THRESHOLD),
                                baseline_date = NULL,
                                methods = c("BCS", "weight")) {
  out_list <- list()
  study_start <- baseline_date
  if ("BCS" %in% methods) {
    bcs_onset <- calculate_bcs_onset(bcs_with_treatment, threshold = bcs_threshold, baseline_date = baseline_date)
    if (is.null(study_start) && nrow(bcs_onset) > 0) {
      bcs_df <- as.data.frame(bcs_with_treatment)
      study_start <- min(bcs_df$date, na.rm = TRUE)
    }
    if (nrow(bcs_onset) > 0) {
      bcs_onset$method <- "BCS"
      bcs_onset$onset_date <- bcs_onset$first_below_date
      bcs_onset <- bcs_onset[, c("animal_id", "method", "onset_date", "days_to_onset", "treatment")]
      out_list[["BCS"]] <- bcs_onset
    }
  }
  if ("weight" %in% methods) {
    wt_onset <- calculate_weight_loss_onset(weights_with_treatment, threshold_percent = weight_loss_percent, baseline_date = baseline_date)
    if (is.null(study_start) && nrow(wt_onset) > 0) {
      wt_df <- as.data.frame(weights_with_treatment)
      study_start <- min(wt_df$date, na.rm = TRUE)
    }
    if (nrow(wt_onset) > 0) {
      wt_onset$method <- "weight"
      wt_onset$onset_date <- wt_onset$first_below_date
      wt_onset <- wt_onset[, c("animal_id", "method", "onset_date", "days_to_onset", "treatment")]
      out_list[["weight"]] <- wt_onset
    }
  }
  if (length(out_list) == 0) {
    return(data.frame(
      animal_id = character(), method = character(), onset_date = as.Date(character()),
      days_to_onset = integer(), treatment = character(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, out_list)
}

#' Aggregate digital cage 1-min data to daily per-animal features.
#' @param digital data.table or data.frame from load_digital_cage_data()
#' @param feature_col Column name for phenotype (e.g. "activity.animal.cm_s.min"). If NULL, uses first matching activity column.
#' @param date_col Column name for date (default "start.date.local")
#' @return data.frame with animal_id (as animal.id), date, value (daily mean of feature), n_obs
aggregate_digital_to_daily <- function(digital,
                                     feature_col = NULL,
                                     date_col = "start.date.local") {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package data.table is required for aggregation.")
  dt <- data.table::as.data.table(digital)
  if (!"animal.id" %in% names(dt)) stop("Digital data must have column animal.id")
  date_vec <- dt[[date_col]]
  if (is.character(date_vec)) {
    date_vec <- as.Date(date_vec)
  } else if (is.numeric(date_vec) || inherits(date_vec, "integer")) {
    dnum <- as.numeric(date_vec)
    if (length(dnum) > 0 && all(dnum[!is.na(dnum)] > 40000)) {
      date_vec <- as.Date(as.integer(date_vec), origin = "1899-12-30")
    } else {
      date_vec <- as.Date(as.integer(date_vec), origin = "1970-01-01")
    }
  }
  # Exclude implausible dates (e.g. 1899-12-30)
  date_vec[!is.na(date_vec) & date_vec < as.Date("2000-01-01")] <- as.Date(NA)
  dt$date <- date_vec
  if (is.null(feature_col)) {
    cand <- c("activity.animal.cm_s.min", "activity-active.animal.percent.min", "activity-inactive.animal.percent.min")
    feature_col <- cand[cand %in% names(dt)][1]
    if (is.na(feature_col)) feature_col <- names(dt)[sapply(dt, is.numeric)][1]
  }
  if (!feature_col %in% names(dt)) stop("Feature column not found: ", feature_col)
  daily <- dt[, list(value = mean(get(feature_col), na.rm = TRUE), n_obs = .N), by = c("animal.id", "date")]
  daily <- daily[order(animal.id, date)]
  out <- as.data.frame(daily)
  names(out)[names(out) == "animal.id"] <- "animal_id"
  out$feature_used <- feature_col
  out
}

#' Detect onset from digital daily feature using sustained drop from baseline.
#' Onset = first day when value is below (baseline_mean - k * baseline_sd) for n_consecutive days.
#' @param daily_digital data.frame from aggregate_digital_to_daily() with animal_id, date, value
#' @param baseline_days Number of initial days used as baseline (default 3)
#' @param k_sd Number of SDs below baseline mean to trigger (default 2)
#' @param n_consecutive Number of consecutive days below threshold required (default 2)
#' @param baseline_date Optional; if set, baseline = mean over [baseline_date, baseline_date+baseline_days)
#' @param study_start_date Optional; days_to_onset computed from this date
#' @return data.frame with animal_id, onset_date, days_to_onset, feature_used
detect_digital_onset <- function(daily_digital,
                                baseline_days = 3,
                                k_sd = 2,
                                n_consecutive = 2,
                                baseline_date = NULL,
                                study_start_date = NULL) {
  df <- as.data.frame(daily_digital)
  df <- df[!is.na(df$value) & !is.na(df$date), ]
  if (nrow(df) == 0) stop("No valid daily digital data.")
  animals <- unique(df$animal_id)
  study_start <- study_start_date
  if (is.null(study_start)) study_start <- min(df$date, na.rm = TRUE)
  out_list <- list()
  for (a in animals) {
    sub <- df[df$animal_id == a, ]
    sub <- sub[order(sub$date), ]
    if (nrow(sub) < baseline_days + n_consecutive) next
    if (is.null(baseline_date)) {
      base_sub <- sub[seq_len(baseline_days), ]
    } else {
      base_date <- as.Date(baseline_date)
      base_sub <- sub[sub$date >= base_date & sub$date < base_date + baseline_days, ]
    }
    if (nrow(base_sub) < 2) next
    base_mean <- mean(base_sub$value, na.rm = TRUE)
    base_sd <- stats::sd(base_sub$value, na.rm = TRUE)
    if (is.na(base_sd) || base_sd <= 0) base_sd <- 1e-6
    threshold <- base_mean - k_sd * base_sd
    below <- sub$value < threshold
    # First run of n_consecutive TRUE
    onset_date <- NULL
    for (i in seq_len(nrow(sub) - n_consecutive + 1)) {
      if (all(below[i:(i + n_consecutive - 1)])) {
        onset_date <- sub$date[i]
        break
      }
    }
    if (!is.null(onset_date)) {
      days_to_onset <- as.integer(onset_date - study_start)
      out_list[[a]] <- data.frame(
        animal_id = a,
        onset_date = onset_date,
        days_to_onset = days_to_onset,
        feature_used = if ("feature_used" %in% names(sub)) sub$feature_used[1] else NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(out_list) == 0) {
    return(data.frame(
      animal_id = character(), onset_date = as.Date(character()),
      days_to_onset = integer(), feature_used = character(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, out_list)
}

#' Compare manual and digital onset times; compute lead time (positive = digital detected earlier).
#' @param manual_onset data.frame with animal_id, days_to_onset, method (from detect_manual_onset, one method per row or subset to one method)
#' @param digital_onset data.frame with animal_id, days_to_onset (from detect_digital_onset)
#' @param manual_method If manual_onset has multiple methods, use this to filter (e.g. "BCS" or "weight")
#' @return data.frame with animal_id, manual_days_to_onset, digital_days_to_onset, lead_time_days (manual - digital; >0 = digital earlier)
compare_onset_times <- function(manual_onset,
                               digital_onset,
                               manual_method = NULL) {
  man <- as.data.frame(manual_onset)
  dig <- as.data.frame(digital_onset)
  if (!is.null(manual_method)) man <- man[man$method == manual_method, ]
  man <- man[, c("animal_id", "days_to_onset")]
  names(man)[2] <- "manual_days_to_onset"
  dig <- dig[, c("animal_id", "days_to_onset")]
  names(dig)[2] <- "digital_days_to_onset"
  merged <- merge(man, dig, by = "animal_id", all = FALSE)
  merged$lead_time_days <- merged$manual_days_to_onset - merged$digital_days_to_onset
  merged
}

#' Summarize lead time: mean, median, SD, quantiles, and optional bootstrap CI.
#' @param comparison data.frame from compare_onset_times() with lead_time_days
#' @param bootstrap_n Number of bootstrap samples for CI (0 = no bootstrap)
#' @param ci_level Confidence level for bootstrap CI (default 0.95)
#' @return List with summary stats and optional bootstrap_ci
calculate_lead_time <- function(comparison, bootstrap_n = 0, ci_level = 0.95) {
  x <- comparison$lead_time_days
  x <- x[!is.na(x)]
  if (length(x) == 0) return(list(summary = NULL, message = "No valid lead times"))
  out <- list(
    n = length(x),
    mean = mean(x),
    median = stats::median(x),
    sd = stats::sd(x),
    quantiles = stats::quantile(x, probs = c(0.25, 0.75))
  )
  if (bootstrap_n > 0 && length(x) >= 2) {
    set.seed(42)
    boots <- replicate(bootstrap_n, mean(sample(x, replace = TRUE)))
    alpha <- 1 - ci_level
    out$bootstrap_ci <- stats::quantile(boots, c(alpha / 2, 1 - alpha / 2))
    out$ci_level <- ci_level
  }
  out
}
