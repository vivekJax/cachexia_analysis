# Phase 2: Body Weight Analysis Module
# Requires: ggplot2 (optional for plots)

#' Plot weight trajectories (raw and optionally percent change from baseline).
#' @param weights_with_treatment data.frame with animal_id, date, weight, treatment
#' @param percent_change If TRUE, also compute and plot percent change from baseline
#' @param by_treatment If TRUE, color/facet by treatment
#' @param show_mean If TRUE, add group mean trajectory
#' @return ggplot object or base R plot
plot_weight_trajectories <- function(weights_with_treatment,
                                    percent_change = TRUE,
                                    by_treatment = TRUE,
                                    show_mean = TRUE) {
  df <- as.data.frame(weights_with_treatment)
  df <- df[!is.na(df$weight) & !is.na(df$date), ]
  # Coerce date to Date so ggplot/axis parses correctly (handles character, factor, POSIXct)
  df$date <- as.Date(df$date)
  # Drop implausible dates (e.g. Excel serial 0 -> 1899-12-30) so x-axis is not 1900/1950/2000
  df <- df[df$date >= as.Date("2000-01-01"), , drop = FALSE]
  if (nrow(df) == 0) stop("No valid weight data to plot (all dates were implausible or missing).")
  # Baseline = first observation per animal
  first <- stats::aggregate(weight ~ animal_id, data = df, FUN = function(x) x[1])
  names(first)[2] <- "baseline_weight"
  df <- merge(df, first, by = "animal_id")
  df$pct_change <- 100 * (df$weight - df$baseline_weight) / df$baseline_weight
  has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)
  if (has_ggplot) {
    y_var <- if (percent_change) "pct_change" else "weight"
    y_lab <- if (percent_change) "Weight (% change from baseline)" else "Weight (g)"
    p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = .data[[y_var]], group = animal_id)) +
      ggplot2::geom_line(alpha = 0.5, linewidth = 0.4) +
      ggplot2::labs(x = "Date", y = y_lab, title = "Weight trajectories")
    if (by_treatment && "treatment" %in% names(df)) {
      p <- p + ggplot2::aes(colour = treatment) +
        ggplot2::scale_color_manual(values = c("CT-26" = "#E41A1C", "Vehicle" = "#377EB8"))
      if (show_mean) {
        mean_df <- stats::aggregate(
          stats::as.formula(paste(y_var, "~ date + treatment")),
          data = df, FUN = mean, na.rm = TRUE
        )
        names(mean_df)[names(mean_df) == y_var] <- "mean_val"
        p <- p + ggplot2::geom_line(
          data = mean_df,
          ggplot2::aes(x = date, y = mean_val, group = treatment, colour = treatment),
          inherit.aes = FALSE, linewidth = 1.2
        )
      }
    } else if (show_mean) {
      mean_df <- stats::aggregate(
        stats::as.formula(paste(y_var, "~ date")),
        data = df, FUN = mean, na.rm = TRUE
      )
      names(mean_df)[2] <- "mean_val"
      p <- p + ggplot2::geom_line(
        data = mean_df,
        ggplot2::aes(x = date, y = mean_val, group = 1),
        inherit.aes = FALSE, colour = "black", linewidth = 1.2
      )
    }
    if (percent_change) p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "gray40")
    # Explicit date scale so x-axis shows calendar dates (e.g. Oct 15, 2025), not years 1900/1950/2000
    p <- p + ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d\n%Y", limits = range(df$date, na.rm = TRUE))
    p <- p + ggplot2::theme_minimal()
    return(p)
  }
  # Base R fallback
  df <- df[order(df$animal_id, df$date), ]
  u_animals <- unique(df$animal_id)
  y_var <- if (percent_change) "pct_change" else "weight"
  x_range <- range(df$date, na.rm = TRUE)
  y_range <- range(df[[y_var]], na.rm = TRUE)
  graphics::plot(x_range, y_range, type = "n", xlab = "Date", ylab = if (percent_change) "% change" else "Weight (g)", main = "Weight trajectories")
  for (a in u_animals) {
    sub <- df[df$animal_id == a, ]
    col <- if (by_treatment && "treatment" %in% names(sub)) ifelse(sub$treatment[1] == "CT-26", 2, 4) else 1
    graphics::lines(sub$date, sub[[y_var]], col = col)
  }
  if (percent_change) graphics::abline(h = 0, lty = 2, col = "gray")
  invisible(NULL)
}

#' Calculate time to first X% weight loss from baseline (onset of cachexia by weight).
#' @param weights_with_treatment data.frame with animal_id, date, weight, treatment
#' @param threshold_percent Percent weight loss threshold (e.g. 10)
#' @param baseline_date Optional; baseline = weight at this date (or first date if NULL)
#' @return data.frame with animal_id, treatment, first_below_date, days_to_onset, baseline_weight
calculate_weight_loss_onset <- function(weights_with_treatment,
                                        threshold_percent = getOption("cachexia_weight_loss_pct", WEIGHT_LOSS_PERCENT_THRESHOLD),
                                        baseline_date = NULL) {
  df <- as.data.frame(weights_with_treatment)
  df <- df[!is.na(df$weight) & !is.na(df$date), ]
  if (nrow(df) == 0) {
    return(data.frame(
      animal_id = character(), treatment = character(),
      first_below_date = as.Date(character()), days_to_onset = integer(),
      baseline_weight = numeric()
    ))
  }
  # Baseline per animal: first date or weight at baseline_date
  if (!is.null(baseline_date)) {
    baseline_date <- as.Date(baseline_date)
    df_b <- df[df$date == baseline_date, c("animal_id", "weight")]
    names(df_b)[2] <- "baseline_weight"
  } else {
    first <- stats::aggregate(date ~ animal_id, data = df, FUN = min)
    df_first <- merge(df, first, by = c("animal_id", "date"))
    df_b <- df_first[, c("animal_id", "weight")]
    names(df_b)[2] <- "baseline_weight"
  }
  df <- merge(df, df_b, by = "animal_id")
  df$pct_loss <- 100 * (1 - df$weight / df$baseline_weight)
  below <- df[df$pct_loss >= threshold_percent, ]
  if (nrow(below) == 0) {
    meta <- unique(df[, c("animal_id", "treatment")])
    no_event <- merge(meta, df_b, by = "animal_id")
    no_event$first_below_date <- as.Date(NA)
    no_event$days_to_onset <- NA_integer_
    return(no_event[, c("animal_id", "treatment", "first_below_date", "days_to_onset", "baseline_weight")])
  }
  first_below <- stats::aggregate(date ~ animal_id, data = below, FUN = min)
  names(first_below)[2] <- "first_below_date"
  study_start <- if (is.null(baseline_date)) min(df$date, na.rm = TRUE) else baseline_date
  first_below$days_to_onset <- as.integer(first_below$first_below_date - study_start)
  first_below <- merge(first_below, df_b, by = "animal_id")
  meta <- unique(df[, c("animal_id", "treatment")])
  out <- merge(first_below, meta, by = "animal_id", all.x = TRUE)
  out[order(out$first_below_date), ]
}

#' Compare weight (or percent change) between treatment groups.
#' @param weights_with_treatment data.frame with animal_id, date, weight, treatment
#' @param timepoint Optional date for cross-sectional comparison
#' @param use_percent_change If TRUE, compare percent change from baseline at timepoint
#' @param baseline_date Optional; required if use_percent_change is TRUE
#' @return List with summary, test result, effect size
weight_group_comparison <- function(weights_with_treatment,
                                    timepoint = NULL,
                                    use_percent_change = FALSE,
                                    baseline_date = NULL) {
  df <- as.data.frame(weights_with_treatment)
  df <- df[!is.na(df$weight) & !is.na(df$date), ]
  if (is.null(timepoint)) {
    last <- stats::aggregate(date ~ animal_id, data = df, FUN = max)
    df_last <- merge(df, last, by = c("animal_id", "date"))
    vals <- df_last[, c("animal_id", "weight", "treatment")]
    names(vals)[2] <- "value"
  } else {
    timepoint <- as.Date(timepoint)
    df_t <- df[df$date == timepoint, ]
    if (nrow(df_t) == 0) stop("No weight data at timepoint ", timepoint)
    if (use_percent_change) {
      if (is.null(baseline_date)) baseline_date <- min(df$date, na.rm = TRUE)
      baseline_date <- as.Date(baseline_date)
      df_b <- df[df$date == baseline_date, c("animal_id", "weight")]
      names(df_b)[2] <- "baseline_weight"
      df_t <- merge(df_t, df_b, by = "animal_id")
      df_t$value <- 100 * (df_t$weight - df_t$baseline_weight) / df_t$baseline_weight
    } else {
      df_t$value <- df_t$weight
    }
    vals <- df_t[, c("animal_id", "value", "treatment")]
  }
  ct26 <- vals$value[vals$treatment == "CT-26"]
  vehicle <- vals$value[vals$treatment == "Vehicle"]
  if (length(ct26) < 2 || length(vehicle) < 2) {
    return(list(summary = NULL, test = NULL, message = "Insufficient samples per group"))
  }
  summary_df <- data.frame(
    treatment = c("CT-26", "Vehicle"),
    n = c(length(ct26), length(vehicle)),
    mean = c(mean(ct26, na.rm = TRUE), mean(vehicle, na.rm = TRUE)),
    sd = c(stats::sd(ct26, na.rm = TRUE), stats::sd(vehicle, na.rm = TRUE))
  )
  test_norm <- tryCatch(stats::shapiro.test(c(ct26, vehicle))$p.value > 0.05, error = function(e) FALSE)
  if (test_norm) {
    tt <- stats::t.test(ct26, vehicle)
    test_result <- list(statistic = tt$statistic, p.value = tt$p.value, method = tt$method)
  } else {
    wt <- stats::wilcox.test(ct26, vehicle, exact = FALSE)
    test_result <- list(statistic = wt$statistic, p.value = wt$p.value, method = wt$method)
  }
  pooled_sd <- sqrt((var(ct26, na.rm = TRUE) + var(vehicle, na.rm = TRUE)) / 2)
  cohens_d <- if (pooled_sd > 0) (mean(ct26, na.rm = TRUE) - mean(vehicle, na.rm = TRUE)) / pooled_sd else NA
  list(
    summary = summary_df,
    test = test_result,
    effect_size_cohens_d = cohens_d
  )
}

#' Area-under-curve for weight loss (e.g. percent change from baseline over time).
#' Higher magnitude = more weight loss over study.
#' @param weights_with_treatment data.frame with animal_id, date, weight, treatment
#' @param baseline_date Optional; baseline for percent change
#' @return data.frame with animal_id, treatment, weight_auc (area under percent-change curve)
weight_auc_analysis <- function(weights_with_treatment, baseline_date = NULL) {
  df <- as.data.frame(weights_with_treatment)
  df <- df[!is.na(df$weight) & !is.na(df$date), ]
  if (nrow(df) == 0) stop("No weight data.")
  if (is.null(baseline_date)) {
    first <- stats::aggregate(date ~ animal_id, data = df, FUN = min)
    df_first <- merge(df, first, by = c("animal_id", "date"))
    df_b <- df_first[, c("animal_id", "weight")]
    names(df_b)[2] <- "baseline_weight"
  } else {
    baseline_date <- as.Date(baseline_date)
    df_b <- df[df$date == baseline_date, c("animal_id", "weight")]
    names(df_b)[2] <- "baseline_weight"
  }
  df <- merge(df, df_b, by = "animal_id")
  df$pct_change <- 100 * (df$weight - df$baseline_weight) / df$baseline_weight
  df <- df[order(df$animal_id, df$date), ]
  # AUC via trapezoidal rule per animal (x = days from first date, y = pct_change)
  study_start <- min(df$date, na.rm = TRUE)
  df$day <- as.numeric(df$date - study_start)
  animal_ids <- unique(df$animal_id)
  auc_vec <- numeric(length(animal_ids))
  for (i in seq_along(animal_ids)) {
    sub <- df[df$animal_id == animal_ids[i], ]
    if (nrow(sub) < 2) { auc_vec[i] <- NA; next }
    x <- sub$day
    y <- sub$pct_change
    auc_vec[i] <- sum(diff(x) * (y[-1] + y[-length(y)]) / 2, na.rm = TRUE)
  }
  meta <- unique(df[, c("animal_id", "treatment")])
  out <- data.frame(animal_id = animal_ids, weight_auc = auc_vec, stringsAsFactors = FALSE)
  merge(out, meta, by = "animal_id")
}
