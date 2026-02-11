# Phase 2: BCS (Body Condition Score) Analysis Module
# Requires: ggplot2 (optional for plots)

#' Plot BCS trajectories by animal and/or treatment group.
#' @param bcs_with_treatment data.frame with animal_id, date, bcs, treatment (from merge_all_data()$bcs_with_treatment)
#' @param by_treatment If TRUE, facet or color by treatment; if FALSE, plot all animals
#' @param show_mean If TRUE, add group mean trajectory line
#' @param threshold Optional BCS threshold line (e.g. 3 for cachexia)
#' @return ggplot object (if ggplot2 available) or base R plot
plot_bcs_trajectories <- function(bcs_with_treatment,
                                 by_treatment = TRUE,
                                 show_mean = TRUE,
                                 threshold = getOption("cachexia_bcs_threshold", BCS_CACHEXIA_THRESHOLD)) {
  df <- as.data.frame(bcs_with_treatment)
  df <- df[!is.na(df$bcs) & !is.na(df$date), ]
  if (nrow(df) == 0) stop("No valid BCS data to plot.")
  has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)
  if (has_ggplot) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = bcs, group = animal_id)) +
      ggplot2::geom_line(alpha = 0.5, linewidth = 0.4) +
      ggplot2::labs(x = "Date", y = "Body Condition Score", title = "BCS trajectories")
    if (by_treatment && "treatment" %in% names(df)) {
      p <- p + ggplot2::aes(colour = treatment) +
        ggplot2::scale_color_manual(values = c("CT-26" = "#E41A1C", "Vehicle" = "#377EB8"))
      if (show_mean) {
        mean_df <- stats::aggregate(bcs ~ date + treatment, data = df, FUN = mean, na.rm = TRUE)
        names(mean_df)[names(mean_df) == "bcs"] <- "bcs_mean"
        p <- p + ggplot2::geom_line(
          data = mean_df,
          ggplot2::aes(x = date, y = bcs_mean, group = treatment, colour = treatment),
          inherit.aes = FALSE, linewidth = 1.2
        )
      }
    } else if (show_mean) {
      mean_df <- stats::aggregate(bcs ~ date, data = df, FUN = mean, na.rm = TRUE)
      names(mean_df)[names(mean_df) == "bcs"] <- "bcs_mean"
      p <- p + ggplot2::geom_line(
        data = mean_df,
        ggplot2::aes(x = date, y = bcs_mean, group = 1),
        inherit.aes = FALSE, colour = "black", linewidth = 1.2
      )
    }
    if (!is.null(threshold)) {
      p <- p + ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", colour = "gray40")
    }
    p <- p + ggplot2::theme_minimal()
    return(p)
  }
  # Fallback: base R
  df <- df[order(df$animal_id, df$date), ]
  u_animals <- unique(df$animal_id)
  x_range <- range(df$date, na.rm = TRUE)
  y_range <- range(c(df$bcs, threshold), na.rm = TRUE)
  graphics::plot(x_range, y_range, type = "n", xlab = "Date", ylab = "BCS", main = "BCS trajectories")
  for (a in u_animals) {
    sub <- df[df$animal_id == a, ]
    graphics::lines(sub$date, sub$bcs, col = if (by_treatment && "treatment" %in% names(sub)) ifelse(sub$treatment[1] == "CT-26", 2, 4) else 1)
  }
  if (!is.null(threshold)) graphics::abline(h = threshold, lty = 2, col = "gray")
  invisible(NULL)
}

#' BCS trajectories with mean line and SEM error bars per treatment.
#' @param bcs_with_treatment data.frame with animal_id, date, bcs, treatment
#' @param threshold Optional BCS threshold line (e.g. 3)
#' @return ggplot object
plot_bcs_trajectories_with_sem <- function(bcs_with_treatment,
                                           threshold = getOption("cachexia_bcs_threshold", BCS_CACHEXIA_THRESHOLD)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package ggplot2 is required.")
  df <- as.data.frame(bcs_with_treatment)
  df <- df[!is.na(df$bcs) & !is.na(df$date), ]
  if (nrow(df) == 0) stop("No valid BCS data to plot.")
  if (!"treatment" %in% names(df)) stop("Treatment column required for SEM plot.")
  n_per <- stats::aggregate(bcs ~ date + treatment, data = df, FUN = length)
  names(n_per)[3] <- "n"
  mean_df <- stats::aggregate(bcs ~ date + treatment, data = df, FUN = mean, na.rm = TRUE)
  names(mean_df)[3] <- "bcs_mean"
  sd_df <- stats::aggregate(bcs ~ date + treatment, data = df, FUN = function(x) stats::sd(x, na.rm = TRUE))
  names(sd_df)[3] <- "bcs_sd"
  sum_df <- merge(merge(mean_df, sd_df, by = c("date", "treatment")), n_per, by = c("date", "treatment"))
  sum_df$sem <- sum_df$bcs_sd / sqrt(sum_df$n)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = bcs, group = animal_id, colour = treatment)) +
    ggplot2::geom_line(alpha = 0.5, linewidth = 0.4) +
    ggplot2::geom_line(
      data = sum_df,
      ggplot2::aes(x = date, y = bcs_mean, group = treatment),
      inherit.aes = FALSE, linewidth = 1.2
    ) +
    ggplot2::geom_errorbar(
      data = sum_df,
      ggplot2::aes(x = date, ymin = bcs_mean - sem, ymax = bcs_mean + sem, colour = treatment),
      inherit.aes = FALSE, linewidth = 0.8, width = 0.5
    ) +
    ggplot2::scale_color_manual(values = c("CT-26" = "#E41A1C", "Vehicle" = "#377EB8")) +
    ggplot2::labs(x = "Date", y = "Body Condition Score", title = "BCS trajectories (mean \u00B1 SEM)") +
    ggplot2::theme_minimal()
  if (!is.null(threshold)) p <- p + ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", colour = "gray40")
  p
}

#' BCS trajectories as scatter (individual points) instead of lines.
#' @param bcs_with_treatment data.frame with animal_id, date, bcs, treatment
#' @param show_mean If TRUE, add group mean trajectory line
#' @param threshold Optional BCS threshold line (e.g. 3)
#' @return ggplot object
plot_bcs_trajectories_scatter <- function(bcs_with_treatment,
                                          show_mean = TRUE,
                                          threshold = getOption("cachexia_bcs_threshold", BCS_CACHEXIA_THRESHOLD)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package ggplot2 is required.")
  df <- as.data.frame(bcs_with_treatment)
  df <- df[!is.na(df$bcs) & !is.na(df$date), ]
  if (nrow(df) == 0) stop("No valid BCS data to plot.")
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = bcs, colour = treatment)) +
    ggplot2::geom_point(alpha = 0.6, size = 2) +
    ggplot2::scale_color_manual(values = c("CT-26" = "#E41A1C", "Vehicle" = "#377EB8")) +
    ggplot2::labs(x = "Date", y = "Body Condition Score", title = "BCS trajectories (scatter)") +
    ggplot2::theme_minimal()
  if (show_mean && "treatment" %in% names(df)) {
    mean_df <- stats::aggregate(bcs ~ date + treatment, data = df, FUN = mean, na.rm = TRUE)
    names(mean_df)[names(mean_df) == "bcs"] <- "bcs_mean"
    p <- p + ggplot2::geom_line(
      data = mean_df,
      ggplot2::aes(x = date, y = bcs_mean, group = treatment, colour = treatment),
      inherit.aes = FALSE, linewidth = 1.2
    )
  }
  if (!is.null(threshold)) p <- p + ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", colour = "gray40")
  p
}

#' BCS trajectories with treatment in separate facets.
#' @param bcs_with_treatment data.frame with animal_id, date, bcs, treatment
#' @param show_mean If TRUE, add mean trajectory line per facet
#' @param threshold Optional BCS threshold line (e.g. 3)
#' @return ggplot object
plot_bcs_trajectories_faceted <- function(bcs_with_treatment,
                                          show_mean = TRUE,
                                          threshold = getOption("cachexia_bcs_threshold", BCS_CACHEXIA_THRESHOLD)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package ggplot2 is required.")
  df <- as.data.frame(bcs_with_treatment)
  df <- df[!is.na(df$bcs) & !is.na(df$date), ]
  if (nrow(df) == 0) stop("No valid BCS data to plot.")
  if (!"treatment" %in% names(df)) stop("Treatment column required for faceted plot.")
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = bcs, group = animal_id)) +
    ggplot2::geom_line(alpha = 0.5, linewidth = 0.4) +
    ggplot2::facet_wrap(ggplot2::vars(treatment), ncol = 1) +
    ggplot2::labs(x = "Date", y = "Body Condition Score", title = "BCS trajectories by treatment") +
    ggplot2::theme_minimal()
  if (show_mean) {
    mean_df <- stats::aggregate(bcs ~ date + treatment, data = df, FUN = mean, na.rm = TRUE)
    names(mean_df)[names(mean_df) == "bcs"] <- "bcs_mean"
    p <- p + ggplot2::geom_line(
      data = mean_df,
      ggplot2::aes(x = date, y = bcs_mean, group = 1),
      inherit.aes = FALSE, colour = "black", linewidth = 1.2
    )
  }
  if (!is.null(threshold)) p <- p + ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", colour = "gray40")
  p
}

#' Calculate time to first BCS below threshold (onset of cachexia by BCS).
#' @param bcs_with_treatment data.frame with animal_id, date, bcs, treatment
#' @param threshold BCS threshold (first date where bcs < threshold)
#' @param baseline_date Optional; only consider dates on or after this (e.g. study start)
#' @return data.frame with animal_id, treatment, first_below_date, days_to_onset (from baseline_date or min date)
calculate_bcs_onset <- function(bcs_with_treatment,
                                threshold = getOption("cachexia_bcs_threshold", BCS_CACHEXIA_THRESHOLD),
                                baseline_date = NULL) {
  df <- as.data.frame(bcs_with_treatment)
  df <- df[!is.na(df$bcs) & !is.na(df$date), ]
  if (!is.null(baseline_date)) {
    baseline_date <- as.Date(baseline_date)
    df <- df[df$date >= baseline_date, ]
  }
  below <- df[df$bcs < threshold, ]
  if (nrow(below) == 0) {
    return(data.frame(
      animal_id = character(), treatment = character(),
      first_below_date = as.Date(character()), days_to_onset = integer()
    ))
  }
  first_below <- stats::aggregate(date ~ animal_id, data = below, FUN = min)
  names(first_below)[2] <- "first_below_date"
  study_start <- if (is.null(baseline_date)) min(df$date, na.rm = TRUE) else baseline_date
  first_below$days_to_onset <- as.integer(first_below$first_below_date - study_start)
  meta <- unique(df[, c("animal_id", "treatment")])
  out <- merge(first_below, meta, by = "animal_id", all.x = TRUE)
  out[order(out$first_below_date), ]
}

#' Compare BCS between treatment groups (e.g. at a given timepoint or as change from baseline).
#' @param bcs_with_treatment data.frame with animal_id, date, bcs, treatment
#' @param timepoint Optional date; if given, compare BCS at this date
#' @param baseline_date Optional; if given with timepoint, compute change from baseline at timepoint
#' @return List with summary stats, test result (t-test or Wilcoxon), and effect size
bcs_group_comparison <- function(bcs_with_treatment,
                                 timepoint = NULL,
                                 baseline_date = NULL) {
  df <- as.data.frame(bcs_with_treatment)
  df <- df[!is.na(df$bcs) & !is.na(df$date), ]
  if (is.null(timepoint)) {
    # Use last observed date per animal
    last <- stats::aggregate(date ~ animal_id, data = df, FUN = max)
    df_last <- merge(df, last, by = c("animal_id", "date"))
    vals <- df_last[, c("animal_id", "bcs", "treatment")]
    names(vals)[names(vals) == "bcs"] <- "value"
  } else {
    timepoint <- as.Date(timepoint)
    df_t <- df[df$date == timepoint, ]
    if (nrow(df_t) == 0) stop("No data at timepoint ", timepoint)
    if (!is.null(baseline_date)) {
      baseline_date <- as.Date(baseline_date)
      df_b <- df[df$date == baseline_date, c("animal_id", "bcs")]
      names(df_b)[2] <- "bcs_baseline"
      df_t <- merge(df_t, df_b, by = "animal_id")
      df_t$bcs_change <- df_t$bcs - df_t$bcs_baseline
      vals <- df_t[, c("animal_id", "bcs_change", "treatment")]
      names(vals)[2] <- "value"
    } else {
      vals <- df_t[, c("animal_id", "bcs", "treatment")]
      names(vals)[2] <- "value"
    }
  }
  if (!"value" %in% names(vals)) vals$value <- vals$bcs
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
    test_used <- "t.test"
    test_result <- list(statistic = tt$statistic, p.value = tt$p.value, method = tt$method)
  } else {
    wt <- stats::wilcox.test(ct26, vehicle, exact = FALSE)
    test_used <- "wilcox.test"
    test_result <- list(statistic = wt$statistic, p.value = wt$p.value, method = wt$method)
  }
  pooled_sd <- sqrt((var(ct26, na.rm = TRUE) + var(vehicle, na.rm = TRUE)) / 2)
  cohens_d <- if (pooled_sd > 0) (mean(ct26, na.rm = TRUE) - mean(vehicle, na.rm = TRUE)) / pooled_sd else NA
  list(
    summary = summary_df,
    test = test_result,
    effect_size_cohens_d = cohens_d,
    value_type = if (!is.null(baseline_date)) "change_from_baseline" else "BCS"
  )
}

#' Time-to-event analysis for BCS decline (e.g. first time BCS < threshold).
#' @param bcs_with_treatment data.frame with animal_id, date, bcs, treatment
#' @param threshold BCS threshold for "event"
#' @param baseline_date Study start; days computed from this
#' @return List with survival object summary and optional Kaplan-Meier stats
bcs_survival_analysis <- function(bcs_with_treatment,
                                  threshold = getOption("cachexia_bcs_threshold", BCS_CACHEXIA_THRESHOLD),
                                  baseline_date = NULL) {
  onset <- calculate_bcs_onset(bcs_with_treatment, threshold = threshold, baseline_date = baseline_date)
  if (nrow(onset) == 0) return(list(message = "No events (no BCS below threshold)."))
  df <- as.data.frame(bcs_with_treatment)
  df <- df[!is.na(df$date), ]
  study_start <- if (is.null(baseline_date)) min(df$date, na.rm = TRUE) else as.Date(baseline_date)
  last_date <- stats::aggregate(date ~ animal_id, data = df, FUN = max)
  names(last_date)[2] <- "last_date"
  all_animals <- unique(df[, c("animal_id", "treatment")])
  all_animals <- merge(all_animals, last_date, by = "animal_id")
  all_animals$event <- all_animals$animal_id %in% onset$animal_id
  all_animals$time_days <- as.integer(all_animals$last_date - study_start)
  all_animals <- merge(
    all_animals,
    onset[, c("animal_id", "days_to_onset")],
    by = "animal_id",
    all.x = TRUE
  )
  all_animals$time <- ifelse(all_animals$event, all_animals$days_to_onset, all_animals$time_days)
  if (!requireNamespace("survival", quietly = TRUE)) {
    return(list(
      data = all_animals,
      message = "Package 'survival' not installed; returning event data only."
    ))
  }
  surv_obj <- survival::Surv(time = all_animals$time, event = all_animals$event)
  fit <- survival::survfit(surv_obj ~ treatment, data = all_animals)
  list(
    data = all_animals,
    surv_fit = fit,
    surv_summary = summary(fit)
  )
}

#' Plot Kaplan-Meier survival curve for BCS-defined event (e.g. BCS < threshold).
#' Step curves with % survival (0-100), Vehicle = blue circles, CT-26 = red squares.
#' @param bcs_with_treatment data.frame with animal_id, date, bcs, treatment (or result of bcs_survival_analysis())
#' @param threshold BCS threshold for event (ignored if first argument is survival result)
#' @param baseline_date Study start (ignored if first argument is survival result)
#' @param xlab X-axis label (default "Days")
#' @param ylab Y-axis label (default "% Survival")
#' @return Invisibly returns the survfit object; side effect is base R plot
plot_bcs_survival_curve <- function(bcs_with_treatment,
                                    threshold = getOption("cachexia_bcs_threshold", BCS_CACHEXIA_THRESHOLD),
                                    baseline_date = NULL,
                                    xlab = "Days",
                                    ylab = "% Survival") {
  if (!requireNamespace("survival", quietly = TRUE)) stop("Package 'survival' is required for plot_bcs_survival_curve.")
  if (is.list(bcs_with_treatment) && !is.null(bcs_with_treatment$surv_fit)) {
    fit <- bcs_with_treatment$surv_fit
  } else {
    res <- bcs_survival_analysis(bcs_with_treatment, threshold = threshold, baseline_date = baseline_date)
    if (is.null(res$surv_fit)) stop(if (!is.null(res$message)) res$message else "No survival fit available.")
    fit <- res$surv_fit
  }
  strata_names <- names(fit$strata)
  n_strata <- length(strata_names)
  col_vec <- character(n_strata)
  pch_vec <- integer(n_strata)
  for (i in seq_along(strata_names)) {
    nm <- strata_names[i]
    if (grepl("Vehicle", nm, ignore.case = TRUE)) {
      col_vec[i] <- "#377EB8"
      pch_vec[i] <- 21L
    } else {
      col_vec[i] <- "#E41A1C"
      pch_vec[i] <- 22L
    }
  }
  graphics::plot(fit, fun = "pct", col = col_vec, lwd = 2, xlab = xlab, ylab = ylab, main = "", mark.time = FALSE)
  s <- summary(fit)
  if (!is.null(s$strata) && length(s$strata) == length(s$time)) {
    lev <- levels(s$strata)
    for (k in seq_along(lev)) {
      idx <- which(s$strata == lev[k])
      if (length(idx)) graphics::points(s$time[idx], 100 * s$surv[idx], pch = pch_vec[k], col = col_vec[k], bg = col_vec[k], cex = 1.2)
    }
  } else {
    graphics::points(s$time, 100 * s$surv, pch = pch_vec[1L], col = col_vec[1L], bg = col_vec[1L], cex = 1.2)
  }
  legend_labels <- sub("^[^=]+=", "", strata_names)
  graphics::legend("bottomleft", legend = legend_labels, col = col_vec, pch = pch_vec, pt.bg = col_vec, bty = "n", inset = 0.02)
  invisible(fit)
}
