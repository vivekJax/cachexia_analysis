# Phase 3: Digital Cage (Activity / Behavioral) Analysis Module
# Uses digital_with_treatment from merge_all_data(digital = load_digital_cage_data(...))
# Column names in CSV use hyphens; reference with backticks in data.table.

#' Return available numeric metric columns from digital cage data (excluding ids/dates).
#' @param digital data.table or data.frame from load_digital_cage_data()
#' @return Character vector of column names suitable for aggregation
digital_metric_columns <- function(digital) {
  skip <- c("start", "start.date.local", "start.time.local", "study.code", "aggregation.seconds",
            "group.name", "cage.name", "animals.cage.quantity", "light.cycle", "animal.id",
            "strain", "sex", "genotype", "birth.date", "treatment", "housing_id")
  nms <- names(digital)
  num <- vapply(digital, is.numeric, logical(1))
  nms[num & !nms %in% skip]
}

#' Aggregate digital cage metrics by animal and date (daily summaries).
#' @param digital_with_treatment data from merge_all_data(..., digital = dt)$digital_with_treatment
#' @param date_col Name of date column (default start.date.local)
#' @param metrics Optional character vector of metric columns; if NULL, uses digital_metric_columns(digital)
#' @return data.frame with animal_id (or animal.id), date, treatment, and daily mean of each metric
aggregate_digital_by_date <- function(digital_with_treatment,
                                     date_col = "start.date.local",
                                     metrics = NULL) {
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package data.table is required for aggregate_digital_by_date.")
  dt <- data.table::as.data.table(digital_with_treatment)
  id_col <- if ("animal.id" %in% names(dt)) "animal.id" else "animal_id"
  if (!date_col %in% names(dt)) stop("Date column ", date_col, " not found.")
  # Coerce date column to Date (character, numeric/R serial, or Excel serial)
  dvec <- dt[[date_col]]
  if (is.character(dvec)) {
    dt[, (date_col) := as.Date(get(date_col))]
  } else if (is.numeric(dvec) || inherits(dvec, "integer")) {
    dnum <- as.numeric(dvec)
    non_na <- dnum[!is.na(dnum)]
    if (length(non_na) > 0 && all(non_na > 40000)) {
      dt[, (date_col) := as.Date(as.integer(get(date_col)), origin = "1899-12-30")]
    } else {
      dt[, (date_col) := as.Date(as.integer(get(date_col)), origin = "1970-01-01")]
    }
  }
  # Exclude implausible dates (e.g. 1899-12-30) from aggregation
  dt[!is.na(get(date_col)) & get(date_col) < as.Date("2000-01-01"), (date_col) := as.Date(NA)]
  dt <- dt[!is.na(get(date_col))]
  if (is.null(metrics)) metrics <- digital_metric_columns(dt)
  metrics <- metrics[metrics %in% names(dt)]
  if (length(metrics) == 0) stop("No numeric metric columns found.")
  by_cols <- c(id_col, date_col)
  if ("treatment" %in% names(dt)) by_cols <- c(by_cols, "treatment")
  agg <- dt[, lapply(.SD, function(x) mean(as.numeric(x), na.rm = TRUE)),
            by = by_cols,
            .SDcols = metrics]
  out <- as.data.frame(agg)
  if (id_col == "animal.id") names(out)[names(out) == "animal.id"] <- "animal_id"
  if (date_col == "start.date.local") names(out)[names(out) == "start.date.local"] <- "date"
  # Ensure date column is base R Date so plots and serialization show calendar dates, not integers
  date_out_col <- if ("date" %in% names(out)) "date" else date_col
  dvec_out <- out[[date_out_col]]
  if (is.numeric(dvec_out) || inherits(dvec_out, "integer")) {
    out[[date_out_col]] <- as.Date(as.integer(dvec_out), origin = "1970-01-01")
  } else {
    out[[date_out_col]] <- as.Date(dvec_out)
  }
  out
}

#' Plot digital cage metric trajectories by treatment (daily means per animal, group mean line).
#' @param digital_daily data.frame from aggregate_digital_by_date() with animal_id, date, treatment, and metric columns
#' @param metric Name of one metric column to plot (e.g. activity-feeding.animal.percent.min)
#' @param by_treatment If TRUE, color by treatment and show group mean
#' @param show_mean If TRUE, add group mean trajectory
#' @return ggplot object or base R plot
plot_digital_trajectories <- function(digital_daily,
                                     metric,
                                     by_treatment = TRUE,
                                     show_mean = TRUE) {
  if (!metric %in% names(digital_daily)) stop("Metric '", metric, "' not in digital_daily.")
  df <- as.data.frame(digital_daily)
  date_col <- if ("date" %in% names(df)) "date" else "start.date.local"
  # Coerce to base R Date so x-axis shows calendar dates (handles integer/IDate from fread or serialization)
  dvec <- df[[date_col]]
  if (is.numeric(dvec) || inherits(dvec, "integer")) {
    df[[date_col]] <- as.Date(as.integer(dvec), origin = "1970-01-01")
  } else {
    df[[date_col]] <- as.Date(dvec)
  }
  df <- df[!is.na(df[[metric]]) & !is.na(df[[date_col]]), , drop = FALSE]
  # Drop implausible dates so x-axis is not 1900/1950/2000
  df <- df[df[[date_col]] >= as.Date("2000-01-01"), , drop = FALSE]
  if (nrow(df) == 0) stop("No valid data for metric ", metric, ".")
  id_col <- if ("animal_id" %in% names(df)) "animal_id" else "animal.id"
  has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)
  if (has_ggplot) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[date_col]], y = .data[[metric]], group = .data[[id_col]])) +
      ggplot2::geom_line(alpha = 0.5, linewidth = 0.4) +
      ggplot2::labs(x = "Date", y = metric, title = paste("Digital cage:", metric))
    if (by_treatment && "treatment" %in% names(df)) {
      p <- p + ggplot2::aes(colour = treatment) +
        ggplot2::scale_color_manual(values = c("CT-26" = "#E41A1C", "Vehicle" = "#377EB8"))
      if (show_mean) {
        mean_df <- stats::aggregate(
          stats::as.formula(paste0("`", metric, "` ~ ", date_col, " + treatment")),
          data = df, FUN = mean, na.rm = TRUE
        )
        names(mean_df)[names(mean_df) == metric] <- "mean_val"
        p <- p + ggplot2::geom_line(
          data = mean_df,
          ggplot2::aes(x = .data[[date_col]], y = mean_val, group = treatment, colour = treatment),
          inherit.aes = FALSE, linewidth = 1.2
        )
      }
    } else if (show_mean) {
      mean_df <- stats::aggregate(
        stats::as.formula(paste0("`", metric, "` ~ ", date_col)),
        data = df, FUN = mean, na.rm = TRUE
      )
      names(mean_df)[2] <- "mean_val"
      p <- p + ggplot2::geom_line(
        data = mean_df,
        ggplot2::aes(x = .data[[date_col]], y = mean_val, group = 1),
        inherit.aes = FALSE, colour = "black", linewidth = 1.2
      )
    }
    # Explicit date scale so x-axis shows calendar dates, not years 1900/1950/2000
    p <- p + ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d\n%Y", limits = range(df[[date_col]], na.rm = TRUE))
    p <- p + ggplot2::theme_minimal()
    return(p)
  }
  # Base R fallback
  df <- df[order(df[[id_col]], df[[date_col]]), ]
  u_animals <- unique(df[[id_col]])
  x_range <- range(df[[date_col]], na.rm = TRUE)
  y_range <- range(df[[metric]], na.rm = TRUE)
  graphics::plot(x_range, y_range, type = "n", xlab = "Date", ylab = metric,
                 main = paste("Digital cage:", metric))
  for (a in u_animals) {
    sub <- df[df[[id_col]] == a, ]
    col <- if (by_treatment && "treatment" %in% names(sub))
      ifelse(sub$treatment[1] == "CT-26", 2, 4) else 1
    graphics::lines(sub[[date_col]], sub[[metric]], col = col)
  }
  invisible(NULL)
}

#' Compare one digital metric between treatment groups (CT-26 vs Vehicle).
#' Uses daily-aggregated data and summarizes per animal (mean over time or AUC), then runs t-test or Wilcoxon.
#' @param digital_daily data.frame from aggregate_digital_by_date()
#' @param metric Name of one metric column
#' @param summary_per_animal "mean" (mean daily value per animal) or "auc" (area under daily curve)
#' @return List with summary (n, mean, sd per group), test (statistic, p.value, method), effect_size_cohens_d
digital_group_comparison <- function(digital_daily,
                                    metric,
                                    summary_per_animal = c("mean", "auc")) {
  summary_per_animal <- match.arg(summary_per_animal)
  if (!metric %in% names(digital_daily)) stop("Metric '", metric, "' not in digital_daily.")
  df <- as.data.frame(digital_daily)
  df <- df[!is.na(df[[metric]]), ]
  id_col <- if ("animal_id" %in% names(df)) "animal_id" else "animal.id"
  date_col <- if ("date" %in% names(df)) "date" else "start.date.local"
  if (!"treatment" %in% names(df)) stop("digital_daily must contain treatment (use digital_with_treatment).")
  if (summary_per_animal == "mean") {
    per_animal <- stats::aggregate(
      stats::as.formula(paste0("`", metric, "` ~ ", id_col, " + treatment")),
      data = df, FUN = mean, na.rm = TRUE
    )
    names(per_animal)[names(per_animal) == metric] <- "value"
  } else {
    # AUC: area under curve (date as numeric days, metric as y)
    df <- df[order(df[[id_col]], df[[date_col]]), ]
    study_start <- min(df[[date_col]], na.rm = TRUE)
    df$day <- as.numeric(df[[date_col]] - study_start)
    animal_ids <- unique(df[[id_col]])
    auc_vec <- numeric(length(animal_ids))
    for (i in seq_along(animal_ids)) {
      sub <- df[df[[id_col]] == animal_ids[i], ]
      if (nrow(sub) < 2) { auc_vec[i] <- NA; next }
      x <- sub$day
      y <- sub[[metric]]
      auc_vec[i] <- sum(diff(x) * (y[-1] + y[-length(y)]) / 2, na.rm = TRUE)
    }
    meta <- unique(df[, c(id_col, "treatment")])
    per_animal <- data.frame(animal_id = animal_ids, value = auc_vec, stringsAsFactors = FALSE)
    if (id_col == "animal.id") names(per_animal)[1] <- id_col
    per_animal <- merge(per_animal, meta, by = id_col)
  }
  ct26 <- per_animal$value[per_animal$treatment == "CT-26"]
  vehicle <- per_animal$value[per_animal$treatment == "Vehicle"]
  if (length(ct26) < 2 || length(vehicle) < 2)
    return(list(summary = NULL, test = NULL, message = "Insufficient samples per group"))
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
    effect_size_cohens_d = cohens_d,
    metric = metric,
    summary_type = summary_per_animal
  )
}

#' Correlate a digital metric (daily mean per animal) with a longitudinal outcome (e.g. weight AUC or final BCS).
#' @param digital_daily data.frame from aggregate_digital_by_date()
#' @param outcome_df data.frame with animal_id and one outcome column (e.g. from weight_auc_analysis or last BCS)
#' @param outcome_col Name of outcome column in outcome_df
#' @param metric Name of digital metric column
#' @param digital_summary "mean" or "auc" for per-animal digital summary
#' @return List with correlation (Pearson/Spearman), test result, and merged data used
correlate_digital_with_outcome <- function(digital_daily,
                                           outcome_df,
                                           outcome_col,
                                           metric,
                                           digital_summary = c("mean", "auc")) {
  digital_summary <- match.arg(digital_summary)
  id_col <- if ("animal_id" %in% names(digital_daily)) "animal_id" else "animal.id"
  if (!metric %in% names(digital_daily)) stop("Metric '", metric, "' not in digital_daily.")
  if (!outcome_col %in% names(outcome_df)) stop("Outcome column '", outcome_col, "' not in outcome_df.")
  df <- as.data.frame(digital_daily)
  df <- df[!is.na(df[[metric]]), ]
  date_col <- if ("date" %in% names(df)) "date" else "start.date.local"
  if (digital_summary == "mean") {
    per_animal <- stats::aggregate(
      stats::as.formula(paste0("`", metric, "` ~ ", id_col)),
      data = df, FUN = mean, na.rm = TRUE
    )
  } else {
    df <- df[order(df[[id_col]], df[[date_col]]), ]
    study_start <- min(df[[date_col]], na.rm = TRUE)
    df$day <- as.numeric(df[[date_col]] - study_start)
    animal_ids <- unique(df[[id_col]])
    auc_vec <- numeric(length(animal_ids))
    for (i in seq_along(animal_ids)) {
      sub <- df[df[[id_col]] == animal_ids[i], ]
      if (nrow(sub) < 2) { auc_vec[i] <- NA; next }
      auc_vec[i] <- sum(diff(sub$day) * (sub[[metric]][-1] + sub[[metric]][-nrow(sub)]) / 2, na.rm = TRUE)
    }
    per_animal <- data.frame(animal_id = animal_ids, value = auc_vec, stringsAsFactors = FALSE)
    names(per_animal)[1] <- id_col
    names(per_animal)[2] <- metric
  }
  if (names(per_animal)[2] != metric) names(per_animal)[2] <- metric
  out_id <- if ("animal_id" %in% names(outcome_df)) "animal_id" else "animal.id"
  by_x <- id_col
  by_y <- out_id
  merged <- merge(per_animal, outcome_df[, c(out_id, outcome_col)], by.x = by_x, by.y = by_y, all = FALSE)
  merged <- merged[stats::complete.cases(merged[[metric]], merged[[outcome_col]]), ]
  if (nrow(merged) < 3)
    return(list(message = "Too few complete cases for correlation"))
  norm_ok <- tryCatch(stats::shapiro.test(merged[[metric]])$p.value > 0.05 &&
                        stats::shapiro.test(merged[[outcome_col]])$p.value > 0.05, error = function(e) FALSE)
  if (norm_ok) {
    cor_test <- stats::cor.test(merged[[metric]], merged[[outcome_col]], method = "pearson", exact = FALSE)
  } else {
    cor_test <- stats::cor.test(merged[[metric]], merged[[outcome_col]], method = "spearman", exact = FALSE)
  }
  list(
    correlation = cor_test$estimate,
    p.value = cor_test$p.value,
    method = cor_test$method,
    n = nrow(merged),
    data = merged,
    metric = metric,
    outcome_col = outcome_col
  )
}
