# Phase 4: Sensitivity and Power Analysis
# Compares digital vs manual methods: effect sizes, power, ROC, bootstrap CI

#' Calculate Cohen's d effect size for two groups (CT-26 vs Vehicle).
#' @param x Numeric vector of values (e.g. last BCS, weight AUC, or digital feature)
#' @param group Character or factor: "CT-26" vs "Vehicle"
#' @return Numeric: Cohen's d (positive = CT-26 higher than Vehicle on average)
effect_size_cohens_d <- function(x, group) {
  group <- as.character(group)
  g1 <- x[group == "CT-26"]
  g2 <- x[group == "Vehicle"]
  g1 <- g1[!is.na(g1)]
  g2 <- g2[!is.na(g2)]
  if (length(g1) < 2 || length(g2) < 2) return(NA_real_)
  m1 <- mean(g1, na.rm = TRUE)
  m2 <- mean(g2, na.rm = TRUE)
  s1 <- stats::sd(g1)
  s2 <- stats::sd(g2)
  if (is.na(s1)) s1 <- 0
  if (is.na(s2)) s2 <- 0
  n1 <- length(g1)
  n2 <- length(g2)
  pooled_sd <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  if (is.na(pooled_sd) || pooled_sd <= 0) return(NA_real_)
  (m1 - m2) / pooled_sd
}

#' Calculate effect sizes for multiple outcomes (e.g. BCS at endpoint, weight AUC, digital feature).
#' @param data data.frame with columns: animal_id, treatment, and one or more value columns
#' @param value_cols Character vector of column names to compute Cohen's d for
#' @return data.frame with outcome, cohens_d, n_ct26, n_vehicle
calculate_effect_sizes <- function(data, value_cols) {
  data <- as.data.frame(data)
  if (!"treatment" %in% names(data)) stop("data must have column treatment")
  out <- data.frame(
    outcome = character(length(value_cols)),
    cohens_d = numeric(length(value_cols)),
    n_ct26 = integer(length(value_cols)),
    n_vehicle = integer(length(value_cols)),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(value_cols)) {
    col <- value_cols[i]
    if (!col %in% names(data)) { out$cohens_d[i] <- NA; out$outcome[i] <- col; next }
    out$outcome[i] <- col
    out$cohens_d[i] <- effect_size_cohens_d(data[[col]], data$treatment)
    out$n_ct26[i] <- sum(data$treatment == "CT-26" & !is.na(data[[col]]))
    out$n_vehicle[i] <- sum(data$treatment == "Vehicle" & !is.na(data[[col]]))
  }
  out
}

#' Sample size needed per group to detect a given effect size with desired power.
#' @param cohens_d Effect size (Cohen's d)
#' @param power Desired power (default 0.8)
#' @param sig_level Alpha (default 0.05)
#' @param two_sided If TRUE, two-sided test (default TRUE)
#' @return List with n_per_group and inputs
power_analysis_comparison <- function(cohens_d,
                                     power = 0.8,
                                     sig_level = 0.05,
                                     two_sided = TRUE) {
  if (is.na(cohens_d) || abs(cohens_d) < 1e-6) {
    return(list(n_per_group = NA_integer_, message = "Effect size too small or NA"))
  }
  pt <- stats::power.t.test(
    delta = abs(cohens_d),
    sd = 1,
    sig.level = sig_level,
    power = power,
    type = "two.sample",
    alternative = if (two_sided) "two.sided" else "one.sided"
  )
  list(
    n_per_group = ceiling(pt$n),
    power = power,
    sig_level = sig_level,
    cohens_d = cohens_d,
    two_sided = two_sided
  )
}

#' ROC analysis: predict cachexia status (by BCS or weight) using a continuous predictor (e.g. digital feature).
#' @param cachexia_status Integer or logical: 1/TRUE = cachexia, 0/FALSE = no cachexia (e.g. BCS < 3 at endpoint)
#' @param predictor Numeric predictor (e.g. daily activity at a timepoint; lower may indicate cachexia)
#' @param direction "auto", "<", ">": direction of predictor (">" = higher predictor = more cachexia)
#' @return List with auc, roc_curve (data.frame of sensitivities, specificities), and optional pROC object
roc_analysis <- function(cachexia_status, predictor, direction = "auto") {
  cachexia_status <- as.integer(as.logical(cachexia_status))
  ok <- !is.na(cachexia_status) & !is.na(predictor)
  cachexia_status <- cachexia_status[ok]
  predictor <- predictor[ok]
  if (length(unique(cachexia_status)) < 2) return(list(auc = NA_real_, message = "Need both cases and controls"))
  n_pos <- sum(cachexia_status == 1)
  n_neg <- sum(cachexia_status == 0)
  if (n_pos < 1 || n_neg < 1) return(list(auc = NA_real_, message = "Need both cases and controls"))
  if (direction == "auto") {
    r <- stats::cor(predictor, cachexia_status, use = "complete.obs")
    direction <- if (!is.na(r) && r < 0) "<" else ">"
  }
  # AUC via Wilcoxon: rank predictor; AUC = (sum of ranks of cases - n_pos*(n_pos+1)/2) / (n_pos * n_neg)
  rk <- rank(predictor, ties.method = "average")
  sum_ranks_cases <- sum(rk[cachexia_status == 1])
  auc <- (sum_ranks_cases - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  if (direction == "<") auc <- 1 - auc  # lower predictor = more cachexia
  # ROC curve: thresholds = unique predictor values
  ord <- order(predictor, decreasing = (direction == ">"))
  pred_sorted <- predictor[ord]
  status_sorted <- cachexia_status[ord]
  n <- length(pred_sorted)
  sens <- c(1, numeric(n))
  spec <- c(0, numeric(n))
  for (i in seq_len(n)) {
    thresh <- pred_sorted[i]
    pred_pos <- predictor >= thresh
    if (direction == "<") pred_pos <- predictor <= thresh
    sens[i + 1] <- sum(pred_pos & cachexia_status == 1) / n_pos
    spec[i + 1] <- sum(!pred_pos & cachexia_status == 0) / n_neg
  }
  sens[n + 1] <- 0
  spec[n + 1] <- 1
  roc_df <- data.frame(sensitivity = sens, specificity = spec, one_minus_specificity = 1 - spec)
  list(auc = auc, roc_curve = roc_df, direction = direction, n_pos = n_pos, n_neg = n_neg)
}

#' Bootstrap confidence interval for a statistic (e.g. mean lead time or Cohen's d).
#' @param x Numeric vector (e.g. lead_time_days)
#' @param statistic Function that takes a vector and returns a single number (default mean)
#' @param n_bootstrap Number of bootstrap samples (default 2000)
#' @param ci_level Confidence level (default 0.95)
#' @return List with estimate, ci_lower, ci_upper, bootstrap_samples (optional)
bootstrap_confidence_intervals <- function(x,
                                          statistic = mean,
                                          n_bootstrap = 2000,
                                          ci_level = 0.95) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(list(estimate = statistic(x), ci_lower = NA, ci_upper = NA))
  set.seed(42)
  boots <- replicate(n_bootstrap, statistic(sample(x, replace = TRUE)))
  alpha <- 1 - ci_level
  list(
    estimate = statistic(x),
    ci_lower = stats::quantile(boots, alpha / 2),
    ci_upper = stats::quantile(boots, 1 - alpha / 2),
    ci_level = ci_level,
    n_bootstrap = n_bootstrap
  )
}
