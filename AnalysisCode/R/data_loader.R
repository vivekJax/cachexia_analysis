# Phase 1: Data Loader Module for Cachexia Analysis
# Requires: readxl, data.table

#' Load treatment metadata and animal assignments from the Excel file.
#' @param excel_path Path to Final Cachexia in CD2 F1 mice with CT-26.xlsx
#' @return A data.frame with columns: animal_id, treatment, housing_id (cage), and other metadata
load_treatment_metadata <- function(excel_path = getOption("cachexia_excel_path", PATH_EXCEL)) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package readxl is required.")
  stopifnot(file.exists(excel_path))
  raw <- readxl::read_excel(excel_path, sheet = "BCS", col_types = "text")
  # First column is "Name" (row labels), rest are animal IDs
  animal_ids <- colnames(raw)[-1]
  # Extract key rows by name
  name_col <- raw[[1L]]
  treatment_row <- raw[name_col == "Cells or Vehicle", -1L]
  housing_row   <- raw[name_col == "Housing ID", -1L]
  treatment <- as.character(unlist(treatment_row[1, ]))
  housing_id <- as.character(unlist(housing_row[1, ]))
  out <- data.frame(
    animal_id = animal_ids,
    treatment = treatment,
    housing_id = housing_id,
    stringsAsFactors = FALSE
  )
  # Optional: add birth date, tag, etc. if needed
  birth_row <- raw[name_col == "Birth Date", -1L]
  if (nrow(birth_row)) {
    out$birth_date_excel <- as.numeric(as.character(unlist(birth_row[1, ])))
  }
  out
}

#' Reshape BCS sheet from wide (animals as columns) to long format.
#' @param excel_path Path to Excel file
#' @return data.frame with columns: animal_id, date, bcs (numeric)
load_bcs_data <- function(excel_path = getOption("cachexia_excel_path", PATH_EXCEL)) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package readxl is required.")
  stopifnot(file.exists(excel_path))
  raw <- readxl::read_excel(excel_path, sheet = "BCS", col_types = "text")
  name_col <- raw[[1L]]
  # Find the row index where BCS data starts (row after "BCS" label)
  bcs_label_idx <- which(name_col == "BCS")
  if (!length(bcs_label_idx)) stop("BCS sheet: 'BCS' row not found.")
  # Data rows: rows after "BCS" that look like dates (numeric Name)
  data_start <- bcs_label_idx + 1L
  animal_ids <- colnames(raw)[-1]
  out_list <- list()
  for (j in seq_along(animal_ids)) {
    id <- animal_ids[j]
    vals <- raw[[j + 1L]][data_start:nrow(raw)]
    dates <- raw[[1L]][data_start:nrow(raw)]
    dates_num <- suppressWarnings(as.numeric(dates))
    valid <- !is.na(dates_num) & !is.na(vals) & nzchar(trimws(vals))
    if (!any(valid)) next
    bcs_num <- suppressWarnings(as.numeric(vals[valid]))
    date_num <- dates_num[valid]
    date_r <- as.Date(date_num, origin = "1899-12-30")
    out_list[[id]] <- data.frame(animal_id = id, date = date_r, bcs = bcs_num, stringsAsFactors = FALSE)
  }
  if (!length(out_list)) stop("No BCS data found.")
  bcs_long <- do.call(rbind, out_list)
  rownames(bcs_long) <- NULL
  bcs_long
}

#' Reshape Weights sheet from wide to long format.
#' @param excel_path Path to Excel file
#' @return data.frame with columns: animal_id, date, weight (numeric)
load_weight_data <- function(excel_path = getOption("cachexia_excel_path", PATH_EXCEL)) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package readxl is required.")
  stopifnot(file.exists(excel_path))
  raw <- readxl::read_excel(excel_path, sheet = "Weights", col_types = "text")
  name_col <- raw[[1L]]
  weights_label_idx <- which(name_col == "Weights")
  if (!length(weights_label_idx)) stop("Weights sheet: 'Weights' row not found.")
  data_start <- weights_label_idx + 1L
  animal_ids <- colnames(raw)[-1]
  out_list <- list()
  # Resolve date column once: readxl may return Date/POSIXct (not text) or text as Excel serial or date strings
  date_col <- raw[[1L]]
  date_col_class <- class(date_col)[1L]
  if (date_col_class %in% c("Date", "POSIXct", "POSIXt")) {
    date_vals <- as.Date(date_col[data_start:nrow(raw)])
  } else {
    dates_chr <- as.character(date_col[data_start:nrow(raw)])
    dates_num <- suppressWarnings(as.numeric(dates_chr))
    n_na <- sum(is.na(dates_num))
    n_tot <- length(dates_num)
    if (n_na < n_tot) {
      # Numeric: try Excel 1899 then 1904 (Mac) origin
      date_vals_1899 <- as.Date(dates_num, origin = "1899-12-30")
      date_vals_1904 <- as.Date(dates_num, origin = "1904-01-01")
      y1899 <- as.integer(format(date_vals_1899[!is.na(date_vals_1899)], "%Y"))
      y1904 <- as.integer(format(date_vals_1904[!is.na(date_vals_1904)], "%Y"))
      if (any(y1904 >= 2000L & y1904 <= 2030L) && !any(y1899 >= 2000L & y1899 <= 2030L)) {
        date_vals <- date_vals_1904
      } else {
        date_vals <- date_vals_1899
      }
    } else {
      date_vals <- suppressWarnings(as.Date(dates_chr, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%b-%Y", "%Y-%m-%d")))
    }
  }
  # Drop implausible dates (Excel serial 0 -> 1899-12-30; invalid rows)
  date_vals[!is.na(date_vals) & date_vals < as.Date("2000-01-01")] <- NA
  for (j in seq_along(animal_ids)) {
    id <- animal_ids[j]
    vals <- raw[[j + 1L]][data_start:nrow(raw)]
    valid <- !is.na(date_vals) & nzchar(trimws(vals))
    if (!any(valid)) next
    weight_num <- suppressWarnings(as.numeric(vals[valid]))
    date_r <- date_vals[valid]
    out_list[[id]] <- data.frame(animal_id = id, date = date_r, weight = weight_num, stringsAsFactors = FALSE)
  }
  if (!length(out_list)) stop("No weight data found.")
  weight_long <- do.call(rbind, out_list)
  rownames(weight_long) <- NULL
  weight_long
}

#' Load digital cage 1-minute data efficiently using data.table.
#' @param csv_path Path to CCX_C1_animal_1min_*.csv
#' @param n_max Optional max rows to read (NULL = all)
#' @return data.table with digital cage data
load_digital_cage_data <- function(csv_path = getOption("cachexia_csv_path", PATH_DIGITAL_CSV), n_max = NULL) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package data.table is required.")
  stopifnot(file.exists(csv_path))
  if (is.null(n_max)) {
    dt <- data.table::fread(csv_path, showProgress = TRUE)
  } else {
    dt <- data.table::fread(csv_path, nrows = n_max, showProgress = TRUE)
  }
  # Parse start datetime if character
  if (is.character(dt[["start"]])) {
    dt[, start := as.POSIXct(start, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%S%z", "%Y-%m-%d %H:%M:%S"))]
  }
  # Parse start.date.local to Date; handle character, numeric (R or Excel serial), IDate
  dcol <- dt[["start.date.local"]]
  if (is.character(dcol)) {
    dt[, `start.date.local` := as.Date(`start.date.local`)]
  } else if (is.numeric(dcol) || inherits(dcol, "integer")) {
    if (all(dcol > 40000, na.rm = TRUE)) {
      dt[, `start.date.local` := as.Date(as.integer(`start.date.local`), origin = "1899-12-30")]
    } else {
      dt[, `start.date.local` := as.Date(as.integer(`start.date.local`), origin = "1970-01-01")]
    }
  }
  # Drop implausible dates (e.g. serial 0 -> 1899-12-30)
  dt[!is.na(`start.date.local`) & `start.date.local` < as.Date("2000-01-01"), `start.date.local` := as.Date(NA)]
  dt
}

#' Merge treatment metadata, BCS, weights, and optionally digital cage data.
#' @param meta Treatment metadata from load_treatment_metadata()
#' @param bcs BCS long from load_bcs_data()
#' @param weights Weights long from load_weight_data()
#' @param digital Optional data.table from load_digital_cage_data()
#' @return List with meta, bcs, weights, digital (if provided), and merged bcs/weights with treatment
merge_all_data <- function(meta = load_treatment_metadata(),
                           bcs = load_bcs_data(),
                           weights = load_weight_data(),
                           digital = NULL) {
  bcs_with_tx <- merge(bcs, meta[, c("animal_id", "treatment", "housing_id")], by = "animal_id", all.x = TRUE)
  weights_with_tx <- merge(weights, meta[, c("animal_id", "treatment", "housing_id")], by = "animal_id", all.x = TRUE)
  out <- list(
    meta = meta,
    bcs = bcs,
    weights = weights,
    bcs_with_treatment = bcs_with_tx,
    weights_with_treatment = weights_with_tx
  )
  if (!is.null(digital)) {
    out$digital <- digital
    # Digital uses animal.id; align with animal_id
    if (requireNamespace("data.table", quietly = TRUE)) {
      dig <- data.table::as.data.table(digital)
      meta_dt <- data.table::as.data.table(meta[, c("animal_id", "treatment", "housing_id")])
      data.table::setnames(meta_dt, "animal_id", "animal.id")
      out$digital_with_treatment <- merge(dig, meta_dt, by = "animal.id", all.x = TRUE)
    }
  }
  out
}

#' Verify treatment assignments match between Excel metadata and digital cage group.name.
#' @param meta From load_treatment_metadata()
#' @param digital From load_digital_cage_data() (optional; if NULL, only checks meta internal consistency)
#' @return List with ok (logical), message, and details (mismatches if any)
validate_animal_assignments <- function(meta = load_treatment_metadata(), digital = NULL) {
  n_meta <- nrow(meta)
  n_tx <- table(meta$treatment)
  ok <- TRUE
  msg <- character()
  details <- list(meta_n = n_meta, treatment_counts = n_tx)
  # Expect 58 animals
  if (n_meta != 58L) {
    ok <- FALSE
    msg <- c(msg, paste0("Expected 58 animals in metadata, found ", n_meta))
  }
  if (is.null(digital)) {
    return(list(ok = ok, message = msg, details = details))
  }
  # Digital: one row per animal per minute; get unique animal.id and group.name
  if (requireNamespace("data.table", quietly = TRUE)) {
    dig <- data.table::as.data.table(digital)
    animal_group <- unique(dig[, c("animal.id", "group.name")])
    # Map group.name to expected treatment: "Vehicle Uniform 3:0" -> Vehicle; others with CT-26 in name -> CT-26
    animal_group$digital_treatment <- ifelse(grepl("Vehicle", animal_group$group.name), "Vehicle", "CT-26")
    merged <- merge(meta[, c("animal_id", "treatment")], animal_group, by.x = "animal_id", by.y = "animal.id", all = TRUE)
    mismatches <- merged[merged$treatment != merged$digital_treatment & !is.na(merged$treatment) & !is.na(merged$digital_treatment), ]
    details$mismatches <- mismatches
    if (nrow(mismatches) > 0L) {
      ok <- FALSE
      msg <- c(msg, paste0(nrow(mismatches), " animal(s) have treatment mismatch between Excel and digital group.name"))
    }
  }
  list(ok = ok, message = msg, details = details)
}

#' Check group.name consistency per cage (all animals in same cage should have same group.name).
#' @param digital From load_digital_cage_data()
#' @return List with ok, message, and per-cage summary
validate_cage_groups <- function(digital) {
  if (is.null(digital)) return(list(ok = NA, message = "No digital data provided", details = list()))
  if (!requireNamespace("data.table", quietly = TRUE)) return(list(ok = NA, message = "data.table required", details = list()))
  dig <- data.table::as.data.table(digital)
  cage_grp <- unique(dig[, c("cage.name", "group.name")])
  n_groups_per_cage <- cage_grp[, .N, by = cage.name]
  bad <- n_groups_per_cage[N > 1L]
  ok <- nrow(bad) == 0L
  msg <- if (ok) "All cages have a single group.name." else paste0(nrow(bad), " cage(s) have multiple group names.")
  list(ok = ok, message = msg, details = list(cage_groups = cage_grp, inconsistent_cages = bad))
}

#' Summarize data quality: missing values, date ranges, animal counts.
#' @param merged List from merge_all_data()
#' @return List with summaries for BCS, weights, and optional digital
summarize_data_quality <- function(merged = merge_all_data()) {
  bcs <- merged$bcs_with_treatment
  weights <- merged$weights_with_treatment
  out <- list(
    bcs = list(
      n_animals = length(unique(bcs$animal_id)),
      n_records = nrow(bcs),
      date_range = range(bcs$date, na.rm = TRUE),
      n_missing_bcs = sum(is.na(bcs$bcs)),
      n_out_of_range = sum(bcs$bcs < BCS_VALID_RANGE[1] | bcs$bcs > BCS_VALID_RANGE[2], na.rm = TRUE)
    ),
    weights = list(
      n_animals = length(unique(weights$animal_id)),
      n_records = nrow(weights),
      date_range = range(weights$date, na.rm = TRUE),
      n_missing_weight = sum(is.na(weights$weight)),
      n_out_of_range = sum(weights$weight < WEIGHT_VALID_RANGE[1] | weights$weight > WEIGHT_VALID_RANGE[2], na.rm = TRUE)
    )
  )
  if (!is.null(merged$digital)) {
    dig <- merged$digital
    date_col <- if ("start.date.local" %in% names(dig)) "start.date.local" else "start"
    dr <- if (is.character(dig[[date_col]])) range(as.Date(dig[[date_col]]), na.rm = TRUE) else range(dig[[date_col]], na.rm = TRUE)
    out$digital <- list(
      n_animals = length(unique(dig$animal.id)),
      n_rows = nrow(dig),
      date_range = dr,
      cages = length(unique(dig$cage.name))
    )
  }
  out
}
