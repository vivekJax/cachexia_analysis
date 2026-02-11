# Cachexia Analysis Code (Phases 1–5)

## Layout

- **config.R** – Paths and parameters (Excel, CSV, BCS/weight thresholds).
- **R/data_loader.R** – Phase 1: load treatment metadata, BCS, weights, digital cage data; merge and validate.
- **R/bcs_analysis.R** – Phase 2: BCS trajectories, onset, group comparison, survival-style analysis.
- **R/weight_analysis.R** – Phase 2: weight trajectories, loss onset, group comparison, AUC.
- **R/digital_cage_analysis.R** – Phase 3: digital cage aggregation, trajectories, group comparison.
- **R/onset_detection.R** – Phase 4: manual/digital onset detection, compare onset times, lead time.
- **R/sensitivity_analysis.R** – Phase 4: effect sizes, power analysis, ROC, bootstrap CI.
- **R/reporting.R** – Phase 5: full pipeline, summarize results, export CSVs.
- **source_all.R** – Source config and all R modules (run from Cachexia root).
- **tests/** – Test scripts for Phases 1–5.
- **app.R** and **global.R** (project root) – Phase 5: interactive Shiny dashboard.

## Requirements

- R with packages: **readxl**, **data.table**
- Optional: **ggplot2** (plots), **survival** (BCS survival analysis)
- For Phase 5 dashboard: **shiny**, **shinydashboard** — install with `install.packages(c("shiny", "shinydashboard"))`

## Usage

From the **Cachexia** project root (parent of `AnalysisCode/`):

```r
# Load all analysis code
source("AnalysisCode/source_all.R")

# Phase 1: load and merge data
meta <- load_treatment_metadata()
bcs <- load_bcs_data()
weights <- load_weight_data()
merged <- merge_all_data(meta = meta, bcs = bcs, weights = weights)

# Check assignments and data quality
validate_animal_assignments(meta)
summarize_data_quality(merged)

# Phase 2: BCS
plot_bcs_trajectories(merged$bcs_with_treatment, by_treatment = TRUE)
plot_bcs_survival_curve(merged$bcs_with_treatment, threshold = 3)  # Kaplan-Meier: % survival, Vehicle=blue circles, CT-26=red squares
calculate_bcs_onset(merged$bcs_with_treatment, threshold = 3)
bcs_group_comparison(merged$bcs_with_treatment)

# Phase 2: Weights
plot_weight_trajectories(merged$weights_with_treatment, percent_change = TRUE)
calculate_weight_loss_onset(merged$weights_with_treatment, threshold_percent = 10)
weight_auc_analysis(merged$weights_with_treatment)

# Phase 4: Early detection comparison
manual_onset <- detect_manual_onset(merged$bcs_with_treatment, merged$weights_with_treatment)
daily_digital <- aggregate_digital_to_daily(digital, feature_col = "activity.animal.cm_s.min")
digital_onset <- detect_digital_onset(daily_digital, baseline_days = 3, n_consecutive = 2)
comp <- compare_onset_times(manual_onset, digital_onset, manual_method = "BCS")
calculate_lead_time(comp, bootstrap_n = 2000)
calculate_effect_sizes(weight_auc_df, value_cols = "weight_auc")
power_analysis_comparison(cohens_d = 0.8, power = 0.8)
roc_analysis(cachexia_status, digital_predictor, direction = "auto")
bootstrap_confidence_intervals(comp$lead_time_days, statistic = mean, n_bootstrap = 2000)
```

## Running tests

From the **Cachexia** project root:

```bash
Rscript AnalysisCode/tests/test_data_loader.R
Rscript AnalysisCode/tests/test_bcs_weight_analysis.R
Rscript AnalysisCode/tests/test_phase4.R
```

## Data paths

Paths in `config.R` point to:

- `Final Cachexia in CD2 F1 mice with CT-26.xlsx`
- `CCX_C1_animal_1min_2026-01-28T18_21_57Z.csv`

Set `CACHEXIA_ROOT` to the folder containing these files if you run from a different directory.

## Phase 5: Shiny dashboard

From the **Cachexia** project root, run the interactive dashboard:

```r
# In R: set working directory to Cachexia, then run the app
setwd("/path/to/Cachexia")
shiny::runApp()
```

Or from the project root in a terminal:

```bash
cd /path/to/Cachexia
R -e "shiny::runApp()"
```

The dashboard has five tabs: **Data Overview**, **Human Scoring** (BCS/weight trajectories and comparisons), **Digital Phenotyping** (feature selection and trajectories), **Early Detection** (onset tables, lead time, effect sizes), and **Export** (download CSVs). Use the sidebar to set digital data row limit and click "Load / refresh analysis" to run the full pipeline.


setwd('/Users/vkumar/Downloads/Cachexia')

shiny::runApp()
