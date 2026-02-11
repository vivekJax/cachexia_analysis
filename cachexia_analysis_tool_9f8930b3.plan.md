---
name: Cachexia Analysis Tool
overview: Build an interactive R-based analysis tool for comparing digital cage phenotyping vs. human scoring methods in a CT-26 cachexia mouse model (58 mice, 40 CT-26 / 18 Vehicle controls, 30 days of 1-minute resolution data).
todos:
  - id: phase1-loader
    content: "Phase 1: Build data loader module with treatment metadata extraction, BCS/weight reshaping, efficient CSV loading, and validation functions"
    status: pending
  - id: phase2-manual
    content: "Phase 2: Build human scoring analysis module for BCS trajectories, weight analysis, and statistical comparisons"
    status: pending
  - id: phase3-digital
    content: "Phase 3: Build digital cage analysis module with aggregation, feature engineering, and visualization functions"
    status: pending
  - id: phase4-detection
    content: "Phase 4: Build early detection comparison module with onset detection, sensitivity analysis, and power calculations"
    status: pending
  - id: phase5-shiny
    content: "Phase 5: Build interactive Shiny dashboard integrating all modules with data export capabilities"
    status: pending
isProject: false
---

# Cachexia Digital Phenotyping Analysis Tool

## Data Overview

| Source | Size | Format | Key Fields |

|--------|------|--------|------------|

**Digital Cage Data** (`CCX_C1_animal_1min_*.csv`):

- 514MB, 2.1M rows, 25 columns
- 1-minute bins from Oct 15 - Nov 14, 2025
- Phenotypes: activity (inactive/active/locomotion/climbing), feeding, drinking, respiratory rate, social distance, inferred sleeping
- Per-animal tracking with `animal.id`, `cage.name`, `group.name`

**Manual Scoring** (`Final Cachexia in CD2 F1 mice with CT-26.xlsx`):

- 5 sheets: BCS, Weights, Measurements, Notes, Takedown Notes
- Wide format (animals as columns, dates as rows)
- 58 animals: 40 CT-26 treated, 18 Vehicle controls

**Study Design:**

- Cage groupings: "CT-26 Uniform 3:0", "CT-26 Mixed 2:1", "CT-26 Mixed 1:2", "CT-26 Uniform 2:0", "Vehicle Uniform 3:0"
- Mixed cages test social/environmental effects of sick cage-mates on controls

---

## Phase 1: Data Loader Module

### 1.1 Core Data Loader (`R/data_loader.R`)

```r
# Key functions to implement:
load_treatment_metadata()    # Extract treatment assignments from Excel
load_bcs_data()              # Load & reshape BCS sheet to long format
load_weight_data()           # Load & reshape Weights sheet to long format  
load_digital_cage_data()     # Efficient loading with data.table::fread()
merge_all_data()             # Join digital + manual + metadata
```

**Implementation notes:**

- Use `data.table::fread()` for the 514MB CSV (fast, memory-efficient)
- Excel data needs transposition (currently animals=columns, dates=rows)
- Convert Excel serial dates to R Date objects
- Join key: `animal.id` across all datasets

### 1.2 Validation Functions

```r
validate_animal_assignments()  # Verify treatment matches between Excel & CSV
validate_cage_groups()         # Check group.name consistency
summarize_data_quality()       # Missing values, date ranges, animal counts
```

### 1.3 Tests for Phase 1

- [ ] All 58 animal IDs present in both digital and manual data
- [ ] Treatment assignments (CT-26/Vehicle) match between Excel metadata and CSV `group.name`
- [ ] No duplicate timestamps per animal in digital data
- [ ] BCS values in valid range (1-5 scale)
- [ ] Weight values plausible (15-35g for mice)
- [ ] Date ranges align between digital and manual observations

---

## Phase 2: Human Scoring Analysis Module

### 2.1 BCS Analysis (`R/bcs_analysis.R`)

```r
plot_bcs_trajectories()      # Individual + group mean trajectories
calculate_bcs_onset()        # Time to first BCS < 3 (cachexia threshold)
bcs_group_comparison()       # CT-26 vs Vehicle statistical comparison
bcs_survival_analysis()      # Time-to-event for BCS decline
```

### 2.2 Body Weight Analysis (`R/weight_analysis.R`)

```r
plot_weight_trajectories()           # Raw + percent change from baseline
calculate_weight_loss_onset()        # Time to X% weight loss threshold
weight_group_comparison()            # Mixed-effects model for repeated measures
weight_auc_analysis()                # Area under curve for weight loss
```

### 2.3 Tests for Phase 2

- [ ] BCS trajectory plots show expected divergence (CT-26 declining)
- [ ] Weight loss onset correctly identifies first significant decline
- [ ] Statistical tests appropriately handle repeated measures
- [ ] Mixed cage effects can be analyzed (Vehicle mice in mixed vs uniform cages)

---

## Phase 3: Digital Cage Analysis Module

### 3.1 Data Aggregation (`R/digital_aggregation.R`)

```r
aggregate_to_hourly()        # 1-min -> hourly summaries
aggregate_to_daily()         # Daily summaries with light/dark separation
calculate_circadian_metrics() # Activity amplitude, phase, rhythm strength
```

### 3.2 Feature Engineering (`R/digital_features.R`)

```r
calculate_activity_features()    # Total activity, locomotion ratio, rest bouts
calculate_feeding_features()     # Feeding duration, bout frequency, meal size
calculate_respiratory_features() # Mean, variance, trends in breathing rate
calculate_social_features()      # Social distance patterns (mixed cages)
```

### 3.3 Visualization (`R/digital_plots.R`)

```r
plot_actogram()                  # Double-plotted actograms
plot_feature_heatmap()           # Animals x days heatmap
plot_digital_trajectories()      # Longitudinal feature trends
```

### 3.4 Tests for Phase 3

- [ ] Aggregation preserves data integrity (sum of minutes = daily total)
- [ ] Circadian metrics show expected light/dark differences
- [ ] Feature calculations handle NA values appropriately
- [ ] Actograms display correct 24h structure

---

## Phase 4: Early Detection Comparison

### 4.1 Onset Detection (`R/onset_detection.R`)

```r
detect_digital_onset()       # Change-point detection on digital features
detect_manual_onset()        # Threshold-based onset for BCS/weight
compare_onset_times()        # Paired comparison: digital vs manual
calculate_lead_time()        # How many days earlier does digital detect?
```

**Methods to consider:**

- Change-point detection (e.g., `changepoint` package)
- Cumulative sum (CUSUM) control charts
- Moving average crossovers
- Z-score deviation from baseline

### 4.2 Sensitivity Analysis (`R/sensitivity_analysis.R`)

```r
calculate_effect_sizes()         # Cohen's d for digital vs manual features
power_analysis_comparison()      # Sample size needed to detect effect
roc_analysis()                   # ROC curves for digital predictors of cachexia
bootstrap_confidence_intervals() # CI for onset time differences
```

### 4.3 Tests for Phase 4

- [ ] Onset detection algorithms identify known disease progression
- [ ] Lead time calculations are positive (digital earlier than manual)
- [ ] Power analysis shows required N for each method
- [ ] ROC analysis identifies best digital predictors

---

## Phase 5: Interactive Shiny Dashboard

### 5.1 Dashboard Structure (`app.R`)

```
Cachexia Analysis Dashboard
├── Tab 1: Data Overview
│   ├── Study design summary
│   ├── Animal assignment verification
│   └── Data quality report
├── Tab 2: Human Scoring
│   ├── BCS trajectories (selectable animals/groups)
│   ├── Weight trajectories
│   └── Statistical comparisons
├── Tab 3: Digital Phenotyping
│   ├── Feature selection dropdown
│   ├── Actograms
│   ├── Daily/hourly aggregations
│   └── Circadian analysis
├── Tab 4: Early Detection
│   ├── Onset comparison plot
│   ├── Lead time distribution
│   └── Sensitivity/power analysis
└── Tab 5: Export
    ├── Download processed data
    └── Generate analysis report
```

### 5.2 Tests for Phase 5

- [ ] Dashboard loads without errors
- [ ] All interactive filters update plots correctly
- [ ] Large data renders without significant lag (< 3 sec)
- [ ] Export functions produce valid files

---

## Technical Architecture

```
Cachexia/
├── R/
│   ├── data_loader.R          # Phase 1
│   ├── bcs_analysis.R         # Phase 2
│   ├── weight_analysis.R      # Phase 2
│   ├── digital_aggregation.R  # Phase 3
│   ├── digital_features.R     # Phase 3
│   ├── digital_plots.R        # Phase 3
│   ├── onset_detection.R      # Phase 4
│   └── sensitivity_analysis.R # Phase 4
├── tests/
│   ├── test_data_loader.R
│   ├── test_bcs_analysis.R
│   ├── test_digital_analysis.R
│   └── test_onset_detection.R
├── app.R                      # Shiny dashboard (Phase 5)
├── config.R                   # File paths, parameters
└── DESCRIPTION                # Package dependencies
```

**R Package Dependencies:**

- `data.table` - Fast CSV reading (already installed)
- `readxl` - Excel reading (already installed)
- `ggplot2` - Visualization
- `shiny` + `shinydashboard` - Interactive UI
- `lme4` or `nlme` - Mixed-effects models
- `survival` - Time-to-event analysis
- `changepoint` - Change-point detection
- `pROC` - ROC analysis

---

## Performance Considerations

The 514MB CSV (2.1M rows) is manageable with `data.table::fread()`, which typically reads this in ~10-15 seconds. For interactive use:

1. **Pre-aggregate on load**: Create daily summaries cached to RDS
2. **Lazy loading**: Only load detailed minute-level data when needed
3. **Data caching**: Use `reactiveFileReader` in Shiny for efficient updates

If performance becomes an issue, fallback options:

- Convert CSV to Parquet format (install `arrow` package)
- Pre-compute features and store as smaller summary files
- Filter to specific date ranges or animals before loading