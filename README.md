# Cachexia Analysis

Interactive R/Shiny dashboard for analyzing cachexia study data: human scoring (BCS, body weight), digital cage phenotyping, and early-detection comparison (manual vs digital onset).

---

## Requirements

- **R** (4.x recommended)
- **R packages:** `shiny`, `shinydashboard`, `readxl`, `data.table`
- Optional (for plots and survival): `ggplot2`, `survival`

Install from CRAN if needed:

```r
install.packages(c("shiny", "shinydashboard", "readxl", "data.table"))
```

---

## Data

| File | Description | In repo? |
|------|-------------|----------|
| `Final Cachexia in CD2 F1 mice with CT-26.xlsx` | Treatment assignments, BCS, weights (manual scoring) | Yes |
| `CCX_C1_animal_1min_*.csv` | Digital cage 1-minute phenotype data (~514 MB) | No (size limit) |

Place both files in the **project root** (the folder containing `app.R`). The Excel file is in the repo; the digital cage CSV must be copied there (e.g. from your data source). Paths are set in `AnalysisCode/config.R` if you need to use a different directory.

---

## How to run the app

1. Open R (or RStudio).
2. Set the working directory to the project root (the folder that contains `app.R` and `AnalysisCode/`):

   ```r
   setwd("/path/to/Cachexia")
   ```

3. Start the app:

   ```r
   shiny::runApp()
   ```

   Or from a terminal (from the project root):

   ```bash
   Rscript -e "shiny::runApp()"
   ```

4. A browser window opens (e.g. http://127.0.0.1:XXXX). If not, open that URL manually.

**Restarting after code changes:** Stop the app (Esc or Stop button), run `shiny::runApp()` again. You do **not** need to restart R; the app re-sources the analysis code on each start.

---

## Sidebar: Load / refresh analysis

- **Load full digital data (all rows)**  
  Check this to load the entire digital cage CSV (all ~2.1M rows). Leave unchecked to use a sample for faster runs.
- **Digital data rows (when not full)**  
  Slider (10,000–200,000). Used only when “Load full digital data” is unchecked. Larger values = more data and longer load time.
- **Load / refresh analysis**  
  Click to (re)run the full pipeline: load data, merge, run BCS/weight/digital analyses, onset detection, and effect sizes.  
  - On first open, the app runs the pipeline once with the default row limit (50,000).  
  - Change the slider or checkbox, then click **Load / refresh analysis** to update all tabs that use digital data.

---

## Tab 1: Data Overview

- **Study design summary**  
  Table of animal counts by treatment (CT-26 vs Vehicle). 58 animals total; cage groupings (e.g. CT-26 Uniform 3:0, Mixed 2:1) are described in the text.
- **Animal assignment verification**  
  Checks that treatment in the Excel file matches the digital cage `group.name` (when digital data is loaded). Shows OK/fail and treatment counts.
- **Data quality report**  
  Summaries for BCS, weights, and (if loaded) digital data: number of animals, records, date ranges, missing and out-of-range values.

**No pipeline required:** This tab uses only the Excel data and loads immediately.

---

## Tab 2: Human Scoring

- **BCS trajectories**  
  Line plot of Body Condition Score over time by animal, colored by treatment (CT-26 red, Vehicle blue), with group mean lines. BCS &lt; 3 is a common cachexia threshold (optional reference line).
- **Weight trajectories (% change from baseline)**  
  Weight over time as percent change from each animal’s first measurement. Colored by treatment with group means.
- **BCS survival (time to BCS &lt; 3)**  
  Kaplan–Meier curve: time until first BCS &lt; 3 (event). Vehicle = blue circles, CT-26 = red squares. Y-axis = % “survival” (no event yet).
- **BCS group comparison**  
  Summary table (n, mean, SD per treatment) and statistical test (t-test or Wilcoxon) for BCS (e.g. at last observation). Includes Cohen’s d when applicable.
- **Weight group comparison**  
  Same for weight (or % change): summary table and test.

**Pipeline:** BCS/weight plots use Excel-only data. The comparison tables and tests use the **pipeline** (run at least once via **Load / refresh analysis**).

---

## Tab 3: Digital Phenotyping

- **Metric**  
  Dropdown of digital cage metrics (e.g. activity-inactive.animal.percent.min, activity.animal.cm_s.min). Populated after the pipeline runs with digital data.
- **Digital trajectories plot**  
  Daily-averaged metric over time per animal, by treatment, with group mean lines. X-axis is date (ensure digital CSV is present and pipeline has been run).
- **Digital metric group comparison**  
  Summary table (n, mean, SD) and test (CT-26 vs Vehicle) for the selected metric (per-animal mean). Shows Cohen’s d.

**Pipeline and data:** Requires the digital cage CSV in the project root and at least one **Load / refresh analysis** run. Choose “Load full digital data” or a higher row count for more complete trajectories.

---

## Tab 4: Early Detection

- **Manual onset (BCS & weight)**  
  Table of first “event” per animal: BCS &lt; 3 and/or weight loss ≥ 10% from baseline. Columns include animal_id, method (BCS/weight), onset_date, days_to_onset, treatment.
- **Digital onset**  
  Table of first sustained drop in the digital feature (from baseline) per animal. Empty if digital data wasn’t loaded or no events detected.
- **Lead time (manual BCS onset vs digital)**  
  For animals with both manual (BCS) and digital onset: difference in days (positive = digital detected earlier). Summary (mean, median, bootstrap CI) is printed below the table.
- **Effect sizes (last BCS, weight AUC)**  
  Cohen’s d for last BCS and for weight AUC (area under %-change curve), comparing CT-26 vs Vehicle.

**Pipeline:** All content on this tab comes from the pipeline. Run **Load / refresh analysis** (with digital data if you want digital onset and lead time).

---

## Tab 5: Export

Download key pipeline outputs as CSV:

- **BCS onset** – First date BCS &lt; threshold per animal.
- **Weight onset** – First date weight loss ≥ threshold % per animal.
- **Weight AUC** – Per-animal area under the weight %-change curve.
- **Effect sizes** – Table of effect sizes (e.g. last BCS, weight AUC).
- **Manual onset (BCS + weight)** – Combined manual onset table.
- **Lead time comparison** – Manual vs digital onset and lead time per animal.

Run **Load / refresh analysis** at least once before exporting so the downloads contain data.

---

## Suggested workflow

1. **First run**  
   Start the app. Data Overview and Human Scoring (plots) work immediately from Excel. Pipeline runs once with default digital sample (50k rows) if the CSV is present.
2. **Check data**  
   Use **Data Overview** to confirm study design and animal assignments; fix paths or data if needed.
3. **Human scoring**  
   Use **Human Scoring** for BCS and weight trajectories, survival curve, and group comparisons.
4. **Digital phenotyping**  
   If the digital CSV is in the project root: use the sidebar to choose “Load full digital data” or a larger sample, click **Load / refresh analysis**, then open **Digital Phenotyping** and pick a metric.
5. **Early detection**  
   Open **Early Detection** to compare manual vs digital onset and lead time; use **Export** to download tables.

---

## Running analysis code without the app

For scripts or custom analyses, load the analysis code from the project root:

```r
setwd("/path/to/Cachexia")
source("AnalysisCode/source_all.R")
```

Then call the functions (e.g. `load_treatment_metadata()`, `merge_all_data()`, `plot_bcs_trajectories()`). See [AnalysisCode/README.md](AnalysisCode/README.md) for function list and tests.

---

## Push to GitHub

After creating a new repository on [github.com/new](https://github.com/new) (do not add README, .gitignore, or license), run:

```bash
git remote add origin https://github.com/YOUR_USERNAME/YOUR_REPO_NAME.git
git branch -M main
git push -u origin main
```

Replace `YOUR_USERNAME` and `YOUR_REPO_NAME` with your GitHub username and repo name.
