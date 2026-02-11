# Cachexia Analysis

Interactive R/Shiny tool for analyzing cachexia study data: digital cage phenotyping, human scoring (BCS, body weight), and early-detection comparison.

## Data

- **Excel** (`Final Cachexia in CD2 F1 mice with CT-26.xlsx`): treatment assignments, BCS, and weights — included in the repo.
- **Digital cage CSV** (`CCX_C1_animal_1min_*.csv`): 1-minute phenotype data (~514 MB) is **not** in the repo (GitHub file size limit). Place the file in the project root to run the Digital Phenotyping tab and full pipeline. Path is set in `AnalysisCode/config.R`.

## Run the app

From the project root in R:

```r
shiny::runApp()
```

Or: `Rscript -e "shiny::runApp()"`

See [AnalysisCode/README.md](AnalysisCode/README.md) for loading analysis code and running tests.

## Push to GitHub

After creating a new repository on [github.com/new](https://github.com/new) (do not add README, .gitignore, or license), run:

```bash
git remote add origin https://github.com/YOUR_USERNAME/YOUR_REPO_NAME.git
git branch -M main
git push -u origin main
```

Replace `YOUR_USERNAME` and `YOUR_REPO_NAME` with your GitHub username and repo name.
