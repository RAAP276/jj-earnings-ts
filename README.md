# jj-earnings-ts

A reproducible time series analysis of the Johnson & Johnson quarterly
earnings per share series (1960-1980), using R, Quarto, and LaTeX.

---

## Overview

The Johnson & Johnson quarterly earnings series is a canonical dataset
in the time series literature, appearing as the first worked example in
Shumway and Stoffer's *Time Series Analysis and Its Applications*. This
repository contains a complete, reproducible analysis of the series,
organized as a two-stage pipeline:

1. **Analysis stage** (analysis.R) loads the data, fits three families
   of models (log-linear regression, SARIMA, ETS), runs stationarity and
   autocorrelation diagnostics, and writes all figures, tables, and
   numerical results to output/.

2. **Report stage** (report.qmd) reads the precomputed results and
   renders a Quarto document in HTML or PDF. It does not refit any
   model, so iteration on the report is fast.

The final report is available as report.pdf (compiled) and
report.html (viewable in any browser).

---

## What's in the report

The report covers:

- **Exploratory analysis.** Log transformation, STL decomposition,
  seasonal plots, quarterly boxplots, and ACF/PACF diagnostics.
- **Regression baseline.** A log-linear model with a centered time
  trend and quarterly dummy variables, including coefficient
  interpretation and residual analysis.
- **Stationarity testing.** ADF, KPSS, and Ljung-Box tests.
- **Alternative models.** SARIMA(2,0,0)(1,1,0)[4] with drift and
  Holt-Winters ETS(A,A,A), compared via AIC, BIC, and out-of-sample
  errors.
- **Forecast validation.** A single holdout split (1960-1975 into
  1976-1980) and rolling cross-validation, with RMSE, MAE, and MAPE
  reported for both models.
- **Discussion and limitations.** Honest account of what the analysis
  can and cannot conclude.

---

## Reproducing the analysis

### Prerequisites

- R version 4.1 or higher (for the native pipe)
- Quarto version 1.4 or higher
- A LaTeX distribution (TinyTeX, TeX Live, or MiKTeX) for PDF output

### Setup

Clone the repository:

    git clone git@github.com:RAAP276/jj-earnings-ts.git
    cd jj-earnings-ts

Open the project in RStudio, or install the required packages from R:

    install.packages(c(
      "broom", "dplyr", "forecast", "ggplot2", "here", "knitr",
      "quarto", "rlang", "sessioninfo", "tibble", "tseries"
    ))

### Run the analysis

From a shell:

    Rscript analysis.R

Or from an R session opened at the project root:

    source("analysis.R")

This generates all figures and tables in output/ and writes the key
results to output/results.rds.

### Render the report

From a shell:

    quarto render report.qmd --to html
    quarto render report.qmd --to pdf

The HTML output is self-contained. The PDF is compiled via LaTeX, and
the intermediate .tex file is preserved (keep-tex: true) in case you
want to embed the report in a larger LaTeX document.

---

## Project structure

    jj-earnings-ts/
    |-- R/                     modular functions, one responsibility each
    |   |-- 01-load-data.R     load_jj_series()
    |   |-- 02-fit-models.R    build_design(), fit_jj_models()
    |   |-- 03-plots.R         augment_jj_fit(), plot_*()
    |   |-- 04-tables.R        write_*_table() for LaTeX tables
    |   |-- 05-explore.R       STL, seasonal, boxplot, ACF
    |   |-- 06-stationarity.R  ADF, KPSS, Ljung-Box
    |   +-- 07-forecast.R      SARIMA, ETS, holdout, tsCV
    |-- analysis.R             orchestrator
    |-- report.qmd             Quarto document
    |-- report.pdf             compiled PDF
    |-- report.html            compiled HTML
    |-- report.tex             LaTeX source (keep-tex)
    |-- references.bib         bibliography
    |-- .Rprofile              project-level R config
    |-- .gitignore
    |-- jj-earnings-ts.Rproj
    +-- README.md

---

## Data

The series is datasets::JohnsonJohnson, shipped with base R. It
contains 84 quarterly observations of earnings per share, from
1960-Q1 to 1980-Q4. No external files are required.

To verify the series is loaded correctly:

    datasets::JohnsonJohnson
    # Time Series:
    # Start = 1960
    # End = 1980.75
    # Frequency = 4
    # [1] 0.71 0.63 0.85 0.44 ...

---

## Reproducibility notes

- R version: 4.4.1
- Quarto version: 1.4 or higher
- Package versions: pinned via renv (see renv.lock when present)
- Random seed: not required, all models are deterministic
- Compilation artifacts: output/ is gitignored. Running analysis.R
  regenerates every figure and table.

To restore the exact package environment:

    renv::restore()

---

## Limitations

- The sample is small: 84 observations over 21 years. Forecasts
  beyond 1980 should not be extrapolated without additional data.
- Model selection uses auto.arima and ets, which search by AICc
  within their model families but do not guarantee the globally
  best specification.
- No formal breakpoint analysis was performed.

---

## License

MIT. See the LICENSE file for details.

---

## Acknowledgments

The dataset and the original exercise come from Shumway, R. H., and
Stoffer, D. S., *Time Series Analysis and Its Applications: With R
Examples*. The analysis in this repository extends the original
exercise with exploratory decomposition, stationarity testing,
alternative models, and out-of-sample validation.