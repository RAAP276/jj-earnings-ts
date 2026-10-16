# =============================================================================
# Orchestrator for the J&J time series analysis.
#
# Runs the full pipeline end-to-end:
#   load -> regression baseline -> plots -> LaTeX tables
#        -> exploratory (STL, seasonal, boxplot, ACF/PACF)
#        -> stationarity tests
#        -> forecasting (SARIMA, ETS, holdout, tsCV)
#
# Outputs:
#   output/*.png          figures for the report
#   output/*.tex          LaTeX tables ready for \input{}
#   output/results.rds    key objects consumed by report.qmd
#
# Usage:
#   Rscript analysis.R
# =============================================================================

suppressPackageStartupMessages({
  library(here)
})

# ---- Load modules ---------------------------------------------------------
source(here::here("R", "01-load-data.R"))
source(here::here("R", "02-fit-models.R"))
source(here::here("R", "03-plots.R"))
source(here::here("R", "04-tables.R"))
source(here::here("R", "05-explore.R"))
source(here::here("R", "06-stationarity.R"))
source(here::here("R", "07-forecast.R"))

# ---- Output directory -----------------------------------------------------
output_dir <- here::here("output")
dir.create(output_dir, showWarnings = FALSE)

# ---- 1. Load data ---------------------------------------------------------
message("== 1. Loading data ==")
series     <- load_jj_series()
log_series <- log(series)

# ---- 2. Regression baseline -----------------------------------------------
message("== 2. Fitting regression models ==")
design <- build_design(series, center_year = 1970)
fits   <- fit_jj_models(design)

# ---- 3. LaTeX tables ------------------------------------------------------
message("== 3. Writing LaTeX tables ==")
write_coefficient_table(
  fits$no_intercept,
  path    = file.path(output_dir, "coef_no_intercept.tex"),
  caption = "Coefficients of the no-intercept specification"
)
write_coefficient_table(
  fits$with_intercept,
  path    = file.path(output_dir, "coef_with_intercept.tex"),
  caption = "Coefficients of the with-intercept specification"
)
write_model_comparison_table(
  fits,
  path    = file.path(output_dir, "model_comparison.tex"),
  caption = "Comparison of the two regression specifications"
)

# ---- 4. Regression plots --------------------------------------------------
message("== 4. Generating regression plots ==")
aug <- augment_jj_fit(fits$no_intercept, series, "no_intercept")

ggplot2::ggsave(
  filename = file.path(output_dir, "observed_vs_fitted.png"),
  plot     = plot_observed_vs_fitted(aug),
  width = 8, height = 4.5, dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "residuals.png"),
  plot     = plot_residuals(aug),
  width = 8, height = 4.5, dpi = 150
)

# ---- 5. Exploratory analysis ----------------------------------------------
message("== 5. Exploratory analysis ==")
stl_obj <- compute_stl(log_series, s.window = "periodic", robust = TRUE)

ggplot2::ggsave(
  filename = file.path(output_dir, "stl.png"),
  plot     = plot_stl(stl_obj),
  width = 8, height = 8, dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "seasonal.png"),
  plot     = plot_seasonal(log_series),
  width = 8, height = 4.5, dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "quarter_boxplot.png"),
  plot     = plot_quarter_boxplot(log_series),
  width = 6, height = 4.5, dpi = 150
)

# ACF/PACF: ggtsdisplay returns a composed plot that needs png() + print()
png(file.path(output_dir, "acf_pacf.png"),
    width = 1000, height = 1500, res = 120)
old_par <- par(mfrow = c(3, 1))
plot.ts(log_series, main = "Series", ylab = "log(Earnings)")
stats::acf(log_series, main = "ACF", lag.max = 30)
stats::pacf(log_series, main = "PACF", lag.max = 30)
par(old_par)
dev.off()

# ---- 6. Stationarity tests ------------------------------------------------
message("== 6. Stationarity tests ==")
stationarity_original <- summarize_stationarity(log_series)
stationarity_diff     <- summarize_stationarity(diff(log_series))

print(stationarity_original)
print(stationarity_diff)

# ---- 7. Forecasting -------------------------------------------------------
message("== 7. Forecasting ==")
fit_sarima_full <- fit_sarima(log_series)
fit_ets_full    <- fit_ets(log_series)

comparison_ic <- compare_information_criteria(list(
  SARIMA = fit_sarima_full,
  ETS    = fit_ets_full
))

holdout_sarima <- holdout_accuracy(log_series, n_train = 64, fitter = fit_sarima)
holdout_ets    <- holdout_accuracy(log_series, n_train = 64, fitter = fit_ets)

f_sarima <- function(y, h) forecast::forecast(forecast::auto.arima(y), h = h)
f_ets    <- function(y, h) forecast::forecast(forecast::ets(y), h = h)

tscv_sarima <- tscv_rmse(log_series, fitter = f_sarima)
tscv_ets    <- tscv_rmse(log_series, fitter = f_ets)

print(comparison_ic)
print(holdout_sarima)
print(holdout_ets)
message("tsCV RMSE (SARIMA): ", round(tscv_sarima, 4))
message("tsCV RMSE (ETS)   : ", round(tscv_ets, 4))

# ---- 8. Save results for the report --------------------------------------
message("== 8. Saving results for the report ==")
results <- list(
  series             = series,
  log_series         = log_series,
  design             = design,
  fits               = fits,
  augmented          = aug,
  stl                = stl_obj,
  stationarity_orig  = stationarity_original,
  stationarity_diff  = stationarity_diff,
  fit_sarima_full    = fit_sarima_full,
  fit_ets_full       = fit_ets_full,
  comparison_ic      = comparison_ic,
  holdout_sarima     = holdout_sarima,
  holdout_ets        = holdout_ets,
  tscv_sarima        = tscv_sarima,
  tscv_ets           = tscv_ets
)

saveRDS(results, file.path(output_dir, "results.rds"))

message("Analysis complete. Outputs written to: ", output_dir)