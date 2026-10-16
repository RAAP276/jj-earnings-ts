# =============================================================================
# Forecasting models: SARIMA, ETS, and validation.
#
# Provides:
#   - automatic fitting of SARIMA and ETS models
#   - a train/test split helper for holdout validation
#   - holdout accuracy (RMSE, MAE, MAPE on the test set)
#   - rolling one-step-ahead cross-validation via forecast::tsCV()
#   - an information-criteria comparison of two or more fits
# =============================================================================


#' Fit a SARIMA model automatically
#'
#' Wrapper around [forecast::auto.arima()] with input validation.
#' The model order (p,d,q)(P,D,Q)[s] is chosen by minimizing AICc.
#'
#' @param series A `ts` object with frequency > 1.
#' @param ... Passed through to [forecast::auto.arima()].
#' @return An object of class `Arima`.
#' @export
fit_sarima <- function(series, ...) {
  if (!stats::is.ts(series)) {
    rlang::abort("`series` must be a `ts` object.",
                 class = "jjreport_error_not_ts")
  }
  if (stats::frequency(series) <= 1) {
    rlang::abort("`series` must have frequency > 1.",
                 class = "jjreport_error_not_seasonal")
  }
  forecast::auto.arima(series, ...)
}


#' Fit an ETS model automatically
#'
#' Wrapper around [forecast::ets()] with input validation.
#' The ETS taxonomy (Error, Trend, Seasonal) is chosen by minimizing AICc.
#'
#' @param series A `ts` object with frequency > 1.
#' @param ... Passed through to [forecast::ets()].
#' @return An object of class `ets`.
#' @export
fit_ets <- function(series, ...) {
  if (!stats::is.ts(series)) {
    rlang::abort("`series` must be a `ts` object.",
                 class = "jjreport_error_not_ts")
  }
  if (stats::frequency(series) <= 1) {
    rlang::abort("`series` must have frequency > 1.",
                 class = "jjreport_error_not_seasonal")
  }
  forecast::ets(series, ...)
}


#' Split a `ts` into train and test subsets
#'
#' @param series A `ts` object.
#' @param n_train Integer. Number of observations assigned to the train set.
#' @return A list with `train`, `test`, and `split_time` (the time index at
#'   which the test set begins).
#' @export
split_train_test <- function(series, n_train) {
  if (!stats::is.ts(series)) {
    rlang::abort("`series` must be a `ts` object.",
                 class = "jjreport_error_not_ts")
  }
  n_total <- length(series)
  if (!is.numeric(n_train) || n_train < 1 || n_train >= n_total) {
    rlang::abort(
      message = sprintf("`n_train` must be between 1 and %d.", n_total - 1),
      class = "jjreport_error_bad_split"
    )
  }
  
  times <- stats::time(series)
  train <- stats::window(series, end   = times[n_train])
  test  <- stats::window(series, start = times[n_train + 1L])
  
  list(
    train      = train,
    test       = test,
    split_time = times[n_train + 1L]
  )
}


#' Holdout accuracy for a forecaster
#'
#' Fits the model on the train subset, forecasts the test subset, and
#' returns the accuracy table produced by [forecast::accuracy()].
#'
#' @param series A `ts` object.
#' @param n_train Integer. Number of observations in the train set.
#' @param fitter A function `function(series) -> fit`. Typically
#'   [fit_sarima()] or [fit_ets()].
#' @return A tibble with rows for training and test sets, columns
#'   `ME`, `RMSE`, `MAE`, `MPE`, `MAPE`, `MASE`, `ACF1`, `Theil's U`.
#' @export
holdout_accuracy <- function(series, n_train, fitter) {
  if (!is.function(fitter)) {
    rlang::abort("`fitter` must be a function.",
                 class = "jjreport_error_not_function")
  }
  
  split      <- split_train_test(series, n_train)
  fit        <- fitter(split$train)
  fc         <- forecast::forecast(fit, h = length(split$test))
  acc        <- forecast::accuracy(fc, split$test)
  
  tibble::as_tibble(acc, rownames = "set")
}


#' Rolling one-step-ahead cross-validation RMSE
#'
#' Uses [forecast::tsCV()] to fit the model at every time point and predict
#' one step ahead. Returns the RMSE across all folds.
#'
#' @param series A `ts` object.
#' @param fitter A function `function(y, h) -> forecast`. Must accept a
#'   series and a horizon. Example: `function(y, h)
#'   forecast::forecast(forecast::auto.arima(y), h = h)`.
#' @param h Integer. Forecast horizon for each fold. Default 1.
#' @return Numeric scalar with the cross-validated RMSE.
#' @export
tscv_rmse <- function(series, fitter, h = 1L) {
  if (!stats::is.ts(series)) {
    rlang::abort("`series` must be a `ts` object.",
                 class = "jjreport_error_not_ts")
  }
  if (!is.function(fitter)) {
    rlang::abort("`fitter` must be a function `(y, h)`.",
                 class = "jjreport_error_not_function")
  }
  
  errors <- forecast::tsCV(series, fitter, h = h)
  sqrt(mean(errors^2, na.rm = TRUE))
}


#' Compare two or more fitted models via AIC and BIC
#'
#' @param fits A named list of fitted objects with `AIC()` and `BIC()`
#'   methods (e.g. `Arima`, `ets`, `lm`).
#' @return A tibble with one row per model: `model`, `aic`, `bic`, `aicc`
#'   (when available), and `log_lik`.
#' @export
compare_information_criteria <- function(fits) {
  if (!is.list(fits) || is.null(names(fits))) {
    rlang::abort("`fits` must be a named list of fitted models.",
                 class = "jjreport_error_not_named_list")
  }
  
  rows <- lapply(names(fits), function(nm) {
    fit <- fits[[nm]]
    data.frame(
      model   = nm,
      log_lik = as.numeric(stats::logLik(fit)),
      aic     = stats::AIC(fit),
      bic     = stats::BIC(fit),
      stringsAsFactors = FALSE
    )
  })
  
  tibble::as_tibble(do.call(rbind, rows))
}