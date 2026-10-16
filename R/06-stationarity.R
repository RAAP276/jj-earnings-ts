# =============================================================================
# Stationarity tests for time series.
#
# Three complementary tests:
#   - ADF       : H0 = unit root (non-stationary). Reject -> stationary.
#   - KPSS      : H0 = stationary. Reject -> non-stationary.
#   - Ljung-Box : H0 = no autocorrelation. Reject -> autocorrelated.
#
# ADF and KPSS have opposite null hypotheses, which makes them useful
# together: they should agree on the direction of the conclusion.
# =============================================================================


#' Augmented Dickey-Fuller test
#'
#' Wrapper around [tseries::adf.test()] with input validation.
#'
#' @param series A `ts` object.
#' @param ... Passed through to [tseries::adf.test()].
#' @return An `htest` object.
#' @export
test_adf <- function(series, ...) {
  if (!stats::is.ts(series)) {
    rlang::abort("`series` must be a `ts` object.",
                 class = "jjreport_error_not_ts")
  }
  tseries::adf.test(series, ...)
}


#' KPSS test
#'
#' Wrapper around [tseries::kpss.test()] with input validation.
#'
#' @param series A `ts` object.
#' @param null Character. Either `"Level"` (default) or `"Trend"`.
#' @param ... Passed through to [tseries::kpss.test()].
#' @return An `htest` object.
#' @export
test_kpss <- function(series, null = c("Level", "Trend"), ...) {
  if (!stats::is.ts(series)) {
    rlang::abort("`series` must be a `ts` object.",
                 class = "jjreport_error_not_ts")
  }
  null <- match.arg(null)
  tseries::kpss.test(series, null = null, ...)
}


#' Ljung-Box test for autocorrelation
#'
#' Wrapper around [stats::Box.test()] with `type = "Ljung-Box"`.
#'
#' @param x Numeric vector (typically residuals from a model).
#' @param lag Integer. Number of lags to test. Default 10.
#' @return An `htest` object.
#' @export
test_ljung_box <- function(x, lag = 10) {
  if (!is.numeric(x)) {
    rlang::abort("`x` must be a numeric vector.",
                 class = "jjreport_error_not_numeric")
  }
  stats::Box.test(x, lag = lag, type = "Ljung-Box")
}


#' Run ADF + KPSS on a series and return a tidy summary
#'
#' @param series A `ts` object.
#' @param kpss_null Character. `"Level"` (default) or `"Trend"`.
#' @return A tibble with columns `test`, `statistic`, `p_value`,
#'   `null_hypothesis`, and `conclusion`.
#' @export
summarize_stationarity <- function(series, kpss_null = "Level") {
  adf  <- test_adf(series)
  kpss <- test_kpss(series, null = kpss_null)
  
  tibble::tibble(
    test            = c("ADF", "KPSS"),
    statistic       = c(as.numeric(adf$statistic),
                        as.numeric(kpss$statistic)),
    p_value         = c(adf$p.value, kpss$p.value),
    null_hypothesis = c(
      "Unit root (non-stationary)",
      paste0(kpss_null, "-stationary")
    ),
    conclusion = c(
      ifelse(adf$p.value  < 0.05, "Stationary",     "Non-stationary"),
      ifelse(kpss$p.value < 0.05, "Non-stationary", "Stationary")
    )
  )
}