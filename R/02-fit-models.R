# =============================================================================
# Regression models for the J&J earnings series.
#
# Two specifications:
#   - no intercept: one level per quarter (matches the exercise statement)
#   - with intercept: Q1 acts as the reference level (for comparison)
#
# Both span the same column space, so their fitted values are identical.
# Only the parameterization differs.
# =============================================================================


#' Build the design frame for the J&J regression
#'
#' Constructs a tibble with the log-transformed earnings, the time trend
#' (centered at `center_year`), and the quarter factor with 4 levels.
#'
#' @param series A quarterly `ts` object (frequency = 4).
#' @param center_year Numeric scalar. Year used to center the trend.
#'   Defaults to 1970, following the original exercise.
#' @return A tibble with columns `log_earnings`, `trend`, and `quarter`.
#' @export
#'
#' @examples
#' jj <- load_jj_series()
#' design <- build_design(jj)
#' head(design)
build_design <- function(series, center_year = 1970) {
  # ---- Input validation ------------------------------------------------
  if (!stats::is.ts(series)) {
    rlang::abort(
      message = "`series` must be a `ts` object.",
      class = "jjreport_error_not_ts"
    )
  }
  if (stats::frequency(series) != 4) {
    rlang::abort(
      message = "`series` must be quarterly (frequency = 4).",
      class = "jjreport_error_not_quarterly"
    )
  }
  
  n_obs <- length(series)
  if (n_obs %% 4L != 0L) {
    rlang::abort(
      message = "Series length must be a multiple of 4.",
      class = "jjreport_error_bad_length"
    )
  }
  
  # ---- Build the design columns ----------------------------------------
  # `time(series)` returns the decimal time index. Subtracting `center_year`
  # centers the trend so that trend = 0 at the start of `center_year`.
  trend <- as.numeric(stats::time(series)) - center_year
  
  # Cycle 1:4 through every year. `times = n_obs / 4` means "repeat the
  # pattern 1,2,3,4 as many times as there are years".
  quarter <- factor(
    rep(seq_len(4L), times = n_obs / 4L),
    levels = seq_len(4L),
    labels = paste0("Q", seq_len(4L))
  )
  
  # ---- Assemble the tibble ---------------------------------------------
  tibble::tibble(
    log_earnings = as.numeric(log(series)),
    trend        = trend,
    quarter      = quarter
  )
}


#' Fit both J&J regression specifications
#'
#' @param design A tibble produced by [build_design()].
#' @return A named list with `no_intercept` and `with_intercept`.
#' @export
#'
#' @examples
#' jj <- load_jj_series()
#' fits <- fit_jj_models(build_design(jj))
#' names(fits)
fit_jj_models <- function(design) {
  # ---- Input validation ------------------------------------------------
  if (!is.data.frame(design)) {
    rlang::abort(
      message = "`design` must be a data frame or tibble.",
      class = "jjreport_error_not_df"
    )
  }
  
  required_cols <- c("log_earnings", "trend", "quarter")
  missing_cols  <- setdiff(required_cols, names(design))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      message = c(
        "`design` is missing required columns.",
        "x" = paste(missing_cols, collapse = ", ")
      ),
      class = "jjreport_error_missing_cols"
    )
  }
  
  # ---- Fit both specifications -----------------------------------------
  # `0 + trend + quarter`: no intercept, one coefficient per quarter.
  no_intercept <- stats::lm(
    log_earnings ~ 0 + trend + quarter,
    data      = design,
    na.action = NULL
  )
  
  # `1 + trend + quarter`: intercept + trend + Q2/Q3/Q4 dummies
  # (Q1 becomes the reference level).
  with_intercept <- stats::lm(
    log_earnings ~ 1 + trend + quarter,
    data      = design,
    na.action = NULL
  )
  
  list(
    no_intercept   = no_intercept,
    with_intercept = with_intercept
  )
}