# =============================================================================
# Plots.
#
# Two figures for the J&J analysis:
#   - observed vs fitted values over time
#   - residuals over time
#
# Both are built from a single tibble produced by `augment_jj_fit()`.
# =============================================================================


#' Augment an `lm` fit with the original series for plotting
#'
#' Produces a long tibble with the time index, observed values, fitted
#' values, and residuals. This is the shape `ggplot2` needs.
#'
#' @param fit An `lm` object fitted on `log(series)`.
#' @param series The quarterly `ts` used to fit the model.
#' @param model_label Character. Label for the model (used as a grouping
#'   variable if multiple models are combined later).
#' @return A tibble with columns `time`, `observed`, `fitted`, `residual`,
#'   `model`.
#' @export
#'
#' @examples
#' jj <- load_jj_series()
#' fits <- fit_jj_models(build_design(jj))
#' aug <- augment_jj_fit(fits$no_intercept, jj, "no_intercept")
#' head(aug)
augment_jj_fit <- function(fit, series, model_label) {
  tibble::tibble(
    time     = as.numeric(stats::time(series)),
    observed = as.numeric(log(series)),
    fitted   = as.numeric(stats::fitted(fit)),
    residual = as.numeric(stats::residuals(fit)),
    model    = model_label
  )
}


#' Plot observed vs fitted values over time
#'
#' @param augmented Output of [augment_jj_fit()].
#' @return A `ggplot` object.
#' @export
plot_observed_vs_fitted <- function(augmented) {
  ggplot2::ggplot(augmented, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = observed, colour = "Observed")) +
    ggplot2::geom_line(ggplot2::aes(y = fitted, colour = "Fitted")) +
    ggplot2::scale_colour_manual(
      values = c(Observed = "black", Fitted = "red"),
      name   = NULL
    ) +
    ggplot2::labs(
      title = "J&J quarterly earnings: observed vs fitted",
      x     = "Year",
      y     = "log(Earnings)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}


#' Plot residuals over time
#'
#' @param augmented Output of [augment_jj_fit()].
#' @return A `ggplot` object.
#' @export
plot_residuals <- function(augmented) {
  ggplot2::ggplot(augmented, ggplot2::aes(x = time, y = residual)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = "J&J regression residuals over time",
      x     = "Year",
      y     = "log(Earnings) - fitted"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}