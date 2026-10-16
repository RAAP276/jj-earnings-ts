# =============================================================================
# Exploratory analysis for the J&J series.
#
# Provides:
#   - a helper to convert a `ts` into a tibble
#   - STL decomposition and its plot
#   - seasonal plot (one line per year)
#   - quarterly boxplot
#   - ACF/PACF display via forecast::ggtsdisplay
# =============================================================================


#' Convert a `ts` object to a tibble with year and quarter columns
#'
#' @param series A `ts` object.
#' @param value_name Character. Name of the value column.
#' @return A tibble with columns `time`, `year`, `quarter`, and the value
#'   column (named `value_name`).
#' @export
ts_to_tibble <- function(series, value_name = "value") {
  if (!stats::is.ts(series)) {
    rlang::abort(
      message = "`series` must be a `ts` object.",
      class = "jjreport_error_not_ts"
    )
  }
  
  t_num <- as.numeric(stats::time(series))
  out <- tibble::tibble(
    time    = t_num,
    year    = as.integer(floor(t_num)),
    quarter = as.integer(stats::cycle(series)),
    value   = as.numeric(series)
  )
  names(out)[names(out) == "value"] <- value_name
  out
}


#' Compute the STL decomposition of a `ts`
#'
#' @param series A `ts` object with frequency > 1.
#' @param s.window Either `"periodic"` or a numeric window length for the
#'   seasonal smoother.
#' @param robust Logical. Use robust (outlier-resistant) fitting.
#' @return An `stl` object.
#' @export
compute_stl <- function(series,
                        s.window = "periodic",
                        robust   = TRUE) {
  if (!stats::is.ts(series)) {
    rlang::abort("`series` must be a `ts` object.",
                 class = "jjreport_error_not_ts")
  }
  if (stats::frequency(series) <= 1) {
    rlang::abort("`series` must have frequency > 1.",
                 class = "jjreport_error_not_seasonal")
  }
  stats::stl(series, s.window = s.window, robust = robust)
}


#' Plot the STL decomposition as a 4-panel figure
#'
#' Panels: observed series, trend, seasonal component, remainder.
#'
#' @param stl_obj An `stl` object from [compute_stl()].
#' @return A `ggplot` object.
#' @export
plot_stl <- function(stl_obj) {
  ts_mat <- stl_obj$time.series
  times  <- as.numeric(stats::time(ts_mat))
  
  long <- data.frame(
    time      = rep(times, times = 4L),
    component = rep(c("observed", "trend", "seasonal", "remainder"),
                    each = length(times)),
    value     = c(
      as.numeric(rowSums(ts_mat)),
      as.numeric(ts_mat[, "trend"]),
      as.numeric(ts_mat[, "seasonal"]),
      as.numeric(ts_mat[, "remainder"])
    )
  )
  long$component <- factor(
    long$component,
    levels = c("observed", "trend", "seasonal", "remainder")
  )
  
  ggplot2::ggplot(long, ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ component, ncol = 1, scales = "free_y") +
    ggplot2::labs(x = "Year", y = NULL,
                  title = "STL decomposition") +
    ggplot2::theme_minimal(base_size = 11)
}


#' Seasonal plot: one line per year, x-axis = quarter
#'
#' @param series A `ts` object with frequency 4.
#' @return A `ggplot` object.
#' @export
plot_seasonal <- function(series) {
  df <- ts_to_tibble(series)
  
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = quarter, y = value, group = year, colour = year)
  ) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = 1:4,
                                labels = paste0("Q", 1:4)) +
    ggplot2::scale_colour_gradient(low = "grey60", high = "red",
                                   name = "Year") +
    ggplot2::labs(x = "Quarter", y = "log(Earnings)",
                  title = "Seasonal plot") +
    ggplot2::theme_minimal(base_size = 11)
}


#' Boxplot of values by quarter
#'
#' @param series A `ts` object with frequency 4.
#' @return A `ggplot` object.
#' @export
plot_quarter_boxplot <- function(series) {
  df <- ts_to_tibble(series)
  df$quarter <- factor(df$quarter, labels = paste0("Q", 1:4))
  
  ggplot2::ggplot(df, ggplot2::aes(x = quarter, y = value)) +
    ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.5) +
    ggplot2::labs(x = "Quarter", y = "log(Earnings)",
                  title = "Distribution by quarter") +
    ggplot2::theme_minimal(base_size = 11)
}


#' ACF and PACF display via forecast::ggtsdisplay
#'
#' Wrapper that returns the composed ggplot object produced by
#' [forecast::ggtsdisplay()]. Saving to disk requires `ggplot2::ggsave()`
#' with `plot =` explicitly (the object is a composed `patchwork`).
#'
#' @param series A `ts` object.
#' @param ... Passed through to [forecast::ggtsdisplay()].
#' @return A `patchwork`/`ggplot` object.
#' @export
plot_acf_pacf <- function(series, ...) {
  forecast::ggtsdisplay(series, ...)
}