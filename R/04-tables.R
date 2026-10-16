# =============================================================================
# LaTeX table generation.
#
# Produces standalone `.tex` files from fitted `lm` objects, ready to be
# \input{} into a master LaTeX document. Uses `broom` to tidy model output
# and `knitr::kable` to render LaTeX with booktabs rules.
# =============================================================================


#' Write a coefficient table to a `.tex` file
#'
#' Extracts the coefficient table from an `lm` fit via [broom::tidy()] and
#' writes it as a LaTeX table using [knitr::kable()] with booktabs rules.
#'
#' @param fit An `lm` object.
#' @param path Character. Output path for the `.tex` file.
#' @param caption Character. Table caption (passed to LaTeX).
#' @param label Character. LaTeX label for cross-referencing, without the
#'   `tab:` prefix.
#' @param digits Integer. Number of decimal digits for numeric columns.
#' @return Invisibly returns `path`.
#' @export
#'
#' @examples
#' \dontrun{
#' jj     <- load_jj_series()
#' fits   <- fit_jj_models(build_design(jj))
#' write_coefficient_table(
#'   fits$no_intercept,
#'   path    = "output/coef_no_intercept.tex",
#'   caption = "Coefficients of the no-intercept specification"
#' )
#' }
write_coefficient_table <- function(fit,
                                    path,
                                    caption,
                                    label  = "coef",
                                    digits = 4) {
  # ---- Input validation ------------------------------------------------
  if (!inherits(fit, "lm")) {
    rlang::abort(
      message = "`fit` must be an `lm` object.",
      class = "jjreport_error_not_lm"
    )
  }
  
  # ---- Tidy the model into a coefficient tibble ------------------------
  tidy_fit <- broom::tidy(fit)
  
  # ---- Render as LaTeX and write to disk -------------------------------
  tex <- knitr::kable(
    tidy_fit,
    format   = "latex",
    booktabs = TRUE,
    digits   = digits,
    caption  = caption,
    label    = label
  )
  
  writeLines(as.character(tex), con = path)
  invisible(path)
}


#' Write a model-comparison table to a `.tex` file
#'
#' Builds a one-row summary per model using [broom::glance()], then renders
#' the comparison as a LaTeX table.
#'
#' @param fits Named list of `lm` objects (e.g. output of [fit_jj_models()]).
#' @param path Character. Output path for the `.tex` file.
#' @param caption Character. Table caption (passed to LaTeX).
#' @param label Character. LaTeX label, without the `tab:` prefix.
#' @param digits Integer. Number of decimal digits for numeric columns.
#' @return Invisibly returns `path`.
#' @export
#'
#' @examples
#' \dontrun{
#' jj   <- load_jj_series()
#' fits <- fit_jj_models(build_design(jj))
#' write_model_comparison_table(
#'   fits,
#'   path    = "output/model_comparison.tex",
#'   caption = "Comparison of the two specifications"
#' )
#' }
write_model_comparison_table <- function(fits,
                                         path,
                                         caption,
                                         label  = "comparison",
                                         digits = 3) {
  # ---- Input validation ------------------------------------------------
  if (!is.list(fits) || is.null(names(fits))) {
    rlang::abort(
      message = "`fits` must be a named list of `lm` objects.",
      class = "jjreport_error_not_named_list"
    )
  }
  
  # ---- Build one row per model -----------------------------------------
  rows <- lapply(names(fits), function(nm) {
    fit <- fits[[nm]]
    g   <- broom::glance(fit)
    data.frame(
      model     = nm,
      n_coef    = length(stats::coef(fit)),
      log_lik   = g$logLik,
      aic       = g$AIC,
      bic       = g$BIC,
      r_squared = g$r.squared,
      stringsAsFactors = FALSE
    )
  })
  tbl <- do.call(rbind, rows)
  
  # ---- Render as LaTeX and write to disk -------------------------------
  tex <- knitr::kable(
    tbl,
    format   = "latex",
    booktabs = TRUE,
    digits   = digits,
    caption  = caption,
    label    = label
  )
  
  writeLines(as.character(tex), con = path)
  invisible(path)
}