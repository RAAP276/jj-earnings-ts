# =============================================================================
# Data loading.
#
# The Johnson & Johnson quarterly earnings series (1960-Q1 to 1980-Q4)
# ships with base R as `datasets::JohnsonJohnson`. No external file needed.
# =============================================================================


#' Load the Johnson & Johnson earnings series
#'
#' Returns the `JohnsonJohnson` dataset shipped with base R. Wrapping it
#' here keeps a single source of truth for the series across the project:
#' if the data source ever changes, only this file needs to be edited.
#'
#' @return A quarterly `ts` object with 84 observations starting at 1960-Q1.
#' @export
#'
#' @examples
#' jj <- load_jj_series()
#' stats::start(jj)      # 1960, 1
#' stats::end(jj)        # 1980, 4
#' stats::frequency(jj)  # 4
load_jj_series <- function() {
  datasets::JohnsonJohnson
}