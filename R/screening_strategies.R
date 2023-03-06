#' HALT Screening Strategies
#'
#' List the screening strategies available in the HALT.
#' For details, see \insertCite{HALT_2;textual}{HALT}.
#'
#' @references
#' \insertAllCited{}
#'
#' @export
screening_strategies <- function() {
  c("fwr" = "Filter Without Request",
    "far" = "Filter After Request",
    "scc" = "Split-Convince-Compare")
}
