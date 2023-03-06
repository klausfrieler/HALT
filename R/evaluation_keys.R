#' Evaluation Keys / Test Combinations
#'
#' This functions returns all screening test (combination) methods available in
#' HALT with their respective evaluation key (or method code).
#' For details, see \insertCite{HALT_2;textual}{HALT}.
#'
#' @references
#' \insertAllCited{}
#'
#' @export
evaluation_keys <- function() {
  c("1" = "Test A HP",
    "2" = "Test B HP",
    "3" = "Test C HP",
    "4" = "Test A AND Test B HP",
    "5" = "Test A OR Test B HP",
    "6" = "Test B AND Test C HP",
    "7" = "Test B OR Test C HP",
    "8" = "Test A AND Test C HP",
    "9" = "Test A OR Test C HP",
    "10" = "at least 1 HP",
    "11" = "at least 2 HP",
    "12" = "all HP",
    "13" = "(A OR B) AND C HP",
    "14" = "(B OR C) AND A HP",
    "15" = "(A OR C) AND B HP",
    "16" = "(A AND B) OR C HP",
    "17" = "(B AND C) OR A HP",
    "18" = "(A AND C) OR B HP")
}

#' @rdname evaluation_keys
#' @export
combination_methods <- function() {
  evaluation_keys()
}
