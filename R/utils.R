messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

tagify <- function(x) {
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if (is.character(x)) {
    stopifnot(is.scalar(x))
    shiny::p(x)
  } else x
}

is.null.or <- function(x, f) {
  is.null(x) || f(x)
}

format_answer <- function(type = "data.frame", main_value = "correct", ...){
  full_answer <- tibble(...)
  if(type == "scalar"){
    ret <- full_answer[[main_value]]
    attr(ret, "full_answer") <- full_answer
  }
  else{
    ret <- full_answer
  }
  ret
}
