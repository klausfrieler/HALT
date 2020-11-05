library(tidyverse)
library(psychTestR)
source("R/options.R")
#source("R/main_test.R")
#source("R/item_page.R")
#source("R/feedback.R")
#source("R/utils.R")

#printf   <- function(...) print(sprintf(...))
#messagef <- function(...) message(sprintf(...))
#' HALT
#'
#' This function defines a HALT  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the HALT in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For demoing the HALT, consider using \code{\link{EDT_demo}()}.
#' For a standalone implementation of the HALT,
#' consider using \code{\link{EDT_standalone}()}.
#' @param label (Character scalar) Label to give the HALT results in the output file.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
HALT <- function(num_items = 18L,
                with_welcome = TRUE,
                with_finish = TRUE,
                label = "HALT",
                dict = HALT::HALT_dict) {
  audio_dir <- "https://media.gold-msi.org/test_materials/HLT"
  stopifnot(purrr::is_scalar_character(label),
            purrr::is_scalar_integer(num_items) || purrr::is_scalar_double(num_items),
            purrr::is_scalar_character(audio_dir),
            psychTestR::is.timeline(feedback) ||
              is.list(feedback) ||
              psychTestR::is.test_element(feedback) ||
              is.null(feedback))
  audio_dir <- gsub("/$", "", audio_dir)

  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) HALT_welcome_page(),
    psychTestR::new_timeline({
      main_test(label = label, audio_dir = audio_dir, dict = dict)
    }, dict = dict),
    scoring(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    feedback,
    if(with_finish) HALT_finished_page(),
    psychTestR::end_module())
}
