library(tidyverse)
library(psychTestR)
#source("R/options.R")
#source("R/main_test.R")
#source("R/item_page.R")
#source("R/utils.R")

#' HALT
#'
#' This function defines a HALT  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the HALT in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the HALT,
#' consider using \code{\link{HALT_standalone}()}.
#' @param label (Character scalar) Label to give the HALT results in the output file.
#' @param max_count (Integer scalar) Maximum number to show page 2 and 3 before bailing out (defaults to  3).
#' @param test_AB_strategy (character vector) Main configuration of the HALT. Test A and Test B can used alone
#' or in conjunction to estimate the listening device of the test taker.
#' This is indicated by the first string, which can contain "A" or "B" or both ("AB").
#' If A and B are used then the next string indicates the operation how to
#' combine the information from test A and B. It should be either "or" or "and".
#' The third element contains a comma separated list of "headphones" and "loudspeaker", indicating
#' which device is allowed. If the detected device does not match any of these then test will be interrupted with a
#' warning message.
#' @param audio_dir (url). The URL of the directory containing the stimuli.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
HALT <- function(label = "HALT",
                 max_count = 3,
                 test_AB_strategy = c("AB", "or", "headphones"),
                 audio_dir = "https://media.gold-msi.org/test_materials/HLT",
                 dict = HALT::HALT_dict) {
  stopifnot(length(test_AB_strategy) == 3)
  stopifnot(purrr::is_scalar_character(label))
  stopifnot(purrr::is_scalar_character(audio_dir))
  audio_dir <- gsub("/$", "", audio_dir)

  psychTestR::join(
    psychTestR::begin_module(label),
    main_test(label = label, max_count = max_count, audio_dir = audio_dir, test_AB_strategy, dict = dict),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::end_module())
}

#' HALT Manipulation Check
#'
#' This function is an additional page to be used at the end of a battery. It allows to check whether settings were changed during test time.
#' @param label (Character scalar) Label to give the HALT results in the output file.
#' @param audio_dir (url). The URL of the directory containing the stimuli.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
HALT_manipulation_check <- function(label = "HALT_MC",
                                    audio_dir = "https://media.gold-msi.org/test_materials/HLT",
                                    dict = HALT::HALT_dict){
  stopifnot(purrr::is_scalar_character(label))
  stopifnot(purrr::is_scalar_character(audio_dir))
  audio_dir <- gsub("/$", "", audio_dir)
  psychTestR::join(
    psychTestR::begin_module(label),
    page_calibrate(12, audio_dir),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::end_module())
}


HALT_stop_page <- function(dict = HALT::HALT_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("STOP_HEAD"), style = "margin-left:25%;display:block;text-align:left"),
        shiny::div(psychTestR::i18n("STOP_TEXT"),
                   style = "margin-left:25%;margin-right:25%;display:block;text-align:left")
      )
    ), dict = dict)
}
