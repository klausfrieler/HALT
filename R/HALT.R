library(tidyverse)
library(psychTestR)
source("R/options.R")
source("R/main_test.R")
source("R/item_page.R")
source("R/utils.R")

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
#' @param with_welcome (logical scalar) Show welcome page or not.
#' @param with_finish (logica scalar) Show finish page or not
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
HALT <- function(label = "HALT",
                 with_welcome = FALSE,
                 with_finish = FALSE,
                 test_AB_strategy = c("AB", "or", "headphones"),
                 audio_dir,
                 dict = HALT::HALT_dict) {
  stopifnot(purrr::is_scalar_character(label))
  audio_dir <- gsub("/$", "", audio_dir)

  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) HALT_welcome_page(),
    main_test(label = label, audio_dir = audio_dir, test_AB_strategy, dict = dict),
    #scoring(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    #feedback,
    if(with_finish) HALT_finished_page(),
    psychTestR::end_module())
}

HALT_manipulation_check <- function(label = "HALT_MC", audio_dir, dict = HALT::HALT_dict){
  stopifnot(purrr::is_scalar_character(label))
  audio_dir <- gsub("/$", "", audio_dir)
  psychTestR::join(
    psychTestR::begin_module(label),
    page_calibrate(12, audio_dir),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::end_module())
}

#' @export
HALT_welcome_page <- function(dict = HALT::HALT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("WELCOME")),
        shiny::div(psychTestR::i18n("INTRO_TEXT"),
                   style = "margin-left:0%;display:block")
      ),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

#' @export
HALT_finished_page <- function(dict = HALT::HALT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::div(psychTestR::i18n("SUCCESS"),
                   style = "margin-left:0%;display:block"),
        button_text = psychTestR::i18n("CONTINUE")
      )
    ), dict = dict)
}

#' @export
HALT_final_page <- function(dict = HALT::HALT_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        shiny::div(psychTestR::i18n("SUCCESS"),
                   style = "margin-left:0%;display:block")
      )
    ), dict = dict)
}
