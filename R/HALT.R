#library(tidyverse)
#library(psychTestR)

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
#' @param config (object of class HALT_config or path to config file generated with HALTConfig)
#' Use the functions auto_config() or make_config() to generate this or provide a filename of a config file generated with the HALTConfig Shiny App
#' See also the documentation there for further explanations.
#' @param audio_dir (url). The URL of the directory containing the stimuli.
#' @param no_screening (Scalar boolean). Whether to include screening parts or not.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
HALT <- function(label = "HALT",
                 config = HALT::auto_config(),
                 audio_dir = "https://media.gold-msi.org/test_materials/HLT",
                 no_screening = FALSE,
                 dict = HALT::HALT_dict) {
  stopifnot(is(config, "HALT_config") || is.character(config) && file.exists(config))
  stopifnot(purrr::is_scalar_character(label))
  stopifnot(purrr::is_scalar_character(audio_dir))
  audio_dir <- gsub("/$", "", audio_dir)
  main_test <-  main_test(label = label, audio_dir = audio_dir, config, dict = dict)
  if(no_screening){
    main_test <- main_test_no_screening(label = label, audio_dir = audio_dir, config, dict = dict)
  }

  psychTestR::join(
    psychTestR::begin_module(label),
    main_test,
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::end_module())
}

HALT_stop_page <- function(dict = HALT::HALT_dict){
  psychTestR::join(
    psychTestR::code_block(function(state, ...){
      psychTestR::elt_save_results_to_disk(complete = TRUE)
    }
    ),
    psychTestR::new_timeline(
      psychTestR::final_page(
        body = shiny::div(
          shiny::h4(psychTestR::i18n("STOP_HEAD"), style = "margin-left:25%;display:block;text-align:left"),
          shiny::div(psychTestR::i18n("STOP_TEXT"),
                     style = "margin-left:25%;margin-right:25%;display:block;text-align:left")
        )
      ), dict = dict)
  )
}
