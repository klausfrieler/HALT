source("R/HALT.R")
options(shiny.error = browser)
debug_locally <- !grepl("shiny-server", getwd())


#' Standalone HALT
#'
#' This function launches a standalone testing session for the HALT
#' This can be used for data collection, either in the laboratory or online.
#' @param title (Scalar character) Title to display during testing.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"en"}),
#' German (\code{"de"}).
#' The first language is selected by default
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto" for default validation
#' which means ID should consist only of  alphanumeric characters.
#' @param ... Further arguments to be passed to \code{\link{HALT}()}.
#' @export
HALT_standalone  <- function(title = NULL,
                            with_id = TRUE,
                            with_welcome = FALSE,
                            test_AB_strategy = c("AB", "or"),
                            admin_password = "conifer",
                            researcher_email = "longgold@gold.uc.ak",
                            languages = c("en", "de"),
                            dict = HALT::HALT_dict,
                            audio_dir = "https://media.gold-msi.org/test_materials/HLT",
                            validate_id = "auto",
                            ...) {
  elts <- psychTestR::join(
    if(with_id)
      psychTestR::new_timeline(
        psychTestR::get_p_id(prompt = psychTestR::i18n("ENTER_ID"),
                             button_text = psychTestR::i18n("CONTINUE"),
                             validate = validate_id),
        dict = dict),
    if(with_welcome) HALT_welcome_page(dict = dict),
    HALT::HALT(label = "HALT",
               with_welcome =  FALSE,
               with_finish = FALSE,
               audio_dir = audio_dir,
               test_AB_strategy = test_AB_strategy,
               dict = dict,
               ...),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    HALT:::HALT_final_page(dict = dict)
  )
  if(is.null(title)){
    #extract title as named vector from dictionary
    title <-
      HALT::HALT_dict  %>%
      as.data.frame() %>%
      dplyr::filter(key == "TESTNAME") %>%
      dplyr::select(-key) %>%
      as.list() %>%
      unlist()
    names(title) <- tolower(names(title))
  }

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   languages = tolower(languages)))
}
