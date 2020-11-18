#source("R/HALT.R")
#options(shiny.error = browser)
#debug_locally <- !grepl("shiny-server", getwd())


#' Standalone HALT
#'
#' This function launches a standalone testing session for the HALT
#' This can be used for data collection, either in the laboratory or online.
#' @param title (Scalar character) Title to display during testing. If \code{NULL}, \code{TESTNAME} from the dictionaly is used.
#' @param with_id (Logical character) Should an id page be included at the beginning.
#' @param max_count (Integer scalar) Maximum number to show page 2 and 3 before bailing out (defaults to  3).
#' @param test_AB_strategy (character vector) Main configuration of the HALT. Test A and Test B can used alone
#' or in conjunction to estimate the listening device of the test taker.
#' This is indicated by the first string, which can contain "A" or "B" or both ("AB").
#' If A and B are used then the next string indicates the operation how to
#' combine the information from test A and B. It should be either "or" or "and".
#' The third element contains a comma separated list of "headphones" and "loudspeaker", indicating
#' which device is allowed. If the detected device does not match any of these then test will be interrupted with a
#' warning message.
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
#' @param audio_dir (url). The URL of the directory containing the stimuli.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto" for default validation
#' which means ID should consist only of  alphanumeric characters.
#' @param ... Further arguments to be passed to \code{\link{HALT}()}.
#' @export
HALT_standalone  <- function(title = NULL,
                            with_id = FALSE,
                            max_count = 3,
                            test_AB_strategy = c(tests = "AB",
                                                 condition = "or",
                                                 keep = c("headphones,loudspeaker")),
                            admin_password = "HALTadmin",
                            researcher_email = "yves.wycisk@hmtm-hannover.de",
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
    HALT::HALT(label = "HALT",
               max_count = max_count,
               audio_dir = audio_dir,
               test_AB_strategy = test_AB_strategy,
               dict = dict,
               ...),
    psychTestR::code_block(function(state, ...){
      browser()
      res <- psychTestR::get_results(state, complete = T) %>% as.list() %>% as_tibble()
      #psychTestR::save_result(state, "HALT", res)
      #res <- psychTestR::get_results(state, complete = T)
    }),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::final_page(body = "")
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
