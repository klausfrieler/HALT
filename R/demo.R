#' Demo HALT
#'
#' This function launches a demo for the HALT, which is wrapped around a RAT test.
#'
#' @param num_items (Integer scalar) Number of items in the RAT test.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test. Defaults to a graph-based feedback page.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' Defaults to \code{"demo"}.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' Defaults to \email{longgold@gold.uc.ak},
#' the email address of this package's developer.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param language The language you want to run your demo in.
#' Possible languages include English (\code{"EN"}) and German (\code{"DE"}).
#' The first language is selected by default
#' @param ... Further arguments to be passed to \code{\link{RAT}()}.
#' @export
#'
HALT_demo <- function(num_items = 3L,
                     feedback = NULL,
                     config = HALT::auto_config(use_scc = T),
                     admin_password = "demo",
                     researcher_email = "",
                     dict = HALT::HALT_dict,
                     language = "en",
                     ...) {
  elts <- psychTestR::join(
    HALT::HALT(dict = dict),
    RAT::RAT(num_items = num_items,
             with_welcome = FALSE,
             take_training = TRUE,
             feedback = feedback,
             dict = RAT::RAT_dict,
             ...),
    psychTestR::final_page(body = "")
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = "HALT Demo + RAT",
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = TRUE,
                                   languages = tolower(language)))
}
