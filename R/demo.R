#' Demo HALT
#'
#' This function launches a demo for the HALT, which is just a wrapper to HALT_standalong with an no_screening flag.
#' This function launches a demo for the HALT, which is just a wrapper to HALT_standalong with an no_screening flag.
#'
#' @param config (HALT_config object) A HALT_config object, generate with HATL::make_config() or auto_config() (default).
#' @param no_screening (Scalar booelan) Whether to use the screening part or not.
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
#' @export
#'
HALT_demo <- function(config = HALT::auto_config(use_scc = T),
                      no_screening = TRUE,
                      admin_password = "demo",
                      researcher_email = "",
                      dict = HALT::HALT_dict,
                      language = "en") {
  elts <- psychTestR::join(
    HALT::HALT(dict = dict, no_screening = no_screening),
    psychTestR::final_page(body = "")
  )
  title = "HALT Demo"
  if(no_screening){
    title <- "HALT Demo Without Screening)"

  }
  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = TRUE,
                                   languages = tolower(language)))
}
