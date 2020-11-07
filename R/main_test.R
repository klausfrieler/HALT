
scoring <- function(){
  psychTestR::code_block(function(state,...){
    #browser()
    results <- psychTestR::get_results(state = state,
                                       complete = FALSE,
                                       add_session_info = FALSE) %>% as.list()

    sum_score <- sum(purrr::map_lgl(results$HALT, function(x) x$correct))
    num_question <- length(results$HALT)
    perc_correct <- sum_score/num_question
    psychTestR::save_result(place = state,
                 label = "score",
                 value = perc_correct)
    psychTestR::save_result(place = state,
                             label = "num_questions",
                             value = num_question)

  })

}

parse_testAB_strategy <- function(test_AB_strategy){
  #browser()
  include_testAB <- c(A = FALSE, B = FALSE)
  if(length(test_AB_strategy) > 0){
    if("A" %in% unlist(strsplit(test_AB_strategy[1], ""))) {
      include_testAB["A"] <- TRUE
    }
    if("B" %in% unlist(strsplit(test_AB_strategy[1], ""))) {
      include_testAB["B"] <- TRUE
    }
  }
  operator <- "or"
  if(length(test_AB_strategy) > 1){
    operator <- test_AB_strategy[2]
  }
  list(includes = include_testAB, operator = operator)
}

main_test <- function(label, audio_dir, test_AB_strategy, dict= HALT::HALT_dict) {
  parseAB <-  parse_testAB_strategy(test_AB_strategy)
  if(sum(parseAB$includes) == 0){
    stop("Must include at least one of A or B")
  }
  elts <- psychTestR::join(
    page_po1(audio_dir),
    page_force_correct(2, audio_dir),
    page_calibrate(3, audio_dir),
    page_po4(audio_dir),
    page_force_correct(5, audio_dir),
    if(parseAB$includes["A"]) page_testAB(6, audio_dir),
    if(parseAB$includes["B"]) page_testAB(7, audio_dir),
    code_block(function(state,...){

    }),
    page_calibrate(8, audio_dir),
    page_calibrate(9, audio_dir),
    page_calibrate(10, audio_dir),
    page_calibrate(11, audio_dir)
  )
  elts
}


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
