
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


main_test <- function(label, num_items_in_test, audio_dir, dict = HALT::HALT_dict) {
  elts <- c()
  item_bank <- HALT::HALT_item_bank
  item_sequence <- sample(1:nrow(item_bank), num_items_in_test)
  #browser()
  for(i in 1:length(item_sequence)){
    item <- HALT::HALT_item_bank[item_sequence[i],]
    emotion <- psychTestR::i18n(item[1,]$emotion_i18)
    #printf("Emotion %s ", emotion)
    item_page <-
      HALT_item(label = item$item_number[1],
               correct_answer = item$correct[1],
               prompt = get_prompt(i, num_items_in_test, emotion),
               audio_file = item$audio_file[1],
               audio_dir = audio_dir,
               save_answer = TRUE)
    elts <- psychTestR::join(elts, item_page)
  }
  elts
}


item_page <- function(item_number, item_id, num_items_in_test, audio_dir, dict = HALT::HALT_dict) {
  item <- HALT::HALT_item_bank %>% filter(item_number == item_id) %>% as.data.frame()
  emotion <- psychTestR::i18n(item[1,]$emotion_i18)
  HALT_item(label = item_id,
           correct_answer = item$correct[1],
           prompt = get_prompt(item_number, num_items_in_test, emotion),
           audio_file = item$audio_file[1],
           audio_dir = audio_dir,
           save_answer = TRUE)

}

get_prompt <- function(item_number, num_items_in_test, emotion, dict = HALT::HALT_dict) {
  shiny::div(
    shiny::h4(
      psychTestR::i18n(
        "PROGRESS_TEXT",
        sub = list(num_question = item_number,
                   test_length = if (is.null(num_items_in_test))
                     "?" else
                       num_items_in_test)),
      style  = "text_align:left"
    ),
    shiny::p(
      psychTestR::i18n("ITEM_INSTRUCTION",
                       sub = list(emotion = emotion)),
      style = "margin-left:20%;margin-right:20%;text-align:justify")
    )
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
