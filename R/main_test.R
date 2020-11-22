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
  keep <- c("headphones", "loudspeaker")
  if(length(test_AB_strategy) > 2){
    keep  <- strsplit(test_AB_strategy[3], ",") %>% unlist()
  }
  list(includes = include_testAB, operator = operator, keep = keep)
}

test_device <- function(test_AB_strategy, invert = F){
  function(state, ...){
    ret <- TRUE
    results <- psychTestR::get_results(state, complete = F) %>% as_tibble()

    if("HALT.device" %in% names(results)){
      if(is.scalar(results$HALT.device)){
        ret <- results$HALT.device
      }
      else{
        ret <- results$HALT.device$correct
      }
    }

    if(invert) ret <- !ret
    ret
  }
}

get_device <- function(parseAB){
  function(state,...){
    #browser()
    a_correct <- psychTestR::get_local("po6_num_correct", state)
    if(!is.null(a_correct)){
      a_correct <- a_correct >= 3
    }
    else{
      a_correct <- parseAB$operator != "or"
    }
    b_correct <- psychTestR::get_local("po7_num_correct", state)
    if(!is.null(b_correct)){
      b_correct <- b_correct >= 3
    }
    else{
      b_correct <- parseAB$operator != "or"
    }
    device <- "loudspeaker"
    if(parseAB$operator == "or"){
      if(a_correct || b_correct){
        device <- "headphones"
      }
    }
    else{
      if(a_correct && b_correct){
        device <- "headphones"
      }

    }
    correct <- device %in% parseAB$keep
    value <- format_answer(HALT_answer_format,
                           raw_answer = sprintf("A:%s;B:%s", as.character(a_correct), as.character(b_correct)),
                           answer = device,
                           correct = correct)
    psychTestR::save_result(place = state, label = "device", value = value)
  }
}
count_page <- function(){
  new_timeline(
  join(
    psychTestR::code_block(function(state, ...) set_global("counter", 0L, state)),
    while_loop(
      test = function(state, ...) get_global("counter", state) < 3L,
      logic = join(
        reactive_page(function(state, ...) {
          set_global("counter", 1L + get_global("counter", state), state)
          one_button_page(
            tags$div(
              tags$p(tags$strong(sprintf("%i", get_global("counter", state)))),
              tags$p(i18n("TESTNAME")))
          )
        })))
  ), dict = HALT::HALT_dict)
}
main_test <- function(label, max_count = 3L, audio_dir, test_AB_strategy, dict= HALT::HALT_dict) {
  parseAB <-  parse_testAB_strategy(test_AB_strategy)
  num_AB_tests <- sum(parseAB$includes)
  if(num_AB_tests == 0L){
    stop("Must include at least one of A or B")
  }
  num_pages <- 9 + 4 * num_AB_tests
  elts <- psychTestR::join(
    page_po1(audio_dir, num_pages),
    psychTestR::code_block(
      get_device(parseAB)
    ),
    psychTestR::conditional(test = test_device(test_AB_strategy, invert = T),
                            logic = HALT_stop_page(dict)),
    psychTestR::conditional(test = test_device(test_AB_strategy),
                            logic = page_calibrate(8L, num_pages, audio_dir)),

    page_force_correct(2L, num_pages, max_count, audio_dir),
    psychTestR::conditional(
      test = function(state, ...){
        counter <- psychTestR::get_local("po2_counter", state)
        answer <- psychTestR::get_local("po2", state)
        counter >= max_count && !stringr::str_detect(answer, "correct")
      },
      logic = HALT_stop_page(dict)),
    page_calibrate(3L, num_pages,  audio_dir),
    page_po4(audio_dir, max_count, num_pages),
    psychTestR::conditional(
      test = function(state, ...){
        #browser()
        counter <- psychTestR::get_local("po4_counter", state)
        answer <- psychTestR::get_local("po4", state)
        counter >= max_count && !stringr::str_detect(answer, "left")
      },
      logic = HALT_stop_page(dict)),
    page_force_correct(5L, num_pages, max_count, audio_dir),
    psychTestR::conditional(
      test = function(state, ...){
        counter <- psychTestR::get_local("po5_counter", state)
        answer <- psychTestR::get_local("po5", state)
        counter >= max_count && !stringr::str_detect(answer, "correct")
      },
      logic = HALT_stop_page(dict)),
    #if(parseAB$includes["A"]) page_testAB(6L, num_pages, audio_dir),
    #if(parseAB$includes["B"]) page_testAB(7L, num_pages, audio_dir),
    if(parseAB$includes["A"]) page_AB_section(6L, num_pages, audio_dir),
    if(parseAB$includes["B"]) page_AB_section(7L, num_pages, audio_dir),
    psychTestR::code_block(
      get_device(parseAB)
    ),
    psychTestR::conditional(test = test_device(test_AB_strategy, invert = T),
                            logic = HALT_stop_page(dict)),
    psychTestR::conditional(test = test_device(test_AB_strategy),
                            logic = page_calibrate(8L, num_pages, audio_dir)),
    page_calibrate(9L, num_pages, audio_dir),
    page_calibrate(10L, num_pages, audio_dir),
    page_calibrate(11L, num_pages, audio_dir)
  )
  elts
}


