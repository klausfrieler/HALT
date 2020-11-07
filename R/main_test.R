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
    keep  <- test_AB_strategy
  }
  list(includes = include_testAB, operator = operator, keep = keep)
}

test_device <- function(test_AB_strategy, invert = F){
  function(state, ...){
    ret <- TRUE
    #browser()
    results <- psychTestR::get_results(state, complete = F) %>% as_tibble()
    if("HALT.device" %in% names(results)){
      if(length(test_AB_strategy) > 2){
        if(!(results$HALT.device %in% test_AB_strategy[3])){
          ret <- FALSE
        }
      }
    }
    if(invert) ret <- !ret
    ret
  }
}
get_device <- function(parseAB){
  function(state,...){
    browser()
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
    psychTestR::save_result(place = state, label = "device", value = device)
  }
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
    psychTestR::code_block(
      get_device(parseAB)
    ),
    psychTestR::conditional(test = test_device(test_AB_strategy, invert = T),
                            logic = HALT_finished_page(dict)),
    psychTestR::conditional(test = test_device(test_AB_strategy),
                            logic = page_calibrate(8, audio_dir)),
    page_calibrate(8, audio_dir),
    page_calibrate(9, audio_dir),
    page_calibrate(10, audio_dir),
    page_calibrate(11, audio_dir)
  )
  elts
}


