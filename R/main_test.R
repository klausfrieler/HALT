test_device <- function(invert = F){
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

get_device <- function(config){
  function(state,...){
    #browser()
    A <- psychTestR::get_local("po6_num_correct", state) >= config$A
    B <- psychTestR::get_local("po7_num_correct", state) >= config$B
    C <- psychTestR::get_local("po13_num_correct", state)  >= config$C
    device <- "LS"
    logic_expr <- HALT::test_config  %>%
      filter(method_code == config$method[1]) %>%
      pull(logic_expr) %>%
      unique()

    stopifnot(length(logic_expr) == 1)
    is_hp <- parse(text = logic_expr) %>% eval()
    if(is_hp){
      device <- "HP"
    }
    correct <- device %in% config$devices
    value <- format_answer(HALT_answer_format,
                           raw_answer = sprintf("A:%s;B:%s;C:%s",
                                                as.character(A),
                                                as.character(B),
                                                as.character(C)),
                           answer = device,
                           correct = correct)
    psychTestR::save_result(place = state, label = "device", value = value)
  }
}

count_page <- function(){
  psychTestR::new_timeline(
    psychTestR::join(
      psychTestR::code_block(function(state, ...) psychTestR::set_global("counter", 0L, state)),
      psychTestR::while_loop(
        test = function(state, ...) psychTestR::get_global("counter", state) < 3L,
        logic = psychTestR::join(
          psychTestR::reactive_page(function(state, ...) {
            psychTestR::set_global("counter", 1L + psychTestR::get_global("counter", state), state)
            psychTestR::one_button_page(
              shiny::tags$div(
                shiny::tags$p(tags$strong(sprintf("%i", psychTestR::get_global("counter", state)))),
                shiny::tags$p(psychTestR::i18n("TESTNAME"))))
          })))
    ), dict = HALT::HALT_dict)
}

main_test <- function(label, max_count = 3L, audio_dir, config, dict = HALT::HALT_dict) {
  #parseAB <-  parse_testAB_strategy(test_AB_strategy)
  num_ABC_items <- 18
  num_pages <- 9 + num_ABC_items
  elts <- psychTestR::join(

    page_po1(audio_dir, num_pages),
    page_force_correct(2L, num_pages, max_count, audio_dir),
    psychTestR::conditional(
      test = function(state, ...){
        counter <- psychTestR::get_local("po2_counter", state)
        answer <- psychTestR::get_local("po2", state)
        counter >= max_count && !stringr::str_detect(answer, "correct")
      },
      logic = HALT_stop_page(dict)),
    #page_calibrate(3L, num_pages,  audio_dir),
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
    device_page(num_pages),
    if(config$use_scc) psychTestR::conditional(
      test = function(state, ...){
        #browser()
        uses_headphones <- psychTestR::get_local("uses_headphones", state)
        !uses_headphones
      },
      logic = scc_page(dict)),
    psychTestR::conditional(
      test = function(state, ...){
        counter <- psychTestR::get_local("po5_counter", state)
        answer <- psychTestR::get_local("po5", state)
        counter >= max_count && !stringr::str_detect(answer, "correct")
      },
      logic = HALT_stop_page(dict)),
    page_ABC_section(6L, num_pages, audio_dir),
    page_ABC_section(7L, num_pages, audio_dir),
    page_ABC_section(13L, num_pages, audio_dir),
    psychTestR::code_block(
      get_device(config)
    ),
    psychTestR::conditional(test = test_device(invert = T),
                            logic = HALT_stop_page(dict)),
    psychTestR::conditional(test = test_device(),
                            logic = page_calibrate(8L, num_pages, audio_dir)),
    page_calibrate(9L, num_pages, audio_dir),
    page_calibrate(10L, num_pages, audio_dir),
    page_calibrate(11L, num_pages, audio_dir)
  )
  elts
}


