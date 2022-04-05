test_device <- function(invert = F, config){
  function(state, ...){
    if(config$use_scc == TRUE || config$devices_exclude == FALSE){
      return(FALSE)
    }
    ret <- TRUE
    results <- psychTestR::get_results(state, complete = F) %>% as_tibble()

    if("HALT.device_screening" %in% names(results)){
      if(is.scalar(results$HALT.device_screening)){
        ret <- results$HALT.device_screening
      }
      else{
        ret <- results$HALT.device_screening$correct
      }
    }

    if(invert) ret <- !ret
    ret
  }
}

get_device <- function(config){
  function(state,...){
    #browser()
    A_correct <- psychTestR::get_local("po6_num_correct", state)
    B_correct <- psychTestR::get_local("po7_num_correct", state)
    C_correct <- psychTestR::get_local("po13_num_correct", state)
    A <- A_correct >= config$A_threshold
    B <- B_correct >= config$B_threshold
    C <- C_correct >= config$C_threshold
    logic_expr <- HALT::test_config  %>%
      filter(method_code == config$combination_method[1]) %>%
      pull(logic_expr) %>%
      unique()

    stopifnot(length(logic_expr) == 1)
    is_hp <- parse(text = logic_expr) %>% eval()

    device <- "LS"
    if(is_hp){
      device <- "HP"
    }
    correct <- device %in% config$devices
    value <- format_answer(HALT_answer_format,
                           raw_answer = sprintf("A:%s(%s);B:%s(%s);C:%s(%s)",
                                                A_correct, as.character(A),
                                                B_correct, as.character(B),
                                                C_correct, as.character(C)),
                           answer = device,
                           A_correct = A_correct,
                           B_correct = B_correct,
                           C_correct = C_correct,
                           correct = correct)
    psychTestR::save_result(place = state, label = "device_screening", value = value)
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
                shiny::tags$p(shiny::tags$strong(sprintf("%i", psychTestR::get_global("counter", state)))),
                shiny::tags$p(psychTestR::i18n("TESTNAME"))))
          })))
    ), dict = HALT::HALT_dict)
}

main_test <- function(label,
                      audio_dir,
                      config,
                      dict = HALT::HALT_dict,
                      show_id = FALSE,
                      type = "loud") {
  if(is.character(config)) {
    if(file.exists(config)) {
      config <- read.csv(config, sep = ";")
      stopifnot(length(config) == 15 && nrow(config) == 1)
      names(config) <- names(auto_config())
    }
  }
  if (config$screening_parts) {
    config$channel_check <- TRUE
  } else {
    config$use_scc <-- FALSE
  }
  num_ABC_items <- ifelse(config$screening_parts, 18, 0)
  num_pages <- 3 + num_ABC_items + ifelse(config$channel_check, 2, 0) +
    ifelse(config$frequency_check, 4, 0)
  p04_max_count <- ifelse(config$lr_img_exclude, 0, 1)
  max_count <- config$loop_exclude
  elts <- psychTestR::join(
    page_po1(audio_dir, num_pages, type = type),
    page_force_correct(2L, num_pages, config, audio_dir, type = type, show_id = show_id),
    psychTestR::conditional(
      test = function(state, ...){
        if(config$lr_audio_exclude  == FALSE){
          return(FALSE)
        }
        counter <- psychTestR::get_local("po2_counter", state)
        answer <- psychTestR::get_local("po2", state)

        counter >= max_count && !stringr::str_detect(answer, "correct")
      },
      logic = HALT_stop_page(dict))
  )
  if (config$channel_check) {
    elts <- psychTestR::join(
      elts,
      page_po4(config, audio_dir, num_pages),
      page_po5(config, audio_dir, num_pages, type = type, show_id = show_id)
    )
  }
  elts <- psychTestR::join(
    elts,
    device_page(num_pages, config)
  )
  if (config$use_scc) {
    elts <- psychTestR::join(
      elts,
      psychTestR::conditional(
        test = function(state, ...){
          has_admissable_device <- psychTestR::get_local("device_selfreport", state)
          length(config$devices) < 2 && !has_admissable_device
        },
        logic = scc_page(dict, config))
    )
  }
  if (config$screening_parts) {
    elts <- psychTestR::join(
      elts,
      page_ABC_section(6L, num_pages, audio_dir, type = type, config = config, show_id = show_id),
      page_ABC_section(7L, num_pages, audio_dir, type = type, config = config, show_id = show_id),
      page_ABC_section(13L, num_pages, audio_dir, type = type, config = config, show_id = show_id),
      psychTestR::code_block(
        get_device(config)
      ),
      psychTestR::conditional(test = test_device(invert = T, config),
                              logic = HALT_stop_page(dict))
    )
    #if (config$frequency_check) {
    #  elts <- psychTestR::join(
    #    elts,
    #    psychTestR::conditional(test = test_device(config = config),
    #                            logic = page_calibrate(8L, num_pages, audio_dir, config = config, type = type, show_id = show_id)),
    #    page_calibrate(9L, num_pages, audio_dir, config = config, type = type, show_id = show_id),
    #    page_calibrate(10L, num_pages, audio_dir, config = config, type = type, show_id = show_id),
    #    page_calibrate(11L, num_pages, audio_dir, config = config, type = type, show_id = show_id)
    #  )
    #}
  } #else {
    if (config$frequency_check) {
      elts <- psychTestR::join(
        elts,
        page_calibrate(8L, num_pages, audio_dir, config = config, type = type, show_id = show_id),
        page_calibrate(9L, num_pages, audio_dir, config = config, type = type, show_id = show_id),
        page_calibrate(10L, num_pages, audio_dir, config = config, type = type, show_id = show_id),
        page_calibrate(11L, num_pages, audio_dir, config = config, type = type, show_id = show_id)
      )
    }
  #}
  elts
}
