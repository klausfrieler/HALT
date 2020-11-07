
media_js <- list(
  media_not_played = "var media_played = false;",
  media_played = "media_played = true;",
  play_media = "document.getElementById('media').play();",
  show_media   = paste0("if (!media_played) ",
                        "{document.getElementById('media')",
                        ".style.visibility='inherit'};"),
  hide_media   = paste0("if (media_played) ",
                          "{document.getElementById('media')",
                          ".style.visibility='hidden'};"),
  show_media_btn = paste0("if (!media_played) ",
                          "{document.getElementById('btn_play_media')",
                          ".style.visibility='inherit'};"),
  hide_media_btn = paste0("document.getElementById('btn_play_media')",
                          ".style.visibility='hidden';"),
  show_responses = "document.getElementById('response_ui').style.visibility = 'inherit';document.getElementById('text_input').style.visibility = 'inherit'"
)



media_mobile_play_button <- shiny::tags$p(
  shiny::tags$button(shiny::tags$span("\u25B6"),
                     type = "button",
                     id = "btn_play_media",
                     style = "visibility: hidden",
                     onclick = media_js$play_media)
)
get_audio_ui <- function(url,
                         type = tools::file_ext(url),
                         autoplay = TRUE,
                         width = 0,
                         wait = TRUE,
                         loop = FALSE) {
  #print(url)
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type),
            purrr::is_scalar_logical(wait),
            purrr::is_scalar_logical(loop))
  src    <- shiny::tags$source(src = url, type = paste0("audio/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  audio  <- shiny::tags$audio(
    script,
    src,
    id = "media",
    preload = "auto",
    autoplay = if(autoplay) "autoplay",
    width = width,
    loop = if (loop) "loop",
    oncanplaythrough = media_js$show_media_btn,
    onplay = paste0(media_js$media_played, media_js$hide_media_btn),
    #onended = if (wait) paste0(media_js$show_responses, media_js$hide_media) else "null",
    onended = if (wait) media_js$show_responses else "null"
  )
  shiny::tags$div(audio, media_mobile_play_button)
}



audio_text_page <- function(label,
                            prompt,
                            audio_url,
                            correct_answer = "",
                            save_answer = TRUE,
                            on_complete = NULL,
                            admin_ui = NULL) {
  #browser()
  stopifnot(purrr::is_scalar_character(label))
  audio_ui <- get_audio_ui(audio_url, wait = TRUE, loop = FALSE, width = 200)

  get_answer <- function(input, state, ...) {
    #browser()
    correct_answers <- as.numeric(strsplit(correct_answer, ",") %>% unlist())
    min_val <- min(correct_answers)
    max_val <- max(correct_answers)
    raw_answer <- as.numeric(gsub("answer", "", input$text_input))
    if(is.na(raw_answer)){
      return(NA)
    }
    if(raw_answer < min_val) {
      answer <- "too quiet"
    }
    else if(raw_answer > max_val){
      answer <- "imprecise"
    }
    else{
      answer <- "correct"
    }
    #browser()
    psychTestR::set_local(key = substr(label, 1, 3), value = answer, state = state)
    tibble(label = label,
           raw_answer = raw_answer,
           answer = answer,
           correct = answer == "correct")

  }
  validate <- function(answer, ...){
    #browser()
    !is.na(answer)
  }
  text_input <- shiny::textInput("text_input", label = label, placeholder = "", width = "50")
  body = shiny::div(
    onload = "document.getElementById('text_input').value = '';",
    tagify(prompt),
    audio_ui,
    text_input
  )
  ui <- shiny::div(body, trigger_button("next", psychTestR::i18n("CONTINUE"), enable_after = 10))
  messagef("Created page %s (CONTINUE = %s)", label, psychTestR::i18n("CONTINUE"))

  psychTestR::page(ui = ui, label = label,  get_answer = get_answer, save_answer = save_answer,
                   validate = validate, on_complete = on_complete, final = FALSE,
                   admin_ui = admin_ui)
}

HALT_audio_NAFC_page <- function(page_no,
                                 sub_id,
                                 save_answer = TRUE,
                                 admin_ui = NULL){
  label <- sprintf("po%d%s", page_no, sub_id)
  stopifnot(purrr::is_scalar_character(label))
  audio_url <- get_audio_url(audio_dir, page_no, sub_id)
  correct_answers <- get_item(page_no, sub_id, "correct_answer")
  prompt <- psychTestR::i18n(sprintf("THLT_%04d_PROMPT", page_no))
  on_complete <- function(answer, state, ...) {
    #browser()
    correct_answers <- as.numeric(strsplit(correct_answers, ",") %>% unlist())
    raw_answer <- as.numeric(answer)
    correct <- as.integer(raw_answer %in% correct_answers)
    key <- sprintf("%s_counter", substr(label, 1, 3))
    counter <- psychTestR::get_local(key = key, state = state)
    if(is.null(counter)){
      psychTestR::set_local(key = key, value = 1, state = state)
      messagef("Init counter for %d", page_no)
    }
    else{
      psychTestR::set_local(key = key, value = as.integer(counter) + 1, state = state)
      messagef("Counter now %d for %d ", as.integer(counter) + 1, page_no)
    }
    key <- sprintf("%s_num_correct", substr(label, 1, 3))
    num_correct <- psychTestR::get_local(key = key, state = state)
    if(is.null(num_correct)){
      psychTestR::set_local(key = key, value = correct, state = state)
      messagef("Init num_correct for %d", page_no)
    }
    else{
      psychTestR::set_local(key = key, value = as.integer(num_correct) + correct, state = state)
      messagef("Num_correct now %d for %d", as.integer(num_correct) + correct, page_no)
    }
  }
  labels  <- map_chr(sprintf("THLT_%04d_CHOICES%d", page_no, 1:4), psychTestR::i18n)
  messagef("Created page %s (CONTINUE = %s)", label, psychTestR::i18n("CONTINUE"))
  psychTestR::audio_NAFC_page(label = label, prompt = prompt, choices = as.character(1:4),
                              labels = labels, url = audio_url, save_answer = F, arrange_choices_vertically = T,
                              on_complete = on_complete)

}

get_seed <-function(state, page_no){
  seed <-  psychTestR::get_session_info(state, complete = F)$time_started %>%
    digest::sha1() %>%
    charToRaw() %>%
    as.integer() %>%
    sum()
  seed + page_no
}

select_stimulus <- function(page_no, sub_id){
  function(state, ...){
    #browser()
    seed <-  get_seed(state, page_no)
    set.seed(seed)
    selection <- sample(letters[1:3], 1)
    messagef("Seed %d from time %s, selection = %s, sub_id = %s, page_no =%d",
             seed,
             as.character(psychTestR::get_session_info(state, complete = F)$time_started),
             selection,
             sub_id,
             page_no)
    selection == sub_id
  }
}

get_item <- function(page_no, sub_id = "", column){
  item_bank %>% filter(stimulus_id == sprintf("po%d%s", page_no, sub_id)) %>% pull(!!sym(column))
}

get_audio_url <- function(audio_dir = "https://media.gold-msi.org/test_materials/HLT", page_no, sub_id){
  file.path(audio_dir, get_item(page_no, sub_id, "audio_file"))

}

warning_page <- function(label, warning_message){
  psychTestR::new_timeline(
    psychTestR::one_button_page(body = psychTestR::i18n(warning_message),
                              button_text = psychTestR::i18n("AGAIN")),
    dict = HALT::HALT_dict)
}

HALT_base_page <- function(page_no, sub_id, audio_dir, save_answer = T){
  psychTestR::new_timeline(
    audio_text_page(label = sprintf("po%d%s", page_no, sub_id),
                  prompt = shiny::div(psychTestR::i18n(sprintf("THLT_%04d_PROMPT", page_no))),
                  audio_url = get_audio_url(audio_dir, page_no, sub_id),
                  correct_answer = get_item(page_no, sub_id, "correct_answer"),
                  save_answer = save_answer),
    dict = HALT::HALT_dict)
}

HALT_random_stimulus_page <- function(page_no, audio_dir, save_answer = TRUE){
  psychTestR::join(
      psychTestR::conditional(test = select_stimulus(page_no, "a"),
                              logic = HALT_base_page(page_no, "a", audio_dir, save_answer)),
      psychTestR::conditional(test = select_stimulus(page_no, "b"),
                              logic = HALT_base_page(page_no, "b", audio_dir, save_answer)),
      psychTestR::conditional(test = select_stimulus(page_no, "c"),
                              logic = HALT_base_page(page_no, "c", audio_dir, save_answer)))
}

HALT_testAB_page <- function(page_no, sub_id, audio_dir, save_answer = T){
  psychTestR::new_timeline(
    HALT_audio_NAFC_page(page_no, sub_id, save_answer = save_answer),
    dict = HALT::HALT_dict)
}

HALT_testAB_page_wrapper <- function(page_no, audio_dir, save_answer = TRUE){
  psychTestR::join(
    psychTestR::conditional(test = select_stimulus(page_no, "a"),
                            logic = HALT_testAB_page(page_no, "a", audio_dir, save_answer)),
    psychTestR::conditional(test = select_stimulus(page_no, "b"),
                            logic = HALT_testAB_page(page_no, "b", audio_dir, save_answer)))
}

test_answer <- function(page_no, value, invert = F){
  function(state, ...){
    #browser()
    answer <- psychTestR::get_local(sprintf("po%d", page_no), state)
    ret <- !is.null(answer) && answer == value
    if(invert){
      ret <- !ret
    }
    ret
  }
}

test_counter <- function(page_no, max_count = 4){
  function(state, ...){
    counter <- psychTestR::get_local(sprintf("po%d_counter", page_no), state)
    if(is.null(counter)){
      psychTestR::set_local(sprintf("po%d_counter", page_no), value = 0, state)
      messagef("Init counter for page_no %d, max_count %d", page_no, max_count)
    }
    else{
      messagef("Counter %d, page_no %d, max_count %d", counter, page_no, max_count)
    }
    is.null(counter) || as.integer(counter) < max_count
  }
}

page_po1 <- function(audio_dir){
  psychTestR::new_timeline(
    psychTestR::volume_calibration_page(
      url = get_audio_url(audio_dir, 1, ""),
      prompt = psychTestR::i18n("THLT_0001_PROMPT"),
      button_text  = psychTestR::i18n("THLT_0001_CHOICES")),
    dict = HALT::HALT_dict)
}

select_left_right <- function(direction){
  function(state, ...){
    #browser()
    seed <-  get_seed(state, 0)
    set.seed(seed)
    selection <- sample(c("left", "right"), 1)
    messagef("Selected %s for %s (Seed = %d)", selection, direction, seed )
    selection == direction
  }
}

left_right_page <- function(audio_dir, right_first){
  perm <- c(1,2)
  if(right_first){
    perm <- c(2,1)
  }
  #browser()
  img_url <- gsub("mp3", "png", get_audio_url(audio_dir = audio_dir, page_no = 4, sub_id = ""))
  psychTestR::new_timeline(
    psychTestR::NAFC_page("po4",
                          prompt = shiny::div(p(psychTestR::i18n("THLT_0004_PROMPT")),
                                              p(img(src = img_url, style = "width:200px;text_align:center"))),
                          choices = c("left", "right")[perm],
                          labels = c(psychTestR::i18n("THLT_0004_CHOICES1"),
                                     psychTestR::i18n("THLT_0004_CHOICES2"))[perm],
                          save_answer = TRUE),
  dict = HALT::HALT_dict)
}

page_po4 <- function(audio_dir){
  psychTestR::join(
    psychTestR::conditional(test = select_left_right("left"),
                            logic = left_right_page(audio_dir, right_first = F)),
    psychTestR::conditional(test = select_left_right("right"),
                            logic = left_right_page(audio_dir, right_first = T)))
}

page_force_correct <- function(page_no, audio_dir){
  warning_label <- sprintf("warning_po%d", page_no)
  psychTestR::join(
    psychTestR::while_loop(
      test = test_answer(page_no, "correct", invert = T),
      logic = psychTestR::join(
        psychTestR::conditional(test = test_answer(page_no, "too quiet"), logic = warning_page(warning_label, "WARNING_TOO_QUIET")),
        psychTestR::conditional(test = test_answer(page_no, "imprecise"), logic = warning_page(warning_label, "WARNING_IMPRECISE")),
        HALT_random_stimulus_page(page_no = page_no, audio_dir = audio_dir, save_answer = F)
      )
    ))
}

page_calibrate <- function(page_no, audio_dir){
  HALT_random_stimulus_page(page_no = page_no, audio_dir = audio_dir, save_answer = T)
}

page_testAB <- function(page_no, audio_dir){
  warning_label <- sprintf("warning_po%d", page_no)
  psychTestR::join(
    psychTestR::while_loop(
      test = test_counter(page_no),
      logic = psychTestR::join(
        HALT_testAB_page_wrapper(page_no = page_no, audio_dir = audio_dir, save_answer = F)
      )
    ))
}
