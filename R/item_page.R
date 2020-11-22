HALT_standard_style <- "margin-left:25%;text-align:justify;display:block;margin-right:25%"
HALT_answer_format <- "tibble"

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
                         autoplay = FALSE,
                         show_controls = TRUE,
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
    controls = if (show_controls) "controls",
    #onended = if (wait) paste0(media_js$show_responses, media_js$hide_media) else "null",
    onended = if (wait) media_js$show_responses else "null"
  )
  if(show_controls){
    return(shiny::tags$div(audio))
  }
  shiny::tags$div(audio, media_mobile_play_button)
}

translate_answer <- function(raw_answer, correct_answer, page_no, sub_id){
  answer_labels <- list("po2" = c("too quiet", "imprecise", "correct"),
                        "po3" = c("too quiet", "imprecise", "correct"),
                        "po5" = c("stereo channels, interchanged", "mono", "stereo channels correct"),
                        "po8" = c("frequency not heard", "frequency not heard", "frequency heard"),
                        "po9" = c("frequency not heard", "frequency not heard", "frequency heard"),
                        "po10" = c("frequency not heard", "frequency not heard", "frequency heard"),
                        "po11" = c("frequency not heard", "frequency not heard", "frequency heard"),
                        "po12" = c("descreased", "increased", "no manipulation")
  )
  page_label <- sprintf("po%d", page_no)
  correct_answers <- as.numeric(strsplit(correct_answer, ",") %>% unlist())
  min_val <- min(correct_answers)
  max_val <- max(correct_answers)
  if(is.na(raw_answer)){
    messagef("Warning: Found NA as answer in HALT::audio_text_page")
    return("imprecise")
  }
  if(page_no == 5L){
    if(!(raw_answer %in% correct_answers)){
      answer <- "imprecise"
    }
    else{
        answer <- answer_labels[["po5"]][list("a" = c("6" = 3, "4" = 1, "10" = 2 ),
                     "b" = c("4" = 3, "4" = 3, "10" = 2 ),
                     "c" = c("2" = 3, "8" = 1, "10" = 2 ))[[sub_id]][as.character(raw_answer)]]
    }
    #messagef("Page po%d%s, raw answer %s, answer %s", page_no, sub_id, raw_answer, answer)
    return(answer)
  }
  if(raw_answer < min_val) {
    answer <- answer_labels[[page_label]][1]
  }
  else if(raw_answer > max_val){
    answer <- answer_labels[[page_label]][2]
  }
  else{
    answer <- answer_labels[[page_label]][3]
  }
  #messagef("Page po%d%s, raw answer %s, answer %s", page_no, sub_id, raw_answer, answer)
  answer
}

get_item <- function(page_no, sub_id = "", column){
  item_bank %>% filter(stimulus_id == sprintf("po%d%s", page_no, sub_id)) %>% pull(!!rlang::sym(column))
}

get_audio_url <- function(audio_dir = "https://media.gold-msi.org/test_materials/HLT", page_no, sub_id){
  file.path(audio_dir, get_item(page_no, sub_id, "audio_file"))
}

volume_calibration_page <- function (url, type = tools::file_ext(url), prompt = NULL, style = "", button_text = "Next",
                                     on_complete = NULL, admin_ui = NULL, btn_play_prompt = "Click here to play"){
  if (is.null(prompt))
    prompt <- shiny::div(shiny::p("You should hear some audio playing.",
                                  "Please adjust the volume to a comfortable level before continuing."),
                         shiny::p("If you cannot make the audio play at a comfortable level,",
                                  "please do not continue, but instead ask the researcher for help."), style = style)
  else{
    prompt <- shiny::div(prompt, shiny::p(""), style = style)
  }
  psychTestR::audio_NAFC_page(label = "volume_calibration", prompt = prompt,
                  choices = button_text, save_answer = FALSE, on_complete = on_complete,
                  url = url, type = type, wait = TRUE, loop = FALSE, admin_ui = admin_ui,
                  btn_play_prompt = btn_play_prompt, show_controls = TRUE)
}

audio_text_page <- function(page_no,
                            sub_id,
                            prompt,
                            audio_url,
                            correct_answer = "",
                            save_answer = TRUE,
                            on_complete = NULL,
                            admin_ui = NULL) {

  stopifnot(purrr::is_scalar_integer(page_no))
  stopifnot(purrr::is_scalar_character(sub_id))
  audio_ui <- get_audio_ui(audio_url, wait = TRUE, loop = FALSE, width = 200L)
  label <- sprintf("po%d%s", page_no, sub_id)
  #messagef("Page no: %d, sub id = %s, correct: %s", page_no, sub_id, correct_answer)
  get_answer <- function(input, state, ...) {
    raw_answer <- as.numeric(gsub("answer", "", input$text_input))
    answer <- translate_answer(raw_answer, correct_answer, page_no, sub_id)
    psychTestR::set_local(key = substr(label, 1, 3), value = answer, state = state)
    counter <- as.numeric(psychTestR::get_local(key = sprintf("%s_counter", substr(label, 1, 3)), state = state))
    #psychTestR::set_local(key = sprintf("%s_counter", substr(label, 1, 3)), value = counter + 1, state = state)
    #messagef("Set %s_counter to %d", substr(label, 1, 3), counter + 1L)
    format_answer(HALT_answer_format,
                  raw_answer = as.character(raw_answer),
                  answer = answer,
                  correct = stringr::str_detect(answer, "correct"))

  }
  validate <- function(answer, ...){
    !all(is.na(answer))
  }
  response_ui <- shiny::div(
    shiny::p(
      shiny::textInput("text_input", label = label, placeholder = "", width = "50"),
      psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE"))),
    id = "response_ui", style = "visibility:hidden")

  ui <- shiny::div(
    onload = "document.getElementById('text_input').value = '';",
    tagify(prompt),
    audio_ui,
    response_ui
  )
  #messagef("Created page %s (CONTINUE = %s)", label, psychTestR::i18n("CONTINUE"))
  psychTestR::page(ui = ui, label = label,  get_answer = get_answer, save_answer = save_answer,
                   validate = validate, on_complete = on_complete, final = FALSE,
                   admin_ui = admin_ui)
}

local_audio_NAFC_page <- function (label, prompt, choices, url, labels = NULL, type = tools::file_ext(url),
                                   save_answer = TRUE, on_complete = NULL,
                                   arrange_choices_vertically = length(choices) > 2L,
                                   wait = TRUE, loop = FALSE, admin_ui = NULL,
                                   autoplay = FALSE,
                                   show_controls = FALSE,
                                   allow_download = FALSE){
  stopifnot(is.scalar.character(label), is.character(choices),
            is.scalar.character(url), is.scalar.character(url), is.scalar.logical(arrange_choices_vertically),
            is.scalar.logical(wait), is.scalar.logical(loop))
  audio_ui <- shiny::tags$div(shiny::tags$audio(
    shiny::tags$head(shiny::tags$script(shiny::HTML(media_js$media_not_played))),
    shiny::tags$source(src = url, type = paste0("audio/", type)),
    id = "media", preload = "auto",
    autoplay = if(autoplay) "autoplay", loop = if (loop)
      "loop", oncanplaythrough = media_js$show_media_btn,
    onplay = paste0(media_js$media_played, media_js$hide_media_btn),
    onended = if (wait) media_js$show_responses else "null",
    controls = if (show_controls) "controls",
    controlsList = if (!allow_download) "nodownload"),
    "")
  prompt2 <- shiny::div(tagify(prompt), audio_ui)
  psychTestR::NAFC_page(label = label, prompt = prompt2, choices = choices,
                        labels = labels, save_answer = save_answer, on_complete = on_complete,
                        arrange_vertically = arrange_choices_vertically, hide_response_ui = wait,
                        response_ui_id = "response_ui", admin_ui = admin_ui)
}

HALT_audio_NAFC_page <- function(page_no,
                                 sub_id,
                                 num_pages,
                                 test_AB_offset,
                                 audio_dir,
                                 save_answer = TRUE,
                                 admin_ui = NULL){
  label <- sprintf("po%d%s", page_no, sub_id)
  stopifnot(purrr::is_scalar_character(label))
  audio_url <- get_audio_url(audio_dir, page_no, sub_id)
  correct_answers <- get_item(page_no, sub_id, "correct_answer")
  #messagef("Page no: %d, sub id = %s, correct: %s", page_no, sub_id, correct_answers)
  prompt <- shiny::div(get_page_counter(page_no, num_pages, test_AB_offset),
                       shiny::div(
                         psychTestR::i18n(sprintf("THLT_%04d_PROMPT", page_no)),
                         label,
                         style  = HALT_standard_style),
                       shiny::p(""))
  on_complete <- function(answer, state, ...) {
    correct_answers <- as.numeric(strsplit(correct_answers, ",") %>% unlist())
    raw_answer <- as.numeric(answer)
    correct <- as.integer(raw_answer %in% correct_answers)
    counter <- psychTestR::get_local(key = sprintf("%s_counter", substr(label, 1, 3)), state = state)
    num_correct <- psychTestR::get_local(key =  sprintf("%s_num_correct", substr(label, 1, 3)), state = state)
    psychTestR::set_local(key = sprintf("%s_num_correct", substr(label, 1, 3)), value = as.integer(num_correct) + correct, state = state)
    #messagef("Num_correct now %d for %d", as.integer(num_correct) + correct, page_no)
  }
  labels  <- purrr::map_chr(sprintf("THLT_%04d_CHOICES%d", page_no, 1:4L), psychTestR::i18n)
  #messagef("Created page %s (CONTINUE = %s)", label, psychTestR::i18n("CONTINUE"))
  local_audio_NAFC_page(label = label, prompt = prompt, choices = as.character(1:4), show_controls = T,
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

select_left_right <- function(direction){
  function(state, ...){
    seed <-  get_seed(state, 0L)
    set.seed(seed)
    selection <- sample(c("left", "right"), 1L)
    #messagef("Left-right-page: Selected %s for %s (Seed = %d)", selection, direction, seed )
    selection == direction
  }
}


select_AB_page <- function(page_no, sub_id){
  function(state, ...){
    seed <-  get_seed(state, page_no)
    set.seed(seed)
    selection <- sample(c("a", "a", "b", "b"))
    #messagef("Master selection: %s", paste(selection, collapse = ","))
    counter <- psychTestR::get_local(key = sprintf("po%s_counter", page_no),
                                     state = state)
    if(is.null(counter)){
      counter <- 1L
    }
    if(counter > length(selection)){
      #messagef("Counter %d too large!", counter)
      return(FALSE)
    }
    if(counter < 1L){
      #messagef("Counter %d too small!", counter)
      return(FALSE)
    }
    selection <- selection[counter]
    selection == sub_id
  }
}


get_page_counter <- function(page_no, num_pages, test_AB_offset = 0L){
  orig_page <- page_no
  if(is.null(num_pages) || is.na(num_pages) || num_pages < 1){
    return(shiny::div(""))
  }
  if(page_no == 7){
    #browser()
  }
  if(num_pages == 13L){
    if(orig_page > 7L ){
      page_no <- orig_page + 2L
    }
    else if(orig_page == 6L || orig_page == 7L){
      page_no <- orig_page + test_AB_offset
    }
    if(orig_page == 7){
      page_no <- page_no - 1
    }
  }
  if(num_pages == 17L){
    if(orig_page > 7L ){
      page_no <- orig_page + 6L
    }
    if(orig_page == 6L || orig_page == 7L){
      page_no <- 6L  + test_AB_offset + 4L*(orig_page == 7L)
    }

  }
  #messagef("Page_no: %d, num_pages: %d, offset: %d, page_no: %d", orig_page, num_pages, test_AB_offset, page_no)
  shiny::tags$p(psychTestR::i18n("PAGE_COUNTER", sub = c(page_no = page_no, num_pages = num_pages)),
                style = "text-align:center;color:#136575;font-size:10pt")
}

warning_page <- function(label, warning_message){
  psychTestR::new_timeline(
    psychTestR::one_button_page(body = psychTestR::i18n(warning_message),
                              button_text = psychTestR::i18n("AGAIN")),
    dict = HALT::HALT_dict)
}

HALT_base_page <- function(page_no, sub_id, num_pages, audio_dir, save_answer = T){
  audio_text_page(page_no, sub_id,
                  prompt = shiny::div(get_page_counter(page_no, num_pages),
                                      shiny::div(psychTestR::i18n(sprintf("THLT_%04d_PROMPT", page_no)),
                                                 style = HALT_standard_style),
                                      shiny::p("")),
                  audio_url = get_audio_url(audio_dir, page_no, sub_id),
                  correct_answer = get_item(page_no, sub_id, "correct_answer"),
                  save_answer = save_answer)
}

test_answer <- function(page_no, value, invert = F){
  function(state, ...){
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
      stop(sprintf("Counter for page_no %d not initialized", page_no))
    }
    else{
      #messagef("Counter %d, page_no %d, max_count %d", counter, page_no, max_count)
    }
    as.integer(counter) < max_count
  }
}

test_force_loop <- function(page_no, max_count, correct_answer = "correct"){
  function(state, ...){
    counter <- psychTestR::get_local(sprintf("po%d_counter", page_no), state)
    if(is.null(counter)){
      stop(sprintf("Counter for page_no %d not initialized", page_no))
    }
    else{
      #messagef("Force loop: Counter %d, page_no %d, max_count %d", counter, page_no, max_count)
    }
    answer <- psychTestR::get_local(sprintf("po%d", page_no), state)
    if(!is.null(answer)){
      #messagef("Force loop: Page_no %d, current answer: '%s'", page_no, answer)

    }
    cond1 <- is.null(answer) || !stringr::str_detect(answer, correct_answer)
    cond2 <- as.integer(counter) < max_count
    #messagef("Force loop: Cond1 %s, cond2 %s, total cond %s", cond1, cond2, cond1 && cond2)
    cond1 && cond2
  }
}


left_right_page <- function(audio_dir, right_first, num_pages){
  perm <- c(1L, 2L)
  if(right_first){
    perm <- c(2L, 1L)
  }
  img_url <- gsub("mp3", "png", get_audio_url(audio_dir = audio_dir, page_no = 4L, sub_id = ""))
  on_complete <- function(answer, state, ...){
    counter <- psychTestR::get_local(key = "po4_counter", state = state)

    psychTestR::set_local("po4", answer, state)
    answer <- format_answer(HALT_answer_format, raw_answer = as.character(answer),
                            answer  = answer,
                            correct = answer == "left")
    psychTestR::save_result(place = state, label = "po4", value = answer)
  }
  psychTestR::new_timeline(
    psychTestR::NAFC_page("po4",
                          prompt = shiny::div(get_page_counter(4L, num_pages),
                                              shiny::div(psychTestR::i18n("THLT_0004_PROMPT"),
                                                         style = HALT_standard_style),
                                              shiny::p(shiny::img(src = img_url,
                                                                  style = "width:200px;text_align:center"))),
                          choices = c("left", "right")[perm],
                          labels = c(psychTestR::i18n("THLT_0004_CHOICES1"),
                                     psychTestR::i18n("THLT_0004_CHOICES2"))[perm],
                          on_complete = on_complete,
                          arrange_vertically =  TRUE,
                          save_answer = FALSE),
  dict = HALT::HALT_dict)
}

#Creating main pages functions
page_po1 <- function(audio_dir, num_pages){
  psychTestR::new_timeline(
    volume_calibration_page(
      url = get_audio_url(audio_dir, 1L, ""),
      prompt = shiny::div(get_page_counter(1L, num_pages),
                          shiny::div(psychTestR::i18n("THLT_0001_PROMPT"),
                                     style = HALT_standard_style)),
      button_text  = psychTestR::i18n("THLT_0001_CHOICES"),
      btn_play_prompt = ""),
    dict = HALT::HALT_dict)
}

page_po4 <- function(audio_dir, max_count = 3L, num_pages){
  psychTestR::join(
    psychTestR::code_block(
      function(state, ...){
        psychTestR::set_local(key = sprintf("po%d_counter", 4L), value = 0L, state)
        #messagef("Init po%d_counter to zero", 4L)
      }
    ),
    psychTestR::while_loop(
      test = test_force_loop(page_no = 4L, max_count, correct_answer = "left"),
      logic = psychTestR::join(
        code_block(function(state,...){
          counter <- psychTestR::get_local(key = "po4_counter", state = state)
          psychTestR::set_local(key = "po4_counter", value = counter + 1L, state = state)
        }),
        psychTestR::conditional(test = select_left_right("left"),
                                logic = left_right_page(audio_dir, right_first = F, num_pages)),
        psychTestR::conditional(test = select_left_right("right"),
                                logic = left_right_page(audio_dir, right_first = T, num_pages)),
        psychTestR::conditional(test = test_answer(page_no = 4L, "right"),
                                logic = warning_page("warning_p04", "WARNING_INCORRECT"))
  )))
}

page_force_correct <- function(page_no, num_pages, max_count = 3L, audio_dir){
  warning_label <- sprintf("warning_po%d", page_no)
  psychTestR::join(
    psychTestR::code_block(
      function(state, ...){
        psychTestR::set_local(key = sprintf("po%d_counter", page_no), value = 0L, state)
        #messagef("Init po%d_counter to zero", page_no)
      }
    ),
    psychTestR::while_loop(
      test = test_force_loop(page_no, max_count),
      logic = psychTestR::join(
        psychTestR::conditional(test = test_answer(page_no, "too quiet"),
                                logic = warning_page(warning_label, "WARNING_TOO_QUIET")),
        psychTestR::conditional(test = test_answer(page_no, "imprecise"),
                                logic = warning_page(warning_label, "WARNING_IMPRECISE")),
        page_calibrate(page_no = page_no, num_pages, audio_dir = audio_dir, save_answer = T),
        code_block(function(state, ...){
          counter <- get_local(key = sprintf("po%d_counter", page_no), state)
          psychTestR::set_local(key = sprintf("po%d_counter", page_no), value = counter + 1L, state)
        })
      )
    ))
}

page_calibrate <- function(page_no, num_pages, audio_dir, save_answer = T){
  psychTestR::new_timeline(
    psychTestR::join(
      psychTestR::code_block(function(state, ...){
        psychTestR::set_local("current_selection", sample(letters[1:3], 1L), state)
      }),
      psychTestR::reactive_page(function(state, ...){
        selection <- psychTestR::get_local("current_selection", state)
        HALT_base_page(page_no, selection, num_pages, audio_dir, save_answer)
      }))
    , dict = HALT::HALT_dict)
}

page_AB_section <- function(page_no, num_pages, audio_dir){
  psychTestR::new_timeline(
    psychTestR::join(
      psychTestR::code_block(
        function(state, ...){
          psychTestR::set_local(key = sprintf("po%d_counter", page_no), value = 0L, state)
          selection <- sample(c("a", "a", "b", "b"))
          psychTestR::set_local(key = sprintf("po%d_selection", page_no), value = selection, state)
          psychTestR::set_local(key = sprintf("po%d_num_correct", page_no), value = 0L, state = state)
        }
      ),
      psychTestR::while_loop(
        test = test_counter(page_no, 4L),
        logic = psychTestR::join(
          psychTestR::reactive_page(function(state, ...){
            selection <- psychTestR::get_local(sprintf("po%d_selection", page_no), state)
            counter <- psychTestR::get_local(sprintf("po%d_counter", page_no), state)
            HALT_audio_NAFC_page(page_no, selection[counter + 1L], num_pages, counter, audio_dir = audio_dir, save_answer = T)
          }
        ),
        code_block(function(state, ...) {
          counter <- psychTestR::get_local(sprintf("po%d_counter", page_no), state)
          psychTestR::set_local(sprintf("po%d_counter", page_no), counter + 1L, state)
        })
        )))
    , dict= HALT::HALT_dict)
}

