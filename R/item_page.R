
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
  show_responses = "document.getElementById('response_ui').style.visibility = 'inherit';"
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
  stopifnot(purrr::is_scalar_character(label))
  audio_ui <- get_audio_ui(audio_url, wait = T, loop = F, width = 200)

  style <- NULL
  ui <- shiny::div(
    tagify(prompt),
    audio_ui,
    psychTestR::text_input_page(label = label,
                                prompt = prompt,
                                one_line = T,
                                button_text = psychTestR::i18n("CONTINUE"),
                                validate = function(x) !is.na(as.numeric(x)) & as.integer(x) == as.numeric(x),
                                save_answer = save_answer,
                                admin_ui= admin_ui))

  get_answer <- function(input, ...) {
    answer <- as.numeric(gsub("answer", "", input$last_btn_pressed))
    correct <- answer %in% correct_answer
    too_quiet <-
    tibble(answer = answer,
         label = label,
         correct = correct)

  }
  validate <- function(answer, ...) !is.null(answer)
  #printf("[audio_NAFC_page_with_img] left")
  psychTestR::page(ui = ui, label = label,  get_answer = get_answer, save_answer = save_answer,
                   validate = validate, on_complete = on_complete, final = FALSE,
                   admin_ui = admin_ui)
}

HALT_item_simple <- function(label = "",
                             correct_answer,
                             prompt = "",
                             audio_file,
                             audio_dir = "",
                             save_answer = TRUE,
                             on_complete = NULL
                     ){

  page_prompt <- shiny::div(prompt)
  audio_url <- file.path(audio_dir, audio_file)
  audio_text_page(label = label,
                  prompt = page_prompt,
                  audio_url = audio_url,
                  correct_answer = correct_answer,
                  save_answer = save_answer,
                  on_complete = on_complete)
}

