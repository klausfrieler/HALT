
#' This function reads a directory of results files and extracts a tibble of HALT data
#'
#' @param result_dir (character scalar) Result dir to parse
#' @export
#'
extract_HALT_items <- function(result_dir){
  purrr::map_dfr(list.files(result_dir, pattern = "*.rds", full.names = T), function(res_name){
    res <- readRDS(res_name)
    if("HALT" %in% names(res)){
      dplyr::bind_cols(res[["session"]][c("p_id", "time_started")],
                purrr::map_dfr(res[["HALT"]], function(x) x %>% mutate(raw_answer = as.character(raw_answer))))
    }
  })
}

