#' Automatic configuration for the HALT module
#'
#' This functions determines the parameters of the psychTestR HALT function
#' based on predefined criteria for the specified application.
#'
#' @param baserate_hp Sets the (estimated) prevalence of headphones in the
#' target population as a number between 0 and 1. Defaults to the unbiased
#' prevalence B of 0.1767 from \insertCite{HALTpaper;textual}{HALT}.
#'
#' @param devices Sets the desired playback device. Possible settings are
#' \code{"HP"} for headphones or \code{"LS"} for loudspeakers.
#'
#' @param volume_level
#' Volume level of the HALT stimuli in loudness units to full scale (integrated).
#' Possible settings are \code{"-8.4 LUFS"} or \code{"-20.0 LUFS"}.
#' You have to adjust your other stimuli to the selected LUFS value.
#' Use \code{"-8.4 LUFS"} for highly compressed stimuli with low dynamic range.
#' Use \code{"-20.0 LUFS"} for stimuli with normal to high dynamic range.
#'
#' @param channel_check (boolean, default = T) Flag whether to include the tasks to check the channels.
#' Will be set to \code{TRUE} if \code{screening_parts = TRUE}.
#'
#' @param frequency_check (boolean, default = T) Flag whether to include the tasks to estimate the lower frequency limit of the playback device.
#'
#' @param screening_parts (boolean, default = T) Flag whether to include the headphone screening tasks.
#' If \code{TRUE} the arguments \code{channel_check}, \code{lr_img_exclude}, and \code{lr_audio_exclude} will be set to \code{TRUE}.
#' @param use_scc (boolean, default = F) Flag whether "Split, Convince, Compare" page shall be shown.
#' Will be set to \code{FALSE} if \code{screening_parts = FALSE}.
#' @param loop_exclude (integer, default = 5) Number of loops for item po2.
#' @param lr_img_exclude (boolean, default = T) Flag, if wrong answer on left-right image question shall lead to exclusion.
#' @param lr_audio_exclude (boolean, default = T) Flag, if wrong answer on left-right audio question shall lead to exclusion.
#' @param devices_exclude (boolean, default = T) Flag, if a classification other than \code{device} shall lead to exclusion.
#' @references
#' \insertRef{HALTpaper}{HALT}
#' @export
#'
auto_config <- function(volume_level = "-8.4 LUFS",
                        loop_exclude = 5L,
                        channel_check = TRUE,
                        lr_img_exclude = TRUE,
                        lr_audio_exclude = TRUE,
                        frequency_check = TRUE,
                        screening_parts = TRUE,
                        baserate_hp = 211/1194,
                        devices = "HP",
                        use_scc = FALSE,
                        devices_exclude = TRUE) {
  stopifnot(all(devices %in% c("HP","LS")),
            length(devices) <= 2,
            is.numeric(baserate_hp),
            length(baserate_hp) == 1,
            baserate_hp < 1,
            baserate_hp > 0,
            #as.integer(loop_exclude) == as.double(integer),
            volume_level %in% c("-8.4 LUFS", "-20.0 LUFS"),
            loop_exclude > 0,
            is.logical(c(screening_parts, channel_check, frequency_check))
            )
  if(length(devices) > 1){
    use_scc <- FALSE
    devices_exclude <- FALSE
  }
  if(screening_parts) {
    channel_check <- TRUE
    lr_img_exclude <- TRUE
    lr_audio_exclude <- TRUE
  } else {
    use_scc <- FALSE
  }

  tests <- HALT::test_config  %>% filter(true_ls_rate > 0.5, true_hp_rate > 0.5)

  tests$hp_pv <-
    baserate_hp * tests$true_hp_rate / (baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$false_hp_rate)
  tests$ls_pv <-
    (1 - baserate_hp) * tests$true_ls_rate / ((1 - baserate_hp) * tests$true_ls_rate + baserate_hp * tests$false_ls_rate)
  tests$utility <-
    baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$true_ls_rate
  tests <- tests %>% filter(utility == max(utility))

  if("HP" %in% devices) {
    tests <- tests %>%
      filter(hp_pv == max(hp_pv)) %>%
      filter(ls_pv == max(ls_pv))
  } else {
    tests <- tests %>%
      filter(ls_pv == max(ls_pv)) %>%
      filter(hp_pv == max(hp_pv))
  }

  volume_level <- c("-8.4 LUFS" = "loud",  "-20.0 LUFS" = "quiet")[volume_level] %>% as.vector()
  config <- tibble(volume_level = volume_level,
                   loop_exclude = loop_exclude,
                   channel_check = channel_check,
                   lr_img_exclude = lr_img_exclude,
                   lr_audio_exclude = lr_audio_exclude,
                   frequency_check = frequency_check,
                   screening_parts = screening_parts) %>%
    mutate(dplyr::select(tests, method_code:C)[1,]) %>%
    mutate(baserate_hp = baserate_hp,
           use_scc = use_scc,
           devices_exclude = devices_exclude) %>% as.list()
  config$devices <- devices
  names(config)[names(config) == "method_code"] <- "combination_method"
  names(config)[names(config) == "A"] <- "A_threshold"
  names(config)[names(config) == "B"] <- "B_threshold"
  names(config)[names(config) == "C"] <- "C_threshold"
  attr(config, "class") <- c(attr(config, "class"), "HALT_config")
  return(config)
}

#' Showing the properties of the automatic configuration
#'
#' This function prints out the properties of the automatic configuration:
#' the True Headphones Rate, the True Loudspeakers Rate, the Headphones
#' Predictive Value, the Loudspeakers Predictive Value and the Utility for
#' maximizing percent correct.
#'
#' @param config Output from \code{\link[=auto_config]{auto_config()}} or an
#' accordingly named list.
#'
#' @export
#'
show_config <- function(config = HALT::auto_config()) {
  #browser()
  stopifnot(is.list(config),
            intersect(c("combination_method", "A_threshold", "B_threshold", "C_threshold", "baserate_hp", "devices"),
                      names(config)) == c("combination_method", "A_threshold", "B_threshold", "C_threshold", "baserate_hp", "devices"),
            config$combination_method %in% 1:18,
            config$A_threshold %in% 0:6,
            config$B_threshold %in% 0:6,
            config$C_threshold %in% 0:6,
            config$baserate_hp > 0,
            config$baserate_hp < 1,
            all(config$devices %in% c("HP", "LS")))
  if(config$combination_method %in% c(1,4,5,8:18) && config$A_threshold == 0) {stop(sprintf("combination_method = %i needs A_threshold > 0!", config$combination_method))}
  if(config$combination_method %in% c(2,4,5,6,7, 10:18) && config$B_threshold == 0) {stop(sprintf("combination_method = %i needs B_threshold > 0!", config$combination_method))}
  if(config$combination_method %in% c(3,6,7,8:18) && config$C_threshold == 0) {stop(sprintf("combination_method = %i needs C_threshold > 0!", config$combination_method))}

  # set unused test's thresholds to 0
  if(config$combination_method %in% c(2,3,6,7)){config$A_threshold <- 0}
  if(config$combination_method %in% c(1,3,8,9)){config$B_threshold <- 0}
  if(config$combination_method %in% c(1,2,4,5)){config$C_threshold <- 0}

  if("HP" %in% config$devices) {
    br <- config$baserate_hp
    devices <- "headphones"
  } else {
    br <- 1 - config$baserate_hp
    devices <- "loudspeakers"
  }
  #test_config <- HALT::test_config
  combi <- HALT::test_config[HALT::test_config$method_code == config$combination_method &
                               HALT::test_config$B == config$B_threshold &
                               HALT::test_config$A == config$A_threshold &
                               HALT::test_config$C == config$C_threshold,]
  #browser()
  combi$HP_PV <- config$baserate_hp * combi$true_hp_rate / (config$baserate_hp * combi$true_hp_rate + (1 - config$baserate_hp) * combi$false_hp_rate)
  combi$LS_PV <- (1 - config$baserate_hp) * combi$true_ls_rate / ((1 - config$baserate_hp) * combi$true_ls_rate + config$baserate_hp * combi$false_ls_rate)
  combi$Utility <- config$baserate_hp * combi$true_hp_rate + (1 - config$baserate_hp) * combi$true_ls_rate
  names(combi) <- c("Method",
                    "Method Code",
                    "Test A Treshold",
                    "Test B Treshold",
                    "Test C Treshold",
                    "True HP Rate",
                    "True LS Rate",
                    "False LS Rate",
                    "False HP Rate",
                    "Logical Expression",
                    "HP Predictive Value",
                    "LS Predictive Value",
                    "Utility max % correct")
  #row.names(combi) <- c("")

  cat(sprintf("If the desired devices are %s with a prevalence of %s, the method '%s' (code %i) with thresholds %i, %i, and %i for test A, B, and C has the following properties",
              devices,
              round(br, digits = 4),
              combi$Method[1],
              combi$`Method Code`[1],
              combi$`Test A Treshold`[1],
              combi$`Test B Treshold`[1],
              combi$`Test C Treshold`[1]),
      "\n\n")
  tmp <- combi[, 6:13]  %>% t()
  print(data.frame(Value = tmp))
  #stats::printCoefmat(subset(combi, select = -c(1:5)))
}
#' Create a configuration object for the HALT module
#'
#' This function creates a configuration list for the psychTestR HALT function.
#'
#' @param combination_method Number (1 to 18) corresponding to the test method.
#' @param A_threshold (scalar integer) Threshold for Test A (1 to 6).
#' @param B_threshold (scalar integer) Threshold for Test B (1 to 6).
#' @param C_threshold (scalar integer) Threshold for Test C (1 to 6).
#' @inheritParams auto_config
#' @references
#' \insertRef{HALTpaper}{HALT}
#'
#' @export
make_config <- function(volume_level = "-8.4 LUFS",
                        loop_exclude = 5L,
                        channel_check = TRUE,
                        lr_img_exclude = TRUE,
                        lr_audio_exclude = TRUE,
                        frequency_check = TRUE,
                        screening_parts = TRUE,
                        combination_method,
                        A_threshold,
                        B_threshold,
                        C_threshold,
                        baserate_hp,
                        devices,
                        use_scc = FALSE,
                        devices_exclude = TRUE) {
  stopifnot(is.logical(c(screening_parts, channel_check, frequency_check)),
            volume_level %in% c("-8.4 LUFS", "-20.0 LUFS"),
            loop_exclude > 0)
  if (screening_parts) {
    channel_check <- TRUE
    lr_img_exclude <- TRUE
    lr_audio_exclude <- TRUE
    stopifnot(combination_method %in% 1:18,
              all(c(A_threshold, B_threshold, C_threshold) %in% 0:6),
              baserate_hp < 1,
              baserate_hp > 0,
              all(devices %in% c("HP", "LS")))
    if(combination_method %in% c(1,4,5,8:18) && A_threshold == 0){stop(sprintf("combination_method = %i needs A_threshold > 0!", combination_method))}
    if(combination_method %in% c(2,4,5,6,7, 10:18) && B_threshold == 0){stop(sprintf("combination_method = %i needs B_threshold > 0!", combination_method))}
    if(combination_method %in% c(3,6,7,8:18) && C_threshold == 0){stop(sprintf("combination_method = %i needs C_threshold > 0!", combination_method))}
    # set unused test's thresholds to 0
    if(combination_method %in% c(2,3,6,7)){A_threshold <- 0}
    if(combination_method %in% c(1,3,8,9)){B_threshold <- 0}
    if(combination_method %in% c(1,2,4,5)){C_threshold <- 0}
    if(length(devices) > 1){
      use_scc <- FALSE
      device_exclude <- FALSE
    }
  } else {
    use_scc <- FALSE
    stopifnot(is.logical(c(lr_img_exclude, lr_audio_exclude)))
    A_threshold <- 6
    B_threshold <- 6
    C_threshold <- 6
    combination_method <- 12
    devices <- c("HP", "LS")
    baserate_hp <- 211/1194
  }

  config <- tibble(volume_level = volume_level,
                   loop_exclude = loop_exclude,
                   channel_check = channel_check,
                   lr_img_exclude = lr_img_exclude,
                   lr_audio_exclude = lr_audio_exclude,
                   frequency_check = frequency_check,
                   screening_parts = screening_parts,
                   "combination_method" = combination_method,
                   "A_threshold" = A_threshold,
                   "B_threshold" = B_threshold,
                   "C_threshold" = C_threshold,
                   "baserate_hp" = baserate_hp,
                   use_scc = use_scc,
                   devices_exclude = devices_exclude) %>% as.list()
  config$devices <- devices
  attr(config, "class") <- c(attr(config, "class"), "HALT_config")

  return(config)
}
#' Export config as csv
#'
#' This function exports a HALT config object as csv file.
#'
#' @param config object of class HALT_config
#'
#' @param file character string naming a file open for writing.
#'
#' @export
export_config <- function(config,
                          file = "") {
  stopifnot(is(config, "HALT_config"))
  attr(config, "class") <- "list"
  if(all(config$devices == c("HP", "LS") || config$devices == c("LS", "HP"))) {
    config$devices <- "HP,LS"
  }
  write.table(as.data.frame(config),
            file = file,
            sep = ";",
            row.names = FALSE,
            quote = FALSE)
}
#' Import a csv config
#'
#' This function imports a csv config file as HALT config object.
#'
#' @param file character string naming the csv file
#'
#' @export
import_config <- function(file) {
  if(!is.scalar.character(file)) {
    stop("'file' has to be a character scalar!")
  } else {
    if(!file.exists(file)) {
      stop(sprintf("The file '%s' does not exist!", file))
    }
  }
  configimport <- read.csv2(file = file)
  configimport <- as.list(configimport)
  if(configimport$devices == "HP,LS" || configimport$devices == "LS,HP") {
    configimport$devices <- c("HP", "LS")
  }
  make_config(volume_level = configimport$volume_level,
              loop_exclude = configimport$loop_exclude,
              channel_check = configimport$channel_check,
              lr_img_exclude = configimport$lr_img_exclude,
              lr_audio_exclude = configimport$lr_audio_exclude,
              frequency_check = configimport$frequency_check,
              screening_parts = configimport$screening_parts,
              combination_method = configimport$combination_method,
              A_threshold = configimport$A_threshold,
              B_threshold = configimport$B_threshold,
              C_threshold = configimport$C_threshold,
              baserate_hp = configimport$baserate_hp,
              use_scc = configimport$use_scc,
              devices_exclude = configimport$devices_exclude,
              devices = configimport$devices)
}
