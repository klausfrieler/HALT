#' Automatic configuration for the HALT module
#'
#' This functions determines the parameters of the psychTestR HALT function
#' based on predefined criteria for the specified application.
#'
#' @param baserate_hp Sets the (estimated) prevalence of headphones in the
#' target population as a number between 0 and 1. Defaults to the unbiased
#' prevalence B of 0.1767.
#' from Wycisk et al.(2020).
#'
#' @param devices Sets the desired playback device. Possible settings are
#' \code{"HP"} for headphones or \code{"LS"} for loudspeakers.
#'
#' @param use_scc (boolean, default = F) Flag wether Splice, Convice. Compare page shall be shown.
#' @references
#' \insertRef{HALTpaper}{HALT}
#' @export
#'
auto_config <- function(baserate_hp = 211/1194,
                        devices = "HP",
                        use_scc = F) {
  stopifnot(devices %in% c("HP", "LS"),
            length(devices) == 1,
            is.numeric(baserate_hp),
            length(baserate_hp) == 1,
            baserate_hp < 1,
            baserate_hp > 0)

  tests <- HALT::test_config  %>% filter(true_ls_rate > 0.5, true_hp_rate > 0.5)

  tests$hp_prevalence <-
    baserate_hp * tests$true_hp_rate / (baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$false_hp_rate)
  tests$ls_prevalence <-
    (1 - baserate_hp) * tests$true_ls_rate / ((1 - baserate_hp) * tests$true_ls_rate + baserate_hp * tests$false_ls_rate)
  tests$utility <-
    baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$true_ls_rate
  tests <- tests %>% filter(utility == max(utility))

  if(devices == "HP") {
    tests <- tests %>%
      filter(tests$hp_prevalence == max(tests$hp_prevalence)) %>%
      filter(tests$ls_prevalence == max(tests$ls_prevalence))
  } else {
    tests <- tests %>%
      filter(tests$ls_prevalence == max(tests$ls_prevalence)) %>%
      filter(tests$hp_prevalence == max(tests$hp_prevalence))
  }

  #method <- tests[1,2]

  #A <- tests[1,4] # Bilsen
  #B <- tests[1,3] # Franssen
  #C <- tests[1,5] # Woods
  #config <- list("method" = method, "A" = A, "B" = B, "C" = C, "baserate_hp" = baserate_hp, "devices" = devices)
  config <- tests[1,] %>% dplyr::select(method = method_code,
                                 A = A,
                                 B = B,
                                 C = C) %>%
    mutate(baserate_hp = baserate_hp, devices = devices, use_scc = use_scc)
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
#' @param config Output from \link{HALT}{\code{auto_config}} or an accordingly named
#' list.
#'
#' @export
#'
show_config <- function(config = auto_config()) {
  stopifnot(is.list(config),
            intersect(c("method", "A", "B", "C", "baserate_hp", "devices"), names(config)) == c("method", "A", "B", "C", "baserate_hp", "devices"),
            config$method %in% 1:18,
            config[2:4] %in% 1:6,
            config$baserate_hp > 0,
            config$baserate_hp < 1,
            config$devices %in% c("HP", "LS"))
  if(config$devices == "HP") {
    br <- config$baserate_hp
    devices <- "headphones"
  } else {
    br <- 1 - config$baserate_hp
    devices <- "loudspeakers"
  }
  test_config <- as.data.frame(HALT::test_config)
  combi <- HALT::test_config[HALT::test_config$method_code == config$method & HALT::test_config$B == config$B & HALT::test_config$A == config$A & HALT::test_config$C == config$C,]
  combi$HP_PV <- config$baserate_hp * combi$true_hp_rate / (config$baserate_hp * combi$true_hp_rate + (1 - config$baserate_hp) * combi$false_hp_rate)
  combi$LS_PV <- (1 - config$baserate_hp) * combi$true_ls_rate / ((1 - config$baserate_hp) * combi$true_ls_rate + config$baserate_hp * combi$false_ls_rate)
  combi$Utility <- config$baserate_hp * combi$true_hp_rate + (1 - config$baserate_hp) * combi$true_ls_rate
  names(combi) <- c(names(HALT::test_config[1:5]),
                    "True HP Rate",
                    "True LS Rate",
                    "False LS Rate",
                    "False HP Rate",
                    "HP Predictive Value",
                    "LS Predictive Value",
                    "Utility max % correct")
  row.names(combi) <- c("")
  cat(sprintf("If the desired devices are %s with a prevalence of %s, the method '%s' (code %i) with thresholds %i, %i, and %i for test A, B, and C has the following properties",
              devices,
              round(br, digits = 4),
              combi$method[1],
              combi$method_code[1],
              combi$A[1],
              combi$B[1],
              combi$C[1]),
      "\n\n")
  print(combi)
  #stats::printCoefmat(subset(combi, select = -c(1:5)))
}
#' Create a configuration object for the HALT module
#'
#' This function creates a configuration list for the psychTestR HALT function.
#'
#' @param method Number (1 to 18) corresponding to the test method.
#' @param A Threshold for Test A (1 to 6).
#' @param B Threshold for Test B (1 to 6).
#' @param C Threshold for Test C (1 to 6).
#' @inheritParams auto_config
#' @references
#' \insertRef{HALTpaper}{HALT}
#'
#' @export
make_config <- function(method, A, B, C, baserate_hp, devices, use_scc = F) {
  stopifnot(method %in% 1:18,
            c(A,B,C) %in% 1:6,
            baserate_hp < 1,
            baserate_hp > 0,
            all(devices %in% c("HP","LS")))
  config <- tibble("method" = method, "A" = A, "B" = B, "C" = C, "baserate_hp" = baserate_hp, "devices" = devices, use_scc = use_scc)
  attr(config, "class") <- c(attr(config, "class"), "HALT_config")

  return(config)
}
