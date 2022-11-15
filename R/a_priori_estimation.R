#' A priori estimaion
#'
#' This function provides estimations for the sample size required for the
#' event that at least a minimum number of participants used the desired
#' devices to have a specified minimum probability. By default, the function
#' returns the screening procedure that requires the smallest sample size for
#' this event. To return screening procedures with similar required sample
#' sizes use the parameter \code{tolerance}.
#'
#' @section Model:
#' The function uses the Normal approximation of the Binomial distribution with
#' continuity correction. For details, see \insertCite{HALT_2;textual}{HALT}.
#'
#' @return A tibble (data frame) with the characteristics of the test
#' procedure(s) and the attribute \code{explanation}.
#' This attribute is intended as an explanatory text containing a
#' probabilistic statement for the test procedure requiring the smallest sample
#' size.
#'
#' @inheritParams post_hoc_estimation
#'
#' @inheritParams auto_config
#'
#'
#'
#'
#' @references
#' \insertAllCited{}
#'
#' @export
a_priori_estimation <- function(screening_strat,
                                devices = "HP",
                                baserate_hp = 211/1194,
                                switch_to_target = NA,
                                min_number,
                                min_prob = 0.8,
                                tolerance = as.integer(0)) {
  stopifnot(screening_strat %in% names(screening_strategies()),
            baserate_hp < 1,
            baserate_hp > 0,
            devices %in% c("HP","LS"),
            min_number > 0,
            as.integer(min_number) == as.double(min_number),
            min_prob >= 0.6,
            min_prob < 1,
            tolerance >= 0,
            as.integer(tolerance)==as.double(tolerance))
  if (screening_strat == "scc") {
    if(is.na(switch_to_target) | switch_to_target < 0 | switch_to_target >= 1) {
      stop('For screening_strat = "scc" you have to specify switch_to_target between 0 and 1!')
    }
    tests <- tests_scc_utility(baserate_hp = baserate_hp,
                               devices = devices,
                               switch_to_target = switch_to_target)
    p <- tests$prob_scc_target
  } else {
    tests <- tests_pv_utility(baserate_hp = baserate_hp)
    if (devices == "HP") {
      p <- tests$hp_pv
    } else {
      p <- tests$ls_pv
    }
  }

  q <- (qnorm(p = 1 - min_prob))^2
  a <- ((-1/p) * (2*min_number - 1 + (1 - p)*q))
  b <- ((min_number - .5) / p)^2

  tests$samplesize <- ceiling(
    - .5*a + sqrt((.5*a)^2 - b)
  )

  if (screening_strat == "scc") {
    if (devices == "HP") {
      tests$expectation_total_participants <- ceiling(
        tests$samplesize /
          (baserate_hp + (1 - baserate_hp) *
             (switch_to_target * tests$true_hp_rate + (1 - switch_to_target) * tests$false_hp_rate)
          )
      )
    } else {
      tests$expectation_total_participants <- ceiling(
        tests$samplesize /
          ((1 - baserate_hp) + baserate_hp *
             ((1 - switch_to_target) * tests$false_ls_rate + switch_to_target * tests$true_ls_rate)
          )
      )
    }
  } else {
    if (devices == "HP") {
      tests$expectation_total_participants <- ceiling(
        tests$samplesize / (tests$true_hp_rate * baserate_hp + (1 - baserate_hp) * tests$false_hp_rate)
      )
    } else {
      tests$expectation_total_participants <- ceiling(
        tests$samplesize / (tests$false_ls_rate * baserate_hp + (1 - baserate_hp) * tests$true_ls_rate)
      )
    }
  }

  tests$min_quality_percent <- 100*min_number/tests$samplesize
  tests <- tests %>%
    dplyr::select(!c(false_hp_rate, false_ls_rate, logic_expr)) %>%
    filter(samplesize <= min(samplesize) + tolerance) %>%
    dplyr::arrange(samplesize, expectation_total_participants, min_quality_percent)

  attr(tests, "explanation") <-
    a_priori_explanation(screening_strat = screening_strat,
                         devices = devices,
                         baserate_hp = baserate_hp,
                         switch_to_target = switch_to_target,
                         min_number = min_number,
                         min_prob = min_prob,
                         test_method = tests[1,])
  tests
}

#' A priori explanation
#'
#' This function provides an explanatory text for an a priori estimation.
#' It is used inside \code{\link{a_priori_estimation}}.
#'
#' @export
a_priori_explanation <- function(screening_strat,
                                 devices,
                                 baserate_hp = 211/1194,
                                 switch_to_target = NA,
                                 min_number,
                                 min_prob = 0.8,
                                 test_method) {
  devices_str <- ifelse(devices == "HP", "headphones", "loudspeakres")
  explanation <- paste0(
    sprintf("When test combination '%s' (evaluation key %i) with thresholds %i, %i, and %i for tests A, B, and C, respectively, is used within screening strategy '%s'",
            test_method$method[[1]], test_method$method_code[[1]], test_method$A[[1]], test_method$B[[1]], test_method$C[[1]], toupper(screening_strat)),
    ifelse(screening_strat == "scc", ",", " and"),
    sprintf(" the prevalence for headphones is assumed to be %.4f",
            baserate_hp),
    ifelse(screening_strat == "scc",
           sprintf(" and the switching prevalence is assumed to be %.4f ",
                   switch_to_target), " "),
    sprintf("in your target population, a sample of %i participants",
            test_method$samplesize[[1]]),
    ifelse(screening_strat == "scc",
           sprintf(" who reported the use of %s or ",
                   devices_str),
           " "),
    sprintf("whose tests indicate the use of %s is required to have a probability of at least %.4f that %i participants actually used %s. ",
            devices_str, min_prob, min_number, devices_str),
    sprintf("The percentage of correctly identified target playback devices ('quality') of such a sample would then be at least %.1f %% with a probability of %.4f",
            test_method$min_quality_percent, min_prob)
  )
  explanation
}
