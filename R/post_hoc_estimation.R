#' Post hoc estimations
#'
#' This function provides probabilistic statements about the composition of a
#' sample after application of a certain test procedure within a screening
#' strategy. For this purpose a Binomial distribution is used.
#'
#' The event that at least \code{k} participants in the final sample used the
#' target device is considered. The function determines either the probability
#' of this event or for a given probability the number \code{k}.
#'
#' Depending on the screening strategy either \code{samplesize} and
#' \code{baserate_hp} (for \code{"fwr"} or \code{"far"}) or
#' \code{target_selfreported}, \code{target_tested}, and
#' \code{switch_to_target} (for \code{"scc"}) are needed.
#'
#' For details, see \insertCite{HALT_2;textual}{HALT}.
#'
#' @param screening_strat three-letter lower-case abbreviation of the screening
#' strategy. See \code{\link{screening_strategies}}
#'
#' @inheritParams make_config
#'
#' @inheritParams tests_scc_utility
#'
#' @param samplesize Number of participants who got a test result indicating
#' the use of the target device and who are therefore in the final sample.
#' \code{samplesize} is needed when \code{screening_strat} is \code{"fwr"} or
#' \code{"far"}.
#'
#' @param target_selfreported Number of participants who reported the use of
#' the target device. \code{target_selfreported} is needed when
#' \code{screening_strat} is \code{"scc"}.
#'
#' @param target_tested Number of participants who reported the use of a
#' playback device other than the target device and got a test result
#' indicating the use of the target device. \code{target_tested} is needed when
#' \code{screening_strat} is \code{"scc"}.
#'
#' @param min_number minimum number of participants \code{k} who are in the
#' final sample and should have used the correct device.
#' When you use \code{min_number} you cannot use \code{min_prob}, i.e.
#' \code{min_prob = NULL}.
#'
#' @param min_prob (greater than 0, less than 1) for the event that
#' at least an unknown number of participants \code{k} who are in the final
#' sample used the correct device.
#' When you use \code{min_prob} you cannot use \code{min_number}, i.e.
#' \code{min_number = NULL}.
#'
#' @note Only one of the arguments \code{min_number} and \code{min_prob} can be
#' used.
#'
#' @references
#' \insertAllCited{}
#'
#' @export
post_hoc_estimation <- function(screening_strat,
                                combination_method,
                                A_threshold,
                                B_threshold,
                                C_threshold,
                                baserate_hp = NA,
                                devices,
                                samplesize = NA,
                                target_selfreported = NA,
                                target_tested = NA,
                                switch_to_target = NA,
                                min_number = NULL,
                                min_prob = NULL) {
  if(screening_strat == "scc") {
    if (is.na(target_selfreported) | is.na(target_tested) | is.na(switch_to_target)) {
      stop("For SCC you have to specify target_selfreported, target_tested, and switch_to_target!")
    }
    if (target_selfreported < 0 | as.integer(target_selfreported) != target_selfreported |
        target_tested <= 0 | as.integer(target_tested) != target_tested) {
      stop("target_selfreported and target_tested have to be natural numbers (positive integers)!")
    }
    if (switch_to_target <= 0 | switch_to_target >= 1) {
      stop("switch_to_target has to be between 0 and 1!")
    }
  } else {
    if(is.null(samplesize)) {
      stop("You have to specify samplerate for this screening strategy!")
    }
    if(samplesize <= 0 | as.integer(samplesize) != samplesize) {
      stop("samplesize has to be a natural number (positive integer)!")
    }
    if(baserate_hp <= 0 | baserate_hp >= 1) {
      stop("baserate_hp has to be between 0 and 1!")
    }
  }
  if(!xor(is.null(min_number), is.null(min_prob))) {
    stop("You have to specify exactly one of the arguments min_number and min_prob!")
  }
  config <- make_config(combination_method = combination_method,
                        A_threshold = A_threshold,
                        B_threshold = B_threshold,
                        C_threshold = C_threshold,
                        baserate_hp = ifelse(screening_strat == "scc",
                                             switch_to_target, baserate_hp),
                        devices = devices,
                        use_scc = (screening_strat == "scc"))
  if (is.null(min_prob)) {
    min_prob <- post_hoc_calc_min_prob(screening_strat = screening_strat,
                                       config = config,
                                       sample_size = samplesize,
                                       target_selfreported = target_selfreported,
                                       target_tested = target_tested,
                                       switch_to_target = switch_to_target,
                                       min_number = min_number)
  } else {
    min_number <- post_hoc_calc_min_number(screening_strat = screening_strat,
                                           config = config,
                                           sample_size = samplesize,
                                           target_selfreported = target_selfreported,
                                           target_tested = target_tested,
                                           switch_to_target = switch_to_target,
                                           min_prob = min_prob)
  }
  est <- post_hoc_tibble(screening_strat = screening_strat,
                         combination_method = combination_method,
                         A = config$A_threshold,
                         B = config$B_threshold,
                         C = config$C_threshold,
                         baserate_hp = baserate_hp,
                         target_device = devices,
                         switch_to_target = switch_to_target,
                         sample_size = samplesize,
                         target_selfreported = target_selfreported,
                         target_tested = target_tested,
                         min_number = min_number,
                         min_prob = min_prob)
  attr(est, "explanation") <-
    post_hoc_explanation(screening_strat = screening_strat,
                         combination_method = combination_method,
                         A = config$A_threshold,
                         B = config$B_threshold,
                         C = config$C_threshold,
                         devices = devices,
                         baserate_hp = baserate_hp,
                         min_number = min_number,
                         min_prob = min_prob,
                         sample_size = samplesize,
                         target_selfreported = target_selfreported,
                         target_tested = target_tested,
                         switch_to_target = switch_to_target,
                         min_data_qual_perc = est$min_data_qual_perc[[1]])
  est
}

# Look up screening procedure and calculate min_number xor min_prob
post_hoc_proc <- function(screening_strat,
                          config,
                          baserate_hp = NA,
                          switch_to_target = NA) {
  if (screening_strat == "scc") {
    procedure <- tests_scc_utility(switch_to_target = switch_to_target,
                                   devices = config$devices)
  } else {
    procedure <- tests_pv_utility(baserate_hp = baserate_hp)
  }

  procedure <- procedure %>%
    filter(method_code == config$combination_method,
           A == config$A_threshold,
           B == config$B_threshold,
           C == config$C_threshold)
  procedure
}

#' @export
post_hoc_calc_min_number <- function(screening_strat,
                                     config,
                                     min_prob,
                                     sample_size = NA,
                                     target_selfreported = NA,
                                     target_tested = NA,
                                     switch_to_target = NA
) {
  procedure <- post_hoc_proc(screening_strat = screening_strat,
                             config = config,
                             baserate_hp = config$baserate_hp,
                             switch_to_target = switch_to_target)
  if (screening_strat == "scc") {
    min_number <- qbinom(p = min_prob, size = target_tested,
                         prob = ifelse(config$devices == "HP", procedure$scc_hp_pv[1],
                                       procedure$scc_ls_pv[1]),
                         lower.tail = FALSE) + target_selfreported
  } else {
    min_number <- qbinom(p = min_prob, size = sample_size,
                         prob = ifelse(config$devices == "HP", procedure$hp_pv[1],
                                       procedure$ls_pv[1]),
                         lower.tail = FALSE)
  }
  min_number
}

#' @export
post_hoc_calc_min_prob <- function(screening_strat,
                                   config,
                                   min_number,
                                   sample_size = NA,
                                   target_selfreported = NA,
                                   target_tested = NA,
                                   switch_to_target = NA
) {
  procedure <- post_hoc_proc(screening_strat = screening_strat,
                             config = config,
                             baserate_hp = config$baserate_hp,
                             switch_to_target = switch_to_target)
  if (screening_strat == "scc") {
    k <- min_number - target_selfreported
    k <- ifelse(k < 0, 0, k)
    min_prob <- sum(dbinom(k:target_tested, size = target_tested,
                           prob = ifelse(config$devices == "HP", procedure$scc_hp_pv[1],
                                         procedure$scc_ls_pv[1])))
  } else {
    min_prob <-
      sum(dbinom(min_number:sample_size,
                 size = sample_size,
                 prob = ifelse(config$devices == "HP", procedure$hp_pv[1],
                               procedure$ls_pv[1])
      )
      )
  }
  min_prob
}

#' Construct table for post hoc estimation
#'
#' This function constructs a tibble with a screening test configuration
#' including screening strategy and the results of a post hoc estimation.
#'
#' @export
post_hoc_tibble <- function(screening_strat,
                            combination_method,
                            A, B, C,
                            target_device,
                            sample_size = NA,
                            target_selfreported = NA,
                            target_tested = NA,
                            baserate_hp,
                            switch_to_target = NA,
                            min_number,
                            min_prob
) {
  min_data_qual_perc <-
    ifelse(screening_strat == "scc",
           min_number / (target_selfreported + target_tested),
           min_number / sample_size) * 100

  if (screening_strat == "scc") {
    sample_size <- target_selfreported + target_tested
    baserate_hp <- NA
  } else {
    target_selfreported <- NA
    target_tested <- NA
    switch_to_target <- NA
  }
  est <- tibble(screening_strat = toupper(screening_strat),
                combination_method = combination_method,
                A = A,
                B = B,
                C = C,
                baserate_hp = baserate_hp,
                target_device = target_device,
                switch_to_target = switch_to_target,
                target_selfreported = target_selfreported,
                target_tested = target_tested,
                sample_size = sample_size,
                min_number = min_number,
                min_prob = min_prob,
                min_data_qual_perc = min_data_qual_perc) %>%
    dplyr::select(where(~!all(is.na(.x))))
  est
}

#' Generate explanation text for Post hoc estimation
#'
#' This functions provides an explanatory text for a post hoc estimation.
#'
#' @export
post_hoc_explanation <- function(screening_strat,
                                 combination_method,
                                 A, B, C,
                                 devices,
                                 baserate_hp,
                                 min_number,
                                 min_prob,
                                 sample_size = NA,
                                 target_selfreported = NA,
                                 target_tested = NA,
                                 switch_to_target = NA,
                                 min_data_qual_perc) {
  combi <- evaluation_keys()[combination_method]
  devices <- ifelse(devices == "HP", "headphones", "loudspeakers")
  explanation <-
    sprintf("You used test combination '%s' (evaluation key %i) with thresholds %i, %i, and %i for Test A, Test B, and Test C, respectively, within screening strategy %s and %s as target devices.",
            combi, combination_method, A, B, C, toupper(screening_strat), devices)
  if (screening_strat != "scc") {
    explanation <- c(
      explanation,
      sprintf("Your final sample consists of %i participants with a test result indicating the use of %s.",
              sample_size, devices),
      sprintf("For the given test combination, this sample and a prevalence for headphones of %.4f, the probability that at least %i participants used %s is at least %.4f according to a Binomial model.",
              baserate_hp, min_number, devices, min_prob)
    )
  } else {
    explanation <- c(
      explanation,
      sprintf("When %i participant indicated the use of %s and %i did not but their test result was %s your total sample size is %i.",
              target_selfreported, devices, target_tested, devices, target_selfreported + target_tested),
      sprintf("For the given test combination, this sample, a prevalence for headphones of %.4f and a switching prevalence of %.4f, the probability that at least %i participants used %s is %.4f according to a Binomial model and the assumption of an unbiased self-report.",
              baserate_hp, switch_to_target, min_number, devices, min_prob)
    )
  }
  explanation <- c(
    explanation,
    sprintf("The percentage of correct identified target playback devices ('quality') for this setting is at least %.1f percent with a probability of at least %.4f.",
            min_data_qual_perc, min_prob)
  )
  explanation
}
