#' Maximum Headphones Predictive Value
#'
#' This function returns a data.frame containing all tests with maximum
#' headphones predictive value for the specified prevalence of headphones.
#'
#' @param baserate_hp Sets the (estimated) prevalence of headphones in the
#' target population as a number between 0 and 1. Defaults to the unbiased
#' prevalence B of 0.1767 from \insertCite{HALTPaper;textual}{HALT}.
#'
#' @references
#' \insertRef{HALTpaper}{HALT}
#'
#' @export
max_hp_pv <- function(baserate_hp = 211/1194) {
  stopifnot(baserate_hp < 1,
            baserate_hp > 0)
  tests <- HALT::test_config
  tests$hp_pv <-
    baserate_hp * tests$true_hp_rate / (baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$false_hp_rate)
  tests$ls_pv <-
    (1 - baserate_hp) * tests$true_ls_rate / ((1 - baserate_hp) * tests$true_ls_rate + baserate_hp * tests$false_ls_rate)
  tests$utility <-
    baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$true_ls_rate

  tests <- tests %>% filter(hp_pv == max(hp_pv)) %>% dplyr::select(method, method_code, A, B, C, true_hp_rate, true_ls_rate, hp_pv, ls_pv, utility)
  tests
}
#' Maximum Loudspeakers Predictive Value
#'
#' This function returns a data.frame containing all tests with maximum
#' loudspeakers predictive value for the specified prevalence of headphones.
#'
#' @inheritParams max_hp_pv
#'
#' @references
#' \insertRef{HALTpaper}{HALT}
#'
#' @export
max_ls_pv <- function(baserate_hp = 211/1194) {
  stopifnot(baserate_hp < 1,
            baserate_hp > 0)
  tests <- HALT::test_config
  tests$hp_pv <-
    baserate_hp * tests$true_hp_rate / (baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$false_hp_rate)
  tests$ls_pv <-
    (1 - baserate_hp) * tests$true_ls_rate / ((1 - baserate_hp) * tests$true_ls_rate + baserate_hp * tests$false_ls_rate)
  tests$utility <-
    baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$true_ls_rate

  tests <- tests %>% filter(ls_pv == max(ls_pv)) %>% dplyr::select(method, method_code, A, B, C, true_hp_rate, true_ls_rate, hp_pv, ls_pv, utility)
  tests
}
#' Maximum Utility maximizing percent correct
#'
#' This function returns a data.frame containing all tests with maximum value
#' for utility maximizing percent correct for the specified prevalence of
#' headphones.
#'
#' @inheritParams max_hp_pv
#'
#' @references
#' \insertRef{HALTpaper}{HALT}
#'
#' @export
max_utility <- function(baserate_hp = 211/1194) {
  stopifnot(baserate_hp < 1,
            baserate_hp > 0)
  tests <- HALT::test_config
  tests$hp_pv <-
    baserate_hp * tests$true_hp_rate / (baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$false_hp_rate)
  tests$ls_pv <-
    (1 - baserate_hp) * tests$true_ls_rate / ((1 - baserate_hp) * tests$true_ls_rate + baserate_hp * tests$false_ls_rate)
  tests$utility <-
    baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$true_ls_rate

  tests <- tests %>% filter(utility == max(utility)) %>% dplyr::select(method, method_code, A, B, C, true_hp_rate, true_ls_rate, hp_pv, ls_pv, utility)
  tests
}
#' Test procedures with maximum properties
#'
#' This function returns a data.frame containing all tests with either
#' maximum headphones predictive value, maximum loudspeakers predictive value,
#' or maximum value for utility maximizing percent correct for the specified
#' prevalence of headphones.
#'
#' @inheritParams max_hp_pv
#'
#' @references
#' \insertRef{HALTpaper}{HALT}
#'
#' @export
max_properties <- function(baserate_hp = 211/1194) {
  stopifnot(baserate_hp < 1,
            baserate_hp > 0)
  rbind(max_hp_pv(baserate_hp = baserate_hp),
        max_ls_pv(baserate_hp = baserate_hp),
        max_utility(baserate_hp = baserate_hp))
}
#' Post hoc estimations
#'
#' This function provides probabilistic statements about the composition of a
#' sample after application of a certain test procedure. For this purpose a
#' Binomial distribution is used.
#'
#' Given a test procedure, prevalence, and sample size the event that at least
#' \code{k} participants who passed the test procedure used the correct device
#' is considered.
#' The function either calculates the minimum probability for this event for a
#' given \code{k} or \code{k} for a given probability for this event.
#'
#' @inheritParams make_config
#'
#' @param samplesize
#'
#' @param min_number minimum number of participants \code{k} who passed the
#' test procedure and used the correct device
#'
#' @param min_prob Probability (greater than 0, less than 1) for the event that
#' at least an unknown number of participants \code{k} who passed the test
#' procedure used the correct device
#'
#' @note Only one of the arguments \code{min_number} and \code{min_prob} can be
#' used.
#'
#' @references
#' \insertRef{HALTpaper}{HALT}
#'
#' @export
post_hoc_est <- function(combination_method, A, B, C, baserate_hp, devices, samplesize, min_number = NULL, min_prob = NULL) {
  stopifnot(combination_method %in% 1:18,
            all(c(A,B,C) %in% 1:6),
            baserate_hp < 1,
            baserate_hp > 0,
            devices %in% c("HP","LS"),
            is.numeric(samplesize),
            as.integer(samplesize) == as.double(samplesize),
            ((min_number <= samplesize & min_number >= 0) & is.null(min_prob)) || (is.null(min_number) & (min_prob <= 1 & min_prob >= 0))
  )

  config <- HALT::make_config(combination_method = combination_method, A = A, B = B, C = C, baserate_hp = baserate_hp, devices = devices, use_scc = F)
  if (combination_method < 10) {
    if (combination_method %in% c(2,3,6,7)) {
      A <- 0
      config$A <- 0
    }
    if (combination_method %in% c(1,3,8,9)) {
      B <- 0
      config$B <- 0
    }
    if (combination_method %in% c(1,2,4,5)) {
      C <- 0
      config$C <- 0
    }
  }

  procedure <- HALT::test_config %>% filter(method_code == config$combination_method, A == config$A, B == config$B, C == config$C)
#    test_config[test_config$method_code == combination_method & test_config$B == B & test_config$A == A & test_config$C == C,]

  if(devices == "HP") {
    procedure$PV <- procedure$true_hp_rate * baserate_hp / (procedure$true_hp_rate * baserate_hp + (1 - baserate_hp) * procedure$false_hp_rate)
  } else {
    procedure$PV <- procedure$true_ls_rate * (1 - baserate_hp) / (procedure$false_ls_rate * baserate_hp + (1 - baserate_hp) * procedure$true_ls_rate)
  }
  if (is.null(min_prob)) {
    min_prob <- sum(dbinom(min_number:samplesize, size = samplesize, prob = procedure$PV[1]))
  } else {
    min_number <- qbinom(p = min_prob, size = samplesize, prob = procedure$PV[1], lower.tail = FALSE)
  }
  cat(sprintf("In a sample of %i participants the event that at least %i used the desired device (%s) has a probability of at least %f when the prevalence for headphones is %f and the test procedure '%s' with thresholds %i, %i, and %i for tests A, B, and C was used.",
              as.integer(samplesize), as.integer(min_number), devices, min_prob, baserate_hp, HALT::test_config$method[test_config$method_code == combination_method][1], as.integer(A), as.integer(B), as.integer(C)))
}
#' A priori estimations
#'
#' This function provides estimations for the sample size required for the
#' event that at least a minimum number of participants used the desired
#' devices to have a specified minimum probability. By default, the function
#' returns the test procedure that requires the smallest sample size for this
#' event. To return test procedures with similar required sample sizes use the
#' parameter \code{tolerance}.
#'
#' The function uses the Normal approximation of the Binomial distribution with
#' continuity correction.
#'
#' @inheritParams auto_config
#'
#' @param min_number minimum number of participants passing the test procedure
#' and using the desired devices
#'
#' @param min_prob minimum probability (equal to or greater than 0.6, less than
#' 1) for the event that at least \link{HALT}{\code{minimum_number}}
#' participants pass the test procedure and use the desired devices.
#'
#' @param tolerance (non-negative integer) defaults to \code{0}. If set to a
#' value greater than 0 the function returns the test procedures whose sample
#' sizes exceed the minimum sample size by at most this value.
#'
#' @return probabilistic statement (explanation text) and a transposed data
#' frame with the characteristics of the test procedure(s).
#'
#' @export
a_priori_est <- function(baserate_hp = 211/1194,
                         devices = "HP",
                         min_number,
                         min_prob,
                         tolerance = as.integer(0)) {
  stopifnot(baserate_hp < 1,
            baserate_hp > 0,
            devices %in% c("HP","LS"),
            is.numeric(min_number),
            as.integer(min_number) == as.double(min_number),
            min_prob >= 0.6,
            min_prob < 1,
            tolerance >= 0,
            as.integer(tolerance)==as.double(tolerance))

  tests <- HALT::test_config
  tests$hp_pv <-
    baserate_hp * tests$true_hp_rate / (baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$false_hp_rate)
  tests$ls_pv <-
    (1 - baserate_hp) * tests$true_ls_rate / ((1 - baserate_hp) * tests$true_ls_rate + baserate_hp * tests$false_ls_rate)
  tests$utility <-
    baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$true_ls_rate

  q <- (qnorm(p = 1 - min_prob))^2

  if (devices == "HP") {
    tests$samplesize <-
      ceiling(
        ((1/tests$hp_pv)*(2*min_number-1+q*(1-tests$hp_pv)))/2 + ((((1/tests$hp_pv)*(2*min_number-1+q*(1-tests$hp_pv)))^2)/4 - ((min_number-0.5)/tests$hp_pv)^2)^0.5
      )
    tests$expectation_total_participants <- ceiling(tests$samplesize / (tests$true_hp_rate * baserate_hp + (1 - baserate_hp) * tests$false_hp_rate))
  } else {
    tests$samplesize <-
      ceiling(
        ((1/tests$ls_pv)*(2*min_number-1+q*(1-tests$ls_pv)))/2 + ((((1/tests$ls_pv)*(2*min_number-1+q*(1-tests$ls_pv)))^2)/4 - ((min_number-0.5)/tests$ls_pv)^2)^0.5
      )
    tests$expectation_total_participants <- ceiling(tests$samplesize / (tests$false_ls_rate * baserate_hp + (1 - baserate_hp) * tests$true_ls_rate))
  }

  tests$min_quality_percent <- 100*min_number/tests$samplesize

  tests <- tests %>% dplyr::select(method, method_code, A, B, C, true_hp_rate, true_ls_rate, hp_pv, ls_pv, utility, samplesize, min_quality_percent, expectation_total_participants) %>% filter(samplesize <= min(samplesize) + tolerance) %>% dplyr::arrange(samplesize)
  # explanation text
  cat(sprintf(
    "When the prevelance for headphones is %f and the test procedure %s (code %i) with thresholds %i, %i, and %i for tests A, B, and C is used you need a sample of %i participants classified as %s to have a probability of at least %f that %i participants actually used %s. The 'quality' (precentage of correct devices) of such a sample would then be at least %f percent.\n",
    baserate_hp, tests$method[1], tests$method_code[1], as.integer(tests$A[1]), as.integer(tests$B[1]), as.integer(tests$C[1]), as.integer(tests$samplesize[1]), devices, min_prob, as.integer(min_number), devices, tests$min_quality_percent[1])
  )
  # actual output
  tests %>% t()
}
