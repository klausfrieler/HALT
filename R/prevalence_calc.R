#' Calculate predictive values and overall utility
#'
#' This function calculates the predictive values and the overall utility for a
#' given prevalence for headphones.
#'
#' @inheritParams auto_config
#'
#' @references
#' \insertAllCited{}
#'
#' @export
tests_pv_utility <- function(baserate_hp = 211/1194) {
  stopifnot(baserate_hp < 1,
            baserate_hp > 0)
  tests <- HALT::test_config
  tests$hp_pv <-
    baserate_hp * tests$true_hp_rate / (baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$false_hp_rate)
  tests$ls_pv <-
    (1 - baserate_hp) * tests$true_ls_rate / ((1 - baserate_hp) * tests$true_ls_rate + baserate_hp * tests$false_ls_rate)
  tests$utility <-
    baserate_hp * tests$true_hp_rate + (1 - baserate_hp) * tests$true_ls_rate
#  tests <- tests %>% dplyr::select(method, method_code, A, B, C, true_hp_rate, true_ls_rate, hp_pv, ls_pv, utility)
  tests
}
#' Maximum Headphones Predictive Value
#'
#' This function returns a data.frame containing all tests with maximum
#' headphones predictive value for the specified prevalence of headphones.
#'
#' @inheritParams tests_pv_utility
#'
#' @references
#' \insertAllCited{}
#'
#' @export
max_hp_pv <- function(baserate_hp = 211/1194) {
  stopifnot(baserate_hp < 1,
            baserate_hp > 0)
  tests <- tests_pv_utility(baserate_hp = baserate_hp) %>% filter(hp_pv == max(hp_pv))
  tests
}
#' Maximum Loudspeakers Predictive Value
#'
#' This function returns a data.frame containing all tests with maximum
#' loudspeakers predictive value for the specified prevalence of headphones.
#'
#' @inheritParams tests_pv_utility
#'
#' @references
#' \insertAllCited{}
#'
#' @export
max_ls_pv <- function(baserate_hp = 211/1194) {
  stopifnot(baserate_hp < 1,
            baserate_hp > 0)
  tests <- tests_pv_utility(baserate_hp = baserate_hp) %>% filter(ls_pv == max(ls_pv))
  tests
}
#' Maximum Utility maximizing percent correct
#'
#' This function returns a data.frame containing all tests with maximum value
#' for utility maximizing percent correct for the specified prevalence of
#' headphones.
#'
#' @inheritParams tests_pv_utility
#'
#' @references
#' \insertAllCited{}
#'
#' @export
max_utility <- function(baserate_hp = 211/1194) {
  stopifnot(baserate_hp < 1,
            baserate_hp > 0)
  tests <- tests_pv_utility(baserate_hp = baserate_hp) %>% filter(utility == max(utility))
  tests
}
#' Test procedures with maximum properties
#'
#' This function returns a data.frame containing all tests with either
#' maximum headphones predictive value, maximum loudspeakers predictive value,
#' or maximum value for utility maximizing percent correct for the specified
#' prevalence of headphones.
#'
#' @inheritParams tests_pv_utility
#'
#' @references
#' \insertAllCited{}
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
#' @param samplesize number of participants classified as users of the target
#' device
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
#' \insertAllCited{}
#'
#' @export
post_hoc_est <- function(combination_method, A_threshold, B_threshold, C_threshold, baserate_hp, devices, samplesize, min_number = NULL, min_prob = NULL) {
  stopifnot(combination_method %in% 1:18,
            all(c(A_threshold,B_threshold,C_threshold) %in% 0:6),
            baserate_hp < 1,
            baserate_hp > 0,
            devices %in% c("HP","LS"),
            samplesize > 0,
            as.integer(samplesize) == as.double(samplesize),
            ((min_number <= samplesize & min_number >= 0) & is.null(min_prob)) || (is.null(min_number) & (min_prob <= 1 & min_prob >= 0))
  )
  if(combination_method %in% c(1,4,5,8:18) && A_threshold == 0){stop(sprintf("combination_method = %i needs A_threshold > 0!", combination_method))}
  if(combination_method %in% c(2,4,5,6,7, 10:18) && B_threshold == 0){stop(sprintf("combination_method = %i needs B_threshold > 0!", combination_method))}
  if(combination_method %in% c(3,6,7,8:18) && C_threshold == 0){stop(sprintf("combination_method = %i needs C_threshold > 0!", combination_method))}

  config <- HALT::make_config(combination_method = combination_method, A_threshold = A_threshold, B_threshold = B_threshold, C_threshold = C_threshold, baserate_hp = baserate_hp, devices = devices, use_scc = F, devices_exclude = T)
  if (combination_method < 10) {
    if (combination_method %in% c(2,3,6,7)) {
      A_threshold <- 0
      config$A_threshold <- 0
    }
    if (combination_method %in% c(1,3,8,9)) {
      B_threshold <- 0
      config$B_threshold <- 0
    }
    if (combination_method %in% c(1,2,4,5)) {
      C_threshold <- 0
      config$C_threshold <- 0
    }
  }

  procedure <- HALT::test_config %>% filter(method_code == config$combination_method, A == config$A_threshold, B == config$B_threshold, C == config$C_threshold)
#    test_config[test_config$method_code == combination_method & test_config$B == B_threshold & test_config$A == A_threshold & test_config$C == C_threshold,]

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
  cat(sprintf("In a sample of %i participants the event that at least %i used the desired device (%s) has a probability of at least %.4f when the prevalence for headphones is %.4f and the test procedure '%s' with thresholds %i, %i, and %i for tests A, B, and C was used.",
              as.integer(samplesize), as.integer(min_number), devices, min_prob, baserate_hp, HALT::test_config$method[test_config$method_code == combination_method][1], as.integer(A_threshold), as.integer(B_threshold), as.integer(C_threshold)))
}
#' A priori estimations
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
#' @inheritParams auto_config
#'
#' @param min_number minimum number of participants passing the test procedure
#' and using the target devices
#'
#' @param min_prob minimum probability (equal to or greater than 0.6, less than
#' 1) for the event that at least \code{min_number} participants pass the test
#' procedure and use the target devices.
#'
#' @param tolerance (non-negative integer) defaults to \code{0}. A value of 0
#' searches for the screening procedures with the minimum sample size.
#' If set to a value > 0 the function returns the screening procedures whose
#' sample sizes exceed the minimum sample size by at most this value.
#'
#' @return A tibble (data frame) with the characteristics of the test
#' procedure(s) and the attribute \code{explanation}.
#' This attribute is intended as an explanatory text containing a
#' probabilistic statement for the test procedure requiring the smallest sample
#' size.
#'
#' @references
#' \insertAllCited{}
#' @export
a_priori_est <- function(baserate_hp = 211/1194,
                         devices = "HP",
                         min_number,
                         min_prob = .8,
                         tolerance = as.integer(0)) {
  stopifnot(baserate_hp < 1,
            baserate_hp > 0,
            devices %in% c("HP","LS"),
            min_number > 0,
            as.integer(min_number) == as.double(min_number),
            min_prob >= 0.6,
            min_prob < 1,
            tolerance >= 0,
            as.integer(tolerance)==as.double(tolerance))

  tests <- tests_pv_utility(baserate_hp = baserate_hp)

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

  tests <- tests %>% dplyr::select(-false_ls_rate, -false_hp_rate, -logic_expr) %>%
    filter(samplesize <= min(samplesize) + tolerance) %>%
    dplyr::arrange(samplesize, expectation_total_participants, min_quality_percent)
  # explanation text
  explanation <-
    sprintf(
    "When the prevelance for headphones in your target sample is assumed to be %.4f and the screening method '%s' (code %i) with thresholds of %i, %i, and %i correct responses for tests A, B, and C is used a sample of %i participants classified as %s users is required to have a probability of at least %.2f that %i participants actually used %s. The percentage of correct identified target playback devices ('quality') of such a sample would then be at least %.1f percent.",
    baserate_hp, tests$method[1], tests$method_code[1], as.integer(tests$A[1]), as.integer(tests$B[1]), as.integer(tests$C[1]), as.integer(tests$samplesize[1]), devices, min_prob, as.integer(min_number), devices, tests$min_quality_percent[1])
  attr(tests, "explanation") <- explanation
  # actual output
  tests
}

#' Calculate overall utilities and target device probabilities for SCC
#'
#' @inheritParams auto_config
#'
#' @param switch_to_target Sets the (estimated) switching prevalence.
#' The switching prevalence describes the probability that a participant who
#' indicates the use of a device other than the target device actually switches
#' to the target device after being prompted to do so.
#'
#' @export
tests_scc_utility <- function(baserate_hp = 211/1194,
                              devices = "HP",
                              switch_to_target = 3/4) {
  stopifnot(baserate_hp < 1,
            baserate_hp > 0,
            switch_to_target < 1,
            switch_to_target > 0,
            length(devices) == 1L,
            devices %in% c("HP", "LS"))
  if (devices == "HP") {
    tests <- tests_pv_utility(baserate_hp = switch_to_target)
    tests$prob_scc_target <-
      (baserate_hp + (1 - baserate_hp)*switch_to_target*tests$true_hp_rate) /
      (baserate_hp + (1 - baserate_hp)*(switch_to_target*tests$true_hp_rate +
                                          (1 - switch_to_target)*tests$false_hp_rate))
  } else {
    tests <- tests_pv_utility(baserate_hp = 1 - switch_to_target)
    tests$prob_scc_target <-
      (1 - baserate_hp + baserate_hp*switch_to_target*tests$true_ls_rate) /
      (1 - baserate_hp + baserate_hp*(switch_to_target*tests$true_ls_rate +
                                        (1 - switch_to_target)*tests$false_ls_rate))
  }
  tests <- tests %>% dplyr::rename(scc_hp_pv = hp_pv,
                                   scc_ls_pv = ls_pv,
                                   scc_utility = utility) %>%
    mutate(scc_target = devices)
  tests
}

#' A priori estimations for SCC
#'
#' @inheritSection a_priori_est Model
#'
#' @inheritParams a_priori_est
#' @inheritParams tests_scc_utility
#'
#' @references
#' \insertAllCited{}
#' @export
a_priori_est_scc <- function(baserate_hp = 211/1194,
                             devices = "HP",
                             switch_to_target = 3/4,
                             min_number,
                             min_prob = .8,
                             tolerance = as.integer(0)) {
  stopifnot(baserate_hp < 1,
            baserate_hp > 0,
            devices %in% c("HP","LS"),
            min_number > 0,
            as.integer(min_number) == as.double(min_number),
            min_prob >= 0.6,
            min_prob < 1,
            tolerance >= 0,
            as.integer(tolerance)==as.double(tolerance))
  tests <- tests_scc_utility(baserate_hp = baserate_hp,
                             devices = devices,
                             switch_to_target = switch_to_target)

  q <- (qnorm(p = 1 - min_prob))^2
  p <- tests$prob_scc_target
  a <- ((-1/p) * (2*min_number - 1 + (1 - p)*q))
  b <- ((min_number - .5) / p)^2

  tests$samplesize <- ceiling(
    - .5*a + sqrt((.5*a)^2 - b)
  )

  if(devices == "HP") {
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
  tests$min_quality_percent <- 100 * min_number / tests$samplesize
  tests %>% dplyr::select(!c(false_hp_rate, false_ls_rate, logic_expr)) %>%
    filter(samplesize <= min(samplesize) + tolerance) %>%
    dplyr::arrange(samplesize, expectation_total_participants, min_quality_percent)
}

#' Post hoc estimation for SCC
#'
#' This function provides probabilistic statements about the composition of a
#' sample after application of a certain test procedure within SCC. For this
#' purpose a Binomial distribution is used.
#'
#' @inheritParams make_config
#' @inheritParams tests_scc_utility
#'
#' @param target_selfreported Number of participants who reported the use of
#' the target device.
#'
#' @param target_tested Number of participants who reported the use of a
#' playback device other than the target device and got a test result
#' indicating the use of the target device.
#'
#' @export
post_hoc_est_scc <- function(combination_method,
                             A_threshold,
                             B_threshold,
                             C_threshold,
                             switch_to_target,
                             devices,
                             target_selfreported,
                             target_tested,
                             min_number = NULL,
                             min_prob = NULL) {
  stopifnot(combination_method %in% 1:18,
            all(c(A_threshold,B_threshold,C_threshold) %in% 0:6),
            switch_to_target < 1,
            switch_to_target > 0,
            devices %in% c("HP","LS"),
            as.integer(target_selfreported) == as.double(target_selfreported),
            as.integer(target_tested) == as.double(target_tested),
            ((min_number <= (target_selfreported + target_tested) & min_number >= 0) & is.null(min_prob)) || (is.null(min_number) & (min_prob <= 1 & min_prob >= 0))
  )
  if(combination_method %in% c(1,4,5,8:18) && A_threshold == 0) {
    stop(sprintf("combination_method = %i needs A_threshold > 0!",
                 combination_method))
  }
  if(combination_method %in% c(2,4,5,6,7, 10:18) && B_threshold == 0){
    stop(sprintf("combination_method = %i needs B_threshold > 0!",
                 combination_method))
  }
  if(combination_method %in% c(3,6,7,8:18) && C_threshold == 0){
    stop(sprintf("combination_method = %i needs C_threshold > 0!",
                 combination_method))
  }
  config <- make_config(combination_method = combination_method,
                        A_threshold = A_threshold,
                        B_threshold = B_threshold,
                        C_threshold = C_threshold,
                        baserate_hp = switch_to_target,
                        devices = devices)
  procedure <- tests_scc_utility(switch_to_target = switch_to_target,
                                 devices = devices) %>%
    filter(method_code == config$combination_method, A == config$A_threshold,
           B == config$B_threshold, C == config$C_threshold)
  n <- target_tested
  if (is.null(min_prob)) {
    k <- min_number - target_selfreported
    k <- ifelse(k < 0, 0, k)
    min_prob <- sum(dbinom(k:n, size = n,
                           prob = ifelse(devices == "HP", procedure$scc_hp_pv[1],
                                         procedure$scc_ls_pv[1])))
  } else {
    min_number <- qbinom(p = min_prob, size = n,
                         prob = ifelse(devices == "HP", procedure$scc_hp_pv[1],
                                       procedure$scc_ls_pv[1]),
                         lower.tail = FALSE) + target_selfreported
  }
  #
  est <- data.frame(combination_method = combination_method,
                    A = config$A_threshold,
                    B = config$B_threshold,
                    C = config$C_threshold,
                    target_selfreported = target_selfreported,
                    target_tested = target_tested,
                    switch_to_target = switch_to_target,
                    min_number = min_number,
                    min_prob = min_prob,
                    min_data_qual_perc = min_number /
                      (target_selfreported + target_tested))

  devices <- ifelse(devices == "HP", "headphones", "loudspeakers")
  explanation <- c(
    sprintf("You used test combination %i with thresholds %i, %i, and %i for Test A, Test B, and Test C, respectively.",
            combination_method, A_threshold, B_threshold, C_threshold),
    sprintf("When %i participant indicated the use of %s and %i did not but there test result was %s your total sample size is %i.",
            target_selfreported, devices, target_tested, devices, target_selfreported + target_tested),
    sprintf("For the given test combination and this sample, the probability that at least %i participants used %s is %f according to a Binomial model and the assumption of an unbiased self-report.",
            min_number, devices, min_prob))

  attr(est, "explanation") <- explanation
  est
}
