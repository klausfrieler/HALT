% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_hoc_estimation.R
\name{post_hoc_estimation}
\alias{post_hoc_estimation}
\title{Post hoc estimations}
\usage{
post_hoc_estimation(
  screening_strat,
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
  min_prob = NULL
)
}
\arguments{
\item{screening_strat}{three-letter lower-case abbreviation of the screening
strategy. See \code{\link{screening_strategies}}}

\item{combination_method}{Number (1 to 18) corresponding to the test method.}

\item{A_threshold}{(scalar integer) Threshold for Test A (1 to 6).}

\item{B_threshold}{(scalar integer) Threshold for Test B (1 to 6).}

\item{C_threshold}{(scalar integer) Threshold for Test C (1 to 6).}

\item{baserate_hp}{Sets the (estimated) prevalence of headphones in the
target population as a number between 0 and 1. Defaults to the unbiased
prevalence B of 0.1767 from \insertCite{HALT_2;textual}{HALT}.}

\item{devices}{Sets the desired playback device. Possible settings are
\code{"HP"} for headphones or \code{"LS"} for loudspeakers.}

\item{samplesize}{Number of participants who got a test result indicating
the use of the target device and who are therefore in the final sample.
\code{samplesize} is needed when \code{screening_strat} is \code{"fwr"} or
\code{"far"}.}

\item{target_selfreported}{Number of participants who reported the use of
the target device. \code{target_selfreported} is needed when
\code{screening_strat} is \code{"scc"}.}

\item{target_tested}{Number of participants who reported the use of a
playback device other than the target device and got a test result
indicating the use of the target device. \code{target_tested} is needed when
\code{screening_strat} is \code{"scc"}.}

\item{switch_to_target}{Sets the (estimated) switching prevalence.
The switching prevalence describes the probability that a participant who
indicates the use of a device other than the target device actually switches
to the target device after being prompted to do so.}

\item{min_number}{minimum number of participants \code{k} who are in the
final sample and should have used the correct device.
When you use \code{min_number} you cannot use \code{min_prob}, i.e.
\code{min_prob = NULL}.}

\item{min_prob}{(greater than 0, less than 1) for the event that
at least an unknown number of participants \code{k} who are in the final
sample used the correct device.
When you use \code{min_prob} you cannot use \code{min_number}, i.e.
\code{min_number = NULL}.}
}
\description{
This function provides probabilistic statements about the composition of a
sample after application of a certain test procedure within a screening
strategy. For this purpose a Binomial distribution is used.
}
\details{
The event that at least \code{k} participants in the final sample used the
target device is considered. The function determines either the probability
of this event or for a given probability the number \code{k}.

Depending on the screening strategy either \code{samplesize} and
\code{baserate_hp} (for \code{"fwr"} or \code{"far"}) or
\code{target_selfreported}, \code{target_tested}, and
\code{switch_to_target} (for \code{"scc"}) are needed.

For details, see \insertCite{HALT_2;textual}{HALT}.
}
\note{
Only one of the arguments \code{min_number} and \code{min_prob} can be
used.
}
\references{
\insertAllCited{}
}
