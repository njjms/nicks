#' Probability mass function for Beta-Binomial SSRE
#'
#' Helper function used in a modified version of Zhong & Koopmeiners (2013) sample size reestimation approach.
#' The original paper details a design which compares two proportions and has an accompanying specification for
#' the reestimation procedure based on a z-statistic to save computational time.
#' In the case of one proportion, we can use beta-binomial distribution directly.
#'
#' @param m second stage sample size
#' @param prior_alpha alpha parameter of beta prior, defaults to .5 (Jeffrey's prior)
#' @param prior_beta beta parameter of beta prior, defaults to .5 (Jeffrey's prior)
#' @param successes number of observed successes in first n samples
#' @param failures number of observed failures in first n samples
#' @return a df of simulated outcomes and posterior probabilities
#' @export
beta_binomial_pmf <- function(m,
                              alpha,
                              beta,
                              successes,
                              failures) {
  output <- (beta(alpha + successes, beta + failures)/(beta(failures + 1, successes + 1)*beta(alpha, beta)*(m+1)))
  return(output)
}
