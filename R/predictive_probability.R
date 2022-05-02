#' Bayesian predictive probability calculations
#'
#' Implementation of a modified version of Zhong & Koopmeiners (2013) sample size reestimation approach.
#' The original paper details a design which compares two proportions and has an accompanying specification for
#' the reestimation procedure based on a z-statistic to save computational time.
#' In the case of one proportion, we can use beta-binomial distribution directly.
#'
#' Calculation consists of two steps: 1. given a prior identify all possible outcomes in second stage where
#' posterior probability is greater than some threshold and 2. add beta-binomial derived probabilities for these
#' outcomes together.
#' The predictive probability is used to determine a second stage sample size e.g.
#' try a range of values for m to see which will return a sufficiently high predictive probability.
#'
#' @param m second stage sample size
#' @param alpha alpha parameter of beta prior
#' @param beta beta parameter of beta prior
#' @param eta posterior probability threshold needed to reject, defaults to .8
#' @param requirement Requiremnet from PRD, defaults to .995
#' @return a 2-element list containing df of all possible second stage outcomes and final predictive probability
#' @include beta_binomial_pmf.R
#' @examples
#' x <- 1:1300
#' y <- sapply(x, FUN = function(x) predictive_probability(m = x,
#'                                                         alpha= 800.5,
#'                                                         beta = 2.5)$predictive_probability)
#' plot(x, y, type = 'l', xlab = "Second stage sample size", ylab = "predictive probability")
#' abline(h = .8, col="red")
#' mmax <- 1300
#' for (m_try in 1:mmax) {
#'   boop<- predictive_probability(m = m_try,
#'                                 alpha = 180.5,
#'                                 beta = .5)
#'   if (boop$predictive_probability >= .9) {
#'     print(paste0("Lowest m is to have predictive probability over 80%:", m_try))
#'     break
#'   }
#' }
#' print("Default to mmax.")
#' @export

predictive_probability <- function(m,
                                   alpha,
                                   beta,
                                   eta = .8,
                                   requirement = .995) {
  successes_n2 <- 0:m
  misses_n2 <- m - successes_n2
  beta_prob <- unlist(purrr::pmap(list(successes_n2, misses_n2),
                           function(successes_n2, misses_n2) {
                             pbeta(requirement,
                                   shape1 = alpha + successes_n2,
                                   shape2 = beta + misses_n2,
                                   lower.tail = FALSE)
                           }))
  beta_binomial_prob <- unlist(purrr::pmap(list(successes_n2, misses_n2),
                                   function(successes_n2, misses_n2) {
                                     beta_binomial_pmf(m = m,
                                                       alpha = alpha,
                                                       beta = beta,
                                                       successes = successes_n2,
                                                       failures = misses_n2)
                                   }))
  beta_prob_over <- dplyr::if_else(beta_prob >= eta, 1, 0)
  df <- data.frame(
    successes_n2 = successes_n2,
    misses_n2 = misses_n2,
    beta_prob = beta_prob,
    beta_binomial_prob = beta_binomial_prob,
    beta_prob_over = beta_prob_over
  )
  predictive_probability <- sum(beta_binomial_prob[beta_prob_over == 1])
  return(
    list(
      df = df,
      predictive_probability = predictive_probability
    )
  )
}
