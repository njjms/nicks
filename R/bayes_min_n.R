#' Calculate minimum sample size for one-stage Bayesian test
#'
#' This function searches over the range specified by min_n to max_n for the smallest
#' sample size that achieves a desired power (e.g. .8).
#' If desired power is not reached, function defaults to returning the maximum
#' allowable sample size.
#'
#' @param req_p observed count
#' @param true_p total count
#' @param gamma posterior probability threshold needed to reject, defaults to .8
#' @param power Desired power, defaults to .8
#' @param n1 first stage sample size, defaults to 200
#' @param n2 second stage sample size, defaults to 800
#' @param prior_alpha alpha parameter of beta prior, defaults to .5 (Jeffrey's prior)
#' @param prior_beta beta parameter of beta prior, defaults to .5 (Jeffrey's prior)
#' @param min_n smallest n to search
#' @param max_n maximum possible n to search
#' @return list containing minimum selected sample size and associated probability of rejection
#' @include bayes_one_stage_direct.R
#' @examples
#' bayes_min_n(true_p = .994,
#'             gamma = .6)
#' @export

bayes_min_n <- function(req_p = .995,
                        true_p = .995,
                        gamma = .8,
                        power = .8,
                        alpha = .05,
                        prior_alpha = .5,
                        prior_beta = .5,
                        min_n = 500,
                        max_n = 1800,
                        step_size = 10) {
  sample_sizes_to_try <- seq(min_n, max_n, by = step_size)
  min_n_conditional <- max_n
  for (n in sample_sizes_to_try) {
    tmp <- bayes_one_stage_direct(req_p = req_p,
                                  true_p = true_p,
                                  gamma = gamma,
                                  n = n,
                                  prior_alpha = prior_alpha,
                                  prior_beta = prior_beta)
    if (tmp$p_reject >= power) {
      message("Satisfactory sample size less than max_n found.")
      min_n_conditional <- n
      break
    }
  }
  return(
    list(
      final_sample_size = min_n_conditional,
      p_reject = bayes_one_stage_direct(req_p = req_p,
                                        true_p = true_p,
                                        gamma = gamma,
                                        n = min_n_conditional,
                                        prior_alpha = prior_alpha,
                                        prior_beta = prior_beta)$p_reject

    )
  )
}
