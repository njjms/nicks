#' Helper function for Bayesian two stage simulations
#'
#' @param req_p observed count
#' @param true_p total count
#' @param gamma posterior probability threshold needed to reject, defaults to .8
#' @param n1 first stage sample size, defaults to 200
#' @param n2 second stage sample size, defaults to 800
#' @param prior_alpha alpha parameter of beta prior, defaults to .5 (Jeffrey's prior)
#' @param prior_beta beta parameter of beta prior, defaults to .5 (Jeffrey's prior)
#' @return a df of simulated outcomes and posterior probabilities
#' @export

bayes_two_stage_sim <- function(req_p = .995,
                                true_p = .995,
                                gamma = .8,
                                alpha = .05,
                                n1 = 200,
                                n2 = 800,
                                prior_alpha = .5,
                                prior_beta = .5) {
  # Get first n1 data points, calculate posterior probability
  n1_data <- rbinom(n = 1, size = n1, prob = true_p)
  posterior_alpha <- prior_alpha + n1_data
  posterior_beta <- prior_beta + n1 - n1_data

  posterior_probability_n1 <- pbeta(req_p,
                                    shape1 = posterior_alpha,
                                    shape2 = posterior_beta,
                                    lower.tail = FALSE)
  result_n1 <- if_else(posterior_probability_n1 >= gamma, 1, 0)

  # Get second n2 data points, calculate posterior probability
  n2_data <- rbinom(n = 1, size = n2, prob = true_p)
  posterior_alpha <- posterior_alpha + n2_data
  posterior_beta <- posterior_beta + n2 - n2_data

  posterior_probability_n2 <- pbeta(req_p,
                                    shape1 = posterior_alpha,
                                    shape2 = posterior_beta,
                                    lower.tail = FALSE)
  result_n2 <- if_else(posterior_probability_n2 >= gamma, 1, 0)
  return(
    list(
      n1_data = n1_data,
      result_n1 = result_n1,
      posterior_probability_n1 = posterior_probability_n1,
      n2_data = n2_data,
      result_n2 = result_n2,
      posterior_probability_n2 = posterior_probability_n2
    )
  )
}
