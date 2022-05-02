#' Calculate rejection probability of Bayesian two stage design via simulation
#'
#' @param n_sim number of simulations, defaults to 10000
#' @param req_p observed count
#' @param true_p total count
#' @param gamma posterior probability threshold needed to reject, defaults to .8
#' @param n1 first stage sample size, defaults to 200
#' @param n2 second stage sample size, defaults to 800
#' @param prior_alpha alpha parameter of beta prior, defaults to .5 (Jeffrey's prior)
#' @param prior_beta beta parameter of beta prior, defaults to .5 (Jeffrey's prior)
#' @return df containing probability of overall rejection, rejection at first stage, and average sample size
#' @examples
#' bayes_two_stage_sim_power_calc(true_p = .995,
#'                                gamma = .9)
#' @include bayes_two_stage_sim.R
#' @export

bayes_two_stage_sim_power_calc <- function(n_sim = 10000,
                                           req_p = .995,
                                           true_p = .995,
                                           gamma = .8,
                                           n1 = 200,
                                           n2 = 800,
                                           prior_alpha = .5,
                                           prior_beta = .5) {
  n1_data <- vector(length = n_sim, mode = "numeric")
  result_n1 <- vector(length = n_sim, mode = "numeric")
  posterior_probability_n1 <- vector(length = n_sim, mode = "numeric")
  n2_data <- vector(length = n_sim, mode = "numeric")
  result_n2 <- vector(length = n_sim, mode = "numeric")
  posterior_probability_n2 <- vector(length = n_sim, mode = "numeric")

  for (i in 1:n_sim) {
    sim_result <- bayes_two_stage_sim(req_p = req_p,
                                      true_p = true_p,
                                      gamma = gamma,
                                      alpha = alpha,
                                      n1 = n1,
                                      n2 = n2,
                                      prior_alpha = prior_alpha,
                                      prior_beta = prior_beta)
    n1_data[i] <- sim_result$n1_data
    result_n1[i] <- sim_result$result_n1
    posterior_probability_n1[i] <- sim_result$posterior_probability_n1
    n2_data[i] <- sim_result$n2_data
    result_n2[i] <- sim_result$result_n2
    posterior_probability_n2[i] <- sim_result$posterior_probability_n2
  }

  final_results <- data.frame(
    n1_data = n1_data,
    result_n1 = result_n1,
    posterior_probability_n1 = posterior_probability_n1,
    n2_data = n2_data,
    result_n2 = result_n2,
    posterior_probability_n2 = posterior_probability_n2
  )

  overall_reject <- ifelse((final_results$result_n1 == 1) |
                           (final_results$result_n1 == 0 & final_results$result_n2 == 1),
                           1, 0)
  return(
    list(
      df = final_results,
      overall_reject = mean(overall_reject),
      reject_stage_1 = mean(final_results$result_n1),
      avg_sample_size = mean((final_results$result_n1)*n1 + (1 - mean(final_results$result_n1))*(n1 + n2))
    )
  )
}
