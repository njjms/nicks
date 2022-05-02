#' RBT Power Calculation Function
#'
#' Calculates power for a Randomized Binomial Test in a binomial
#' experiment setting, given a sample size, true_prob,
#' requirement, and significance level.
#'
#' @param sample_size the total sample size
#' @param true_prob the true underlying probability of success
#' @param requirement the required underlying probability of success
#' @param requirement_type defaults to "gt" (greater than). Can be set to "lt" (less than)
#' @param alpha the significance level for a two-sided interval. Defaults to .05
#' @return list containing parameters of power calculation (float), df of power calculation results, and power (float)
#' @include rbt.R
#' @export

rbt_power_calc <- function(sample_size,
					                 true_p,
					                 requirement,
					                 requirement_type="gt",
					                 alpha = .05) {
  rbt_parameters <- rbt(x = 0,
                        n = sample_size,
                        p_h0 = requirement,
                        alpha = alpha,
                        requirement_type = requirement_type)
  binom_prob_df <- data.frame(
    n = 0:sample_size
  )
  binom_prob_df$prob <- dbinom(binom_prob_df$n,
                               size = sample_size,
                               prob = true_p)
  if (requirement_type == "gt") {
    binom_prob_df$rejection <- ifelse(binom_prob_df$n < rbt_parameters$rejection_bound, 1, 0)
  } else {
    binom_prob_df$rejection <- ifelse(binom_prob_df$n > rbt_parameters$rejection_bound, 1, 0)
  }
  power <- sum(binom_prob_df[binom_prob_df$rejection == 1, "prob"]) +
    (rbt_parameters$gamma*dbinom(x=rbt_parameters$rejection_bound,
                                size = sample_size,
                                prob = true_p))
	return(
		list(
			rbt_power_config = list(
			  sample_size = sample_size,
			  true_prob = true_p,
			  requirement = requirement,
			  requirement_type = requirement_type,
			  alpha = alpha,
			  rbt_rejection_bound = rbt_parameters$rejection_bound,
			  rbt_gamma = rbt_parameters$gamma,
			  rbt_diff = rbt_parameters$diff
			),
			df = binom_prob_df,
			power = power
		)
	)
}

## development
# rbt_power(sample_size = 100,
#           true_p = .95,
#           requirement = .95,
#           alpha = .05)
