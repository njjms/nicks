#' Search neighborhood around a given sample size for highest power
#'
#' In a binomial setting, sometimes higher power can be found by decreasing the
#' sample size by a small amount.
#'
#' @param neighborhood the neighborhood to search in
#' @param power_config a list containing the specifications for a power calculation
#' @return list containing baseline power (float) and smallest sample size in neighborhood that achieves that power
#' @include power_calculation.R
#' @examples
#' power_neighborhood(neighborhood = 10, power_config = power_config)

power_neighborhood <- function(neighborhood,
							                 power_config) {
	potential_sample_sizes <- seq(power_config$sample_size - neighborhood,
							  	              power_config$sample_size + neighborhood,
								                by = 1)

	power <- sapply(potential_sample_sizes,
		   			FUN = function(x)
					    power_calc(
						  sample_size = x,
						  true_prob = power_config$true_prob,
						  requirement = power_config$requirement,
						  requirement_type = power_config$requirement_type,
						  alpha = power_config$alpha,
						  interval_type = power_config$interval_type,
						  AC_type = power_config$AC_type,
						  prq_delta = power_config$prq_delta)$power)

	output_df <- data.frame(
		n = potential_sample_sizes,
		power = power
	)

	baseline_power <- output_df[output_df$n == power_config$sample_size, "power"]
	higher_power_df <- output_df[output_df$power > baseline_power,]
	if (nrow(higher_power_df) > 0) {
		min_surpassing_sample_size <- min(higher_power_df$n)
	} else {
		min_surpassing_sample_size <- power_config$sample_size
	}

	return(list(
			baseline_power = baseline_power,
			higher_power_df = higher_power_df,
			min_surpassing_sample_size = min_surpassing_sample_size))
}
