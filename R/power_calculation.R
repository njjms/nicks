#' Power Calculation Function
#'
#' Calculates power in a binomial experiment setting, given a sample size, true_prob, requirement, and significance level.
#'
#' @param sample_size the total sample size
#' @param true_prob the true underlying probability of success
#' @param requirement the required underlying probability of success
#' @param requirement_type defaults to "gt" (greater than). Can be set to "lt" (less than)
#' @param alpha the significance level for a two-sided interval
#' @param interval_type defaults to "cp" (Clopper-Pearson). Can be set to "ws" (wilson_score)
#' @param interval_surpasses defaults to FALSE, meaning only upper bound has to exceed
#' @return list containing calculation power (float) and df of binomial pmf, calculated confidence intervals, and samples that pass the requirement
#' @include clopper_pearson.R wilson_score.R
#' @examples
#'
#' lapply(c(.990, .985, .980),
#'        FUN = function(x) sapply(
#'          possible_sample_sizes,
#'          FUN = function(y)
#'            power_calc(sample_size = y,
#'                       true_prob = x,
#'                       requirement=desired_sensitivity,
#'                       alpha=significance_level*2,
#'                       requirement_type="gt",
#'                       interval_type="cp",
#'                       interval_surpasses=FALSE)$power
#'          )) -> sens_calculations
#'ggplot(
#'	data.frame(
#'		n = possible_sample_sizes,
#'		sens_990_power=sens_calculations[[1]],
#'		sens_985_power=sens_calculations[[2]],
#'		sens_980_power=sens_calculations[[3]]
#'	)) +
#'	geom_line(mapping=aes(x=n, y=sens_990_power,
#'			  color = "true_sens_990")) +
#'	geom_line(mapping=aes(x=n, y=sens_985_power,
#'			  color = "true_sens_985")) +
#'	geom_line(mapping=aes(x=n, y=sens_980_power,
#'			  color = "true_sens_980")) +
#'	scale_color_manual(name="plates",
#'					   values = c(true_sens_990="dodgerblue1",
#'								  true_sens_985="firebrick",
#'								  true_sens_980="forestgreen"),
#'					   breaks = c("true_sens_990",
#'								  "true_sens_985",
#'								  "true_sens_980")) +
#'	labs(title="Detecting Deviation in Sensitivity from p=.995", x="n", y="Power") +
#'	theme_bw()

power_calc <- function(sample_size,
					   true_prob,
					   requirement,
					   requirement_type="gt",
					   alpha,
					   interval_type="cp",
					   interval_surpasses=FALSE) {
	conf_ints <- switch(
		interval_type,
		"ws" = sapply(0:sample_size, FUN = function(x) wilson_score(x, n=sample_size,conf.level=1-alpha)),
		"cp" = sapply(0:sample_size, FUN = function(x) clopper_pearson(x, n=sample_size,conf.level=1-alpha))
	)

	data.frame(
		obs=0:sample_size,
		prob=dbinom(x=0:sample_size, size=sample_size, prob=true_prob),
		lower_bound = unlist(conf_ints["lower",]),
		upper_bound = unlist(conf_ints["upper",])
	) -> binom_prob_df

	if (interval_surpasses) {
  	binom_prob_df$pass <- switch(
  		requirement_type,
  		"lt" = ifelse(binom_prob_df$upper_bound <= requirement, 1, 0),
  		"gt" = ifelse(binom_prob_df$lower_bound >= requirement, 1, 0)
  	)
	} else {
  	binom_prob_df$pass <- switch(
  		requirement_type,
  		"lt" = ifelse(binom_prob_df$lower_bound <= requirement, 1, 0),
  		"gt" = ifelse(binom_prob_df$upper_bound >= requirement, 1, 0)
  	)
	}

	return(
		list(
			power_config = list(
			  sample_size = sample_size,
			  true_prob = true_prob,
			  requirement = requirement,
			  requirement_type = requirement_type,
			  alpha = alpha,
			  interval_type = interval_type,
			  interval_surpasses = interval_surpasses
			),
			df = binom_prob_df,
			power = sum(binom_prob_df[binom_prob_df$pass == 0, "prob"])
		)
	)
}

