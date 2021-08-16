#' Calculates Max Number of Misses before Requirement Fails
#'
#' @param n total count
#' @param requirement defaults to .95
#' @param requirement_type defaults to "gt" (greater than)
#' @param interval_type defaults to "cp" (Clopper-Pearson). Can be set to "ws".
#' @return an int for max number of misses, and df containing binomial probabilities
#' @include clopper_pearson.R wilson_score.R
#' @examples
#' max_misses(n=20, requirement = .95)

max_misses <- function(n,
                         conf.level=.95,
                         requirement=.95,
                         requirement_type="gt",
                         interval_type="cp") {
	x = 0:n
	conf_ints <- switch(
		interval_type,
		"ws" = wilson_score(x=x,
							n=n,
							conf.level=conf.level),
		"cp" = clopper_pearson(x=x,
							   n=n,
							   conf.level=conf.level)
	)
	df = data.frame(
		x = x,
		lower = conf_ints$lower,
		upper = conf_ints$upper
	)
	if(requirement_type == "gt") {
	  df$pass <- ifelse(df$upper < requirement, FALSE, TRUE)
	  max_misses <- n - tail(df[df$pass==FALSE, "x"], 1) - 1
	} else {
	  df$pass <- ifelse(df$lower > requirement, FALSE, TRUE)
	  max_misses <- tail(df[df$pass==TRUE, "x"], 1)
	}
	return(
		list(
			output_df = df,
			max_misses = max_misses
		)
	)
}
