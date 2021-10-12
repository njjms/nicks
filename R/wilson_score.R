#' Creates Wilson Score binomial confidence intervals
#'
#' @param x observed count
#' @param n total count
#' @param conf.level defaults to .95
#' @return a 2-element list with "upper" and "lower"
#' @examples
#' wilson_score(x=84, n=85, conf.level=.95)

wilson_score <- function(x, n, conf.level=.95) {
	p_hat <- x/n
	z_alpha <- abs(qnorm((1 - conf.level)/2))
	margin <- (z_alpha * ((p_hat*(1-p_hat) + (z_alpha^2)/(4*n))/n)^(1/2))/(1 + (z_alpha^2)/n)
	midpoint <- (p_hat + (z_alpha^2)/(2*n))/(1 + (z_alpha^2)/n)
	return(
		list(
			lower = midpoint - margin,
			upper = midpoint + margin,
			point_estimate = p_hat
		)
	)
}
