#' Calculates Max Number of Sample Failures
#'
#' @param n total count
#' @param prob defaults to .95
#' @return an int for max number of sample failures, and df containing binomial probabilities
#' @examples
#' max_failures(n=20, prob=.0125)

max_failures <- function(n, prob) {
	x = 0:n
	bin_prob = pbinom(x, size=n, prob=prob, lower.tail=TRUE)
	df = data.frame(
		x = x,
		bin_prob = bin_prob,
		lower_tail_prob_over = bin_prob > .999
	)
	return(
		list(
			output_df = df,
			max_failures = head(df[df$lower_tail_prob_over==TRUE, "x"], 1)
		)
	)
}

