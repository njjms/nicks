#' Creates Clopper-Pearson binomial confidence intervals
#'
#' @param x observed count
#' @param n total count
#' @param conf.level defaults to .95
#' @return a 3-element list with "upper", "lower", and "point_estimate"
#' @examples
#' clopper_pearson(x=84, n=85, conf.level=.95)
#' @export

clopper_pearson <- function(x, n, conf.level=.95) {
  if (x > n) {
    stop("Number of successes (x) cannot be greater than sample size (n).")
  }
  if (conf.level > 1) {
    stop("conf.level cannot be larger than 1.")
  }
	return(
		list(
			lower=qbeta((1-conf.level)/2, x, n-x+1),
			upper=qbeta(1-((1-conf.level)/2), x+1, n-x),
			point_estimate=x/n
		)
	)
}

