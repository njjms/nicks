#' Calculates FWER for a given suite of tests, requirements, and true probabilities (assumed to be greater than the requirements)
#'
#' @param requirement_df df containing the following columns: tests, n, prob, requirements
#' @param nsims defaults to 10000
#' @param interval_type defaults to "cp" (Clopper-Pearson). Can be set to "ws" (Wilson-Score)
#' @param conf.level confidence level of the two sided intervals
#' @include clopper_pearson.R wilson_score.R
#' @examples
#' fake_data <- data.frame(
#'    	tests = c("Test 1", "Test 2", "NC"),
#'    	n = c(100, 50, 500),
#'    	prob = c(.992, .962, .023),
#'    	requirements = c(.99, .95, .04)
#' )
#' fwer(requirements_df=fake_data, nsims=10000, conf.level=.95)
#' @export

fwer <- function(requirements_df,
				 nsims=10000,
				 interval_type="cp",
				 conf.level=.95) {
    n_rejections <- vector(mode = "numeric", length = nsims)
    for (i in 1:nsims) {
    	n_rejection <- 0
    	for (test in requirements_df$tests) {
    		random_sample <- rbinom(1,
    								size=requirements_df[requirements_df$tests == test, 'n'],
    								prob=requirements_df[requirements_df$tests == test, 'prob'])
			conf_ints <- switch(
				interval_type,
				"ws" = wilson_score(x=random_sample,
									n=requirements_df[requirements_df$tests == test, 'n'],
									conf.level=conf.level),
				"cp" = clopper_pearson(x=random_sample,
									   n=requirements_df[requirements_df$tests == test, 'n'],
									   conf.level=conf.level)
			)
    		if (test != "NC") {
    			if (conf_ints$upper < requirements_df[requirements_df$tests == test, 'requirements']) {
    				n_rejection <- n_rejection + 1
    			}
    		} else {
    			if (conf_ints$lower > requirements_df[requirements_df$tests == test, 'requirements']) {
    				n_rejection <- n_rejection + 1
    			}
    		}
    	}
    	n_rejections[i] <- n_rejection
			if (sum(i == nsims/10*(1:10)) == 1) {
				print(paste0("completed: ", toString(i/nsims*100), "%"))
			}
    }
    return(
      list(
        n_rejections = n_rejections,
        fwer = mean(n_rejections >= 1)
      )
    )
}
