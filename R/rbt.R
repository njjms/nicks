#' Randomized Binomial Test
#'
#' In short, an RBT rejects borderline cases with \gamma probability. 
#' Whereas tests on binomial data as performed with Clopper-Pearson intervals 
#' or Wilson Score intervals usually have test size < nominal significance \alpha
#' level, an RBT will be guaranteed to always have theoretical size == \alpha.
#' 
#' Further detail can be found on: http://nicksun.fun/statistics/2020/11/16/sawtooth-power-curves.html
#' 
#' The purpose of the rbt() function is to provide a comparison against Wilson
#' and Clopper-Pearson based testing.
#'
#' @param x observed count
#' @param n total count
#' @param p_h0 probability under the null hypothesis
#' @param alpha defaults to .05
#' @param alternative form of alternative hypothesis, can be "lt" or "gt"
#' @return Single boolean; TRUE for rejection, FALSE for non-rejection
#' @examples
#' output <- vector(length=100, mode="numeric")
#' for (i in 1:100) {
#'   output[i] <- rbt(x=7, n=100, alpha=.05, alternative="lt")
#' }
#' mean(output) # should be about .526

rbt <- function(x,
                n,
                p_h0,
                alpha=.05,
                alternative="lt") {
  if (alternative == "lt") {
    rbt_output <- list(
      rejection_bound = qbinom(alpha,
                                size=n,
                                prob=p_h0),
      rejection_region = rejection_bound - 1,
      diff = alpha - pbinom(rejection_bound,
                             size=n,
                             prob=p_h0,
                             lower.tail=TRUE),
      gamma = diff/dbinom(rejection_bound, size=n, prob=p_h0)
    )
    if (x <= rbt_output$rejection_region) {
      rbt_output[["outcome"]] <- TRUE
      return(rbt_output)
    } else if (x == rbt_output$rejection_bound) {
      if (runif(1) <= rbt_output$gamma) {
        rbt_output[["outcome"]] <- TRUE
        return(rbt_output)
      } else {
        rbt_output[["outcome"]] <- FALSE
        return(rbt_output)
      }
    } else {
        rbt_output[["outcome"]] <- FALSE
        return(rbt_output)
    }
  } else if (alternative == "gt") {
      rbt_output <- list(
        rejection_bound = qbinom(alpha,
                                 size=n,
                                 prob=p_h0,
                                 lower.tail=FALSE),
        rejection_region = rejection_bound + 1,
        diff = alpha - pbinom(rejection_bound,
                              size=n,
                              prob=p_h0,
                              lower.tail=FALSE),
        gamma = diff/dbinom(rejection_bound, size=n, prob=p_h0)
      )
    if (x >= rbt_output$rejection_region) {
      rbt_output[["outcome"]] <- TRUE
      return(rbt_output)
    } else if (x == rbt_output$rejection_bound) {
      if (runif(1) <= rbt_output$gamma) {
        rbt_output[["outcome"]] <- TRUE
        return(rbt_output)
      } else {
        rbt_output[["outcome"]] <- FALSE
        return(rbt_output)
      }
    } else {
      rbt_output[["outcome"]] <- FALSE
      return(rbt_output)
    }
  } else {
    stop("Please input 'gt' or 'lt' for alternative parameter.")
  }
}
