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
#' @param requirement_type form of alternative hypothesis, defaults to "gt"
#' @return list containing rejection bound, rejection region, gamma probability, and outcome of RBT
#' @examples
#' output <- vector(length=100, mode="numeric")
#' for (i in 1:100) {
#'   output[i] <- rbt(x=7, n=100, alpha=.05, requirement_type="gt")
#' }
#' mean(output) # should be about .526

rbt <- function(x,
                n,
                p_h0,
                alpha=.05,
                requirement_type="gt") {
  if (requirement_type == "gt") {
    rejection_bound = qbinom(alpha,
                             size=n,
                             prob=p_h0)
    rejection_region = rejection_bound - 1
    diff = alpha - sum(
      dbinom(0:rejection_region,
             size = n,
             prob = p_h0)
    )
    gamma = diff/dbinom(rejection_bound, size=n, prob=p_h0)
    rbt_output <- list(
      rejection_bound = rejection_bound,
      rejection_region = rejection_region,
      diff = diff,
      gamma = gamma
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
  } else if (requirement_type == "lt") {
      rejection_bound = qbinom(alpha,
                               size=n,
                               prob=p_h0,
                               lower.tail=FALSE)
      rejection_region = rejection_bound + 1
      diff = alpha - sum(
        dbinom(rejection_region:n,
               size = n,
               prob = p_h0)
      )
      gamma = diff/dbinom(rejection_bound, size=n, prob=p_h0)
      rbt_output <- list(
        rejection_bound = rejection_bound,
        rejection_region = rejection_region,
        diff = diff,
        gamma = gamma
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
    stop("Please input 'gt' or 'lt' for requirement_type parameter.")
  }
}

# development
# n = 100
# p_h0 = .95
# alpha = .05
# rejection_bound = qbinom(alpha,
#                          size=n,
#                          prob=p_h0,
#                          lower.tail=FALSE)
# rejection_bound
# sum(dbinom(0:rejection_bound, size=n, prob=p_h0))
# sum(dbinom(rejection_bound:n, size=n, prob=p_h0))
# pbinom(rejection_bound, size=100, prob=p_h0, lower.tail=TRUE)
#
# rejection_region = rejection_bound - 1
# sum(dbinom(0:rejection_region, size=n, prob=p_h0))
# pbinom(rejection_region, size=100, prob=.1)
#
# diff = alpha - sum(
#   dbinom(0:rejection_region,
#          size = n,
#          prob = p_h0)
# )
# diff
# gamma = diff/dbinom(rejection_bound, size=n, prob=p_h0)
# gamma
#
# rbt(x = 5,
#     n = 100,
#     p_h0 = .039,
#     requirement_type = "gt")
#
