#' Calculates rejection probability for one-stage Bayesian proportion test
#'
#' @param req_p observed count
#' @param true_p total count
#' @param gamma posterior probability threshold needed to reject, defaults to .8
#' @param n sample size, defaults to 200
#' @param prior_alpha alpha parameter of beta prior, defaults to .5 (Jeffrey's prior)
#' @param prior_beta beta parameter of beta prior, defaults to .5 (Jeffrey's prior)
#' @return a 2-element list with df of all possible observed values and probability of rejection
#' @examples
#' bayes_one_stage_direct(true_p = .996,
#'                        n = 1800,
#'                        gamma = .7)
#' @export

bayes_one_stage_direct <- function(req_p = .995,
                                   true_p = .995,
                                   gamma = .8,
                                   n = 200,
                                   prior_alpha = .5,
                                   prior_beta = .5) {
  n_successes <- 1:n
  binom_prob <- dbinom(n_successes, size = n, prob = true_p)
  posterior_alpha <- prior_alpha + n_successes
  posterior_beta <- prior_beta + n - n_successes
  posterior_probability <- purrr::pmap(list(posterior_alpha,
                                            posterior_beta),
                                       function(posterior_alpha,
                                                posterior_beta) {
                                         pbeta(req_p,
                                               shape1 = posterior_alpha,
                                               shape2 = posterior_beta,
                                               lower.tail = FALSE)
                                       })
  reject_n <- if_else(posterior_probability >= gamma, 1, 0)
  return(
    list(
      df = data.frame(
        n_successes = n_successes,
        binom_prob = binom_prob,
        posterior_alpha = posterior_alpha,
        posterior_beta = posterior_beta,
        posterior_probability = unlist(posterior_probability),
        reject_n = reject_n
      ),
      p_reject = sum(binom_prob[reject_n == 1])
    )
  )
}
