#' Limit of Blank (LoB) calculations
#'
#' These calculations are based on CLSI EP17-A2 (section 5.3.3.1 covers both
#'  calculations and when to use each)
#'
#' According to EP17, the minimal experimental design is:
#' - 2 reagent lots
#' - 1 instrument system
#' - 3 days
#' - 4 blank samples
#' - 2 replicates per sample
#'
#' Note that multiplying all of this together gives us 48 samples.
#' According to EP17, the minimum number of replicates for blank samples is 60,
#'  so at least one of these factors will have to be increased.
#'
#' For the nonparametric approach, the number of reagent lots used in the
#'  experimental design is important.
#' For 2 or 3 lots, LoB is calculated for each of the lots and the overall LoB
#'  is the maximum of those values.
#' For 4 or more lots, the data is pooled together and a single LoB is calculated.
#'
#' @param x Vector of blank sample data
#' @param k Number of unique blank samples
#' @param alpha defaults to .05
#' @return a list containing parametric and nonparametric LoB, sample mean,
#' sample standard deviation, sample size, corrected quantile (parametric), and rank (nonparametric)
#' @examples
#' x <- data.frame(x = rnorm(20, mean = 1, sd = .2))
#' calculate_lob(x = x, k = 4)
#' @export

calculate_lob <- function(x,
                          k,
                          alpha = .05) {
  if (is.data.frame(x)) x <- x[,1] # Assume first column of data.frame is data
  x <- sort(x)
  n <- length(x)
  rp <- .5 + n*(1-alpha)
  mean_blank <- mean(x)
  sd_blank <- sd(x)
  if (!is.numeric(k) | k < 1) stop("k must be a valid number.")
  Cp <- qnorm(1-alpha)/(1-(1/(4*(n-k))))
  return(
    list(
      mean_blank = mean_blank,
      sd_blank = sd_blank,
      total_sample_size = n,
      parametric_corrected_quantile = Cp,
      nonparametric_chosen_rank = rp,
      parametric_lob = mean_blank + Cp*sd_blank,
      nonparametric_lob = x[floor(rp)] + (rp - floor(rp))*(x[ceiling(rp)] - x[floor(rp)])
    )
  )
}

