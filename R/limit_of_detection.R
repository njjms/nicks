#' Limit of Detection (LoD) calculations
#'
#' These calculations are based on CLSI EP17-A2 (section 5.3.3.2 covers
#' both parametric and nonparametric calculations)
#'
#' According to EP17, the minimal experimental design is:
#' - 2 reagent lots
#' - 1 instrument system
#' - 3 days
#' - 4 low measurand content samples
#' - 2 replicates per sample
#'
#' Note that multiplying all of this together gives us 48 samples.
#' According to EP17, the minimum number of replicates for low measurand samples is 60,
#' so at least one of these factors will have to be increased.
#'
#' For these LoD calculations, it is assumed that variability is relatively
#' consistent across low measurand concentrations. If this is not the case,
#' a trial-and-error experimental design or precision profile approach may be
#' needed.
#'
#' @param x data.frame containing two columns: concentration and measurement
#' @param alpha defaults to .05
#' @return a list containing parametric LoD, a table of counts, pooled
#' SD estimate, and per concentration SD estimates.
#' @examples
#' calculate_lod(x = data, LoB = .15)
#' @export

calculate_lod <- function(x,
                          LoB = NA,
                          alpha = .05) {
  if (!is.data.frame(x)) stop("Input should be a two-column data.frame with concentration and measurement")
  if (ncol(x) > 2) warning("More than two columns detected. Only first two columns used.")
  if (is.na(LoB)) stop("LoB needs to be entered.")
  tmp <- data.frame(concentration = as.factor(x[,1]),
                    measurement = x[,2])
  tmp_sd <- aggregate(measurement ~ concentration, data = tmp, FUN = function(x) c(sd = sd(x), n = length(x)))
  tmp_sd$sd <- tmp_sd$measurement[,1]
  tmp_sd$n <- tmp_sd$measurement[,2]
  tmp_sd <- tmp_sd[,c("concentration", "sd", "n")]
  pooled_sd <- sqrt(sum(tmp_sd$sd * (tmp_sd$n - 1))/sum(tmp_sd$n - 1))
  Cp <- qnorm(1 - alpha)/(1 - (1/(4*(nrow(tmp) - nrow(tmp_sd)))))
  return(
    list(
      results_table = tmp_sd,
      pooled_sd = pooled_sd,
      corrected_quantile = Cp,
      LoD = LoB + Cp*pooled_sd
    )
  )
}
