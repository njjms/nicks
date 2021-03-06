% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rbt_power.R
\name{rbt_power_calc}
\alias{rbt_power_calc}
\title{RBT Power Calculation Function}
\usage{
rbt_power_calc(
  sample_size,
  true_p,
  requirement,
  requirement_type = "gt",
  alpha = 0.05
)
}
\arguments{
\item{sample_size}{the total sample size}

\item{requirement}{the required underlying probability of success}

\item{requirement_type}{defaults to "gt" (greater than). Can be set to "lt" (less than)}

\item{alpha}{the significance level for a two-sided interval. Defaults to .05}

\item{true_prob}{the true underlying probability of success}
}
\value{
list containing parameters of power calculation (float), df of power calculation results, and power (float)
}
\description{
Calculates power for a Randomized Binomial Test in a binomial
experiment setting, given a sample size, true_prob,
requirement, and significance level.
}
