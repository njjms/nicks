% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayes_min_n.R
\name{bayes_min_n}
\alias{bayes_min_n}
\title{Calculate minimum sample size for one-stage Bayesian test}
\usage{
bayes_min_n(
  req_p = 0.995,
  true_p = 0.995,
  gamma = 0.8,
  power = 0.8,
  alpha = 0.05,
  prior_alpha = 0.5,
  prior_beta = 0.5,
  min_n = 500,
  max_n = 1800,
  step_size = 10
)
}
\arguments{
\item{req_p}{observed count}

\item{true_p}{total count}

\item{gamma}{posterior probability threshold needed to reject, defaults to .8}

\item{power}{Desired power, defaults to .8}

\item{prior_alpha}{alpha parameter of beta prior, defaults to .5 (Jeffrey's prior)}

\item{prior_beta}{beta parameter of beta prior, defaults to .5 (Jeffrey's prior)}

\item{min_n}{smallest n to search}

\item{max_n}{maximum possible n to search}

\item{n1}{first stage sample size, defaults to 200}

\item{n2}{second stage sample size, defaults to 800}
}
\value{
list containing minimum selected sample size and associated probability of rejection
}
\description{
This function searches over the range specified by min_n to max_n for the smallest
sample size that achieves a desired power (e.g. .8).
If desired power is not reached, function defaults to returning the maximum
allowable sample size.
}
\examples{
bayes_min_n(true_p = .994,
            gamma = .6)
}
