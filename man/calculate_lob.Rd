% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/limit_of_blank.R
\name{calculate_lob}
\alias{calculate_lob}
\title{Limit of Blank (LoB) calculations}
\usage{
calculate_lob(x, k, alpha = 0.05)
}
\arguments{
\item{x}{Vector of blank sample data}

\item{k}{Number of unique blank samples}

\item{alpha}{defaults to .05}
}
\value{
a list containing parametric and nonparametric LoB, sample mean,
sample standard deviation, sample size, corrected quantile (parametric), and rank (nonparametric)
}
\description{
These calculations are based on CLSI EP17-A2 (section 5.3.3.1 covers both
 calculations and when to use each)
}
\details{
According to EP17, the minimal experimental design is:
- 2 reagent lots
- 1 instrument system
- 3 days
- 4 blank samples
- 2 replicates per sample

Note that multiplying all of this together gives us 48 samples.
According to EP17, the minimum number of replicates for blank samples is 60,
 so at least one of these factors will have to be increased.

For the nonparametric approach, the number of reagent lots used in the
 experimental design is important.
For 2 or 3 lots, LoB is calculated for each of the lots and the overall LoB
 is the maximum of those values.
For 4 or more lots, the data is pooled together and a single LoB is calculated.
}
\examples{
x <- data.frame(x = rnorm(20, mean = 1, sd = .2))
calculate_lob(x = x, k = 4)
}
