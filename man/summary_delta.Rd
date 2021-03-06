% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_delta.R
\name{summary_delta}
\alias{summary_delta}
\title{Coefficients for deltas}
\usage{
summary_delta(x,prob=0.95)
}
\arguments{
\item{x}{an object of the class \emph{bayesbr}, containing the list returned from the \code{\link{bayesbr}} function.}

\item{prob}{a probability containing the credibility index for the HPD interval for the coefficients of the covariates.}
}
\value{
A list containing the estimates for delta parametrer, this list contains the following items:
\describe{
\item{table}{a table with the means, medians, standard deviations and the Highest Posterior Density (HPD) Interval,}
\item{coeff}{a vector containing the estimated coefficients.}}
}
\description{
A function that uses posterior distribution values of the model and calculates the estimates for delta parametrer.
}
\seealso{
\code{\link{summary_xi}},\code{\link{values}},\code{\link{summary.bayesbr}}
}
