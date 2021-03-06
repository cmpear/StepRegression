% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step_regression.R
\name{regress_step}
\alias{regress_step}
\title{regress_step}
\usage{
regress_step(x, y)
}
\arguments{
\item{x}{a design matrix.  Function called recursively with fewer columns of x until finished}

\item{y}{a response vector}
}
\value{
a matrix of residuals, each column corresponds to a column of x
}
\description{
successively regresses each variable x our of the rest of the x matrix and y, then calls itself again.  Returns a vector of residuals for each variable.
}
