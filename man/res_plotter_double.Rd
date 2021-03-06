% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/res_plotter.R
\name{res_plotter_double}
\alias{res_plotter_double}
\title{res_plotter_double}
\usage{
res_plotter_double(res, y, xlab = "y-values", ylab = "residuals", ...)
}
\arguments{
\item{res}{a matrix of residuals}

\item{y}{the response vector}

\item{xlab}{label for x-axis, defaults to "y-values"}

\item{ylab}{label for y-axis, defaults to "residuals"}

\item{...}{additional graphical parameters for title(), useful for adding main="my plot"}
}
\description{
creates a y-values vs residuals plot, with one sub-plot per column in a residuals matrix.  Sub-plots are organized in two columns.
}
