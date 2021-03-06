% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dlmxtra.R
\name{dlmxOpt}
\alias{dlmxOpt}
\title{Fit model by minimizing RSS + pen * length of fitted curve.}
\usage{
dlmxOpt(y, parm, build, pen, method = "L-BFGS-B", ...)
}
\arguments{
\item{y}{Data}

\item{parm}{Intial value for parameter}

\item{build}{Build function}

\item{pen}{Penalty for length of fitted curve}

\item{method}{Method used by optim}

\item{...}{Parameters passed on to build and optim}
}
\value{
The result of calling optim padded with y and the fitted
    model object.
}
\description{
Fit model by minimizing RSS + pen * length of fitted
    curve.

Not many details for now
}
\author{
Søren Højsgaard
}
