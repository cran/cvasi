% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pll.R
\name{pll_debug}
\alias{pll_debug}
\title{Disable parallelization for debugging}
\usage{
pll_debug(state = TRUE)
}
\arguments{
\item{state}{\code{logical}, if \code{TRUE} then parallelization is disabled}
}
\value{
no return value
}
\description{
In certain cases it might be beneficial to disable parallel execution
of e.g. effect profile calculations. By disabling, all processes run
sequentially and instantly pass messages to the console which would be
delayed during parallel processing. This makes it easier to pinpoint
problems within the data or algorithm.
}
