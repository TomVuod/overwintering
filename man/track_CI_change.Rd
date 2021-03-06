% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removal_experiment.R
\name{track_CI_change}
\alias{track_CI_change}
\title{Calculate credible interval}
\usage{
track_CI_change(data = experiment_course, summary_data = ant_removal)
}
\description{
Calculate credible interval for data obtained over the course of experiment
}
\details{
This function uses \code{experiment_course} and \code{ant_removal} datasets to
calculate how condifence interval for the null hypothesis and credible interval for
actual data changes over the course of experiment; this issue was examined following
the suggestion of the reviewer
@return an updated version of of \code{experiment_course} data frame with CI values
added (long format)
@examples
track_CI_change()
@importFrom dplyr \%>\%
@export
}
