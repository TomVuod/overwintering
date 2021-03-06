% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removal_experiment.R
\name{sample_distribution}
\alias{sample_distribution}
\title{Sample probability distribution}
\usage{
sample_distribution(data, sample_size = 10000)
}
\arguments{
\item{data}{data.frame returned by the \code{caclulate_CI}}

\item{sample_size}{int defines the number of draw from the probability
distribution}
}
\value{
the values of the random variable whose distribution was sampled; in
this case these are the proportions of ants from the upper nest segment
}
\description{
Take sample from the probability distribution
}
