% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stan.R
\name{as_posterior_array}
\alias{as_posterior_array}
\title{Convert posterior data frame to a list of array}
\usage{
as_posterior_array(d, param = NULL, sample_n = NULL)
}
\arguments{
\item{d}{Data frame. Posterior samples returned from \code{stom::extract()}
or \code{stom::get_pars()}.}

\item{param}{String. Name of the parameter to extract (without brackets).
By default, \code{NULL}, which assumes the input data frame \code{d} contains
only the columns of the parameter of interest.}

\item{sample}{Integer. Number of samples to draw from the posterior samples.
If \code{NULL}, returns the full data frame of the posterior samples.}
}
\value{
A list of arrays, with each array corresponding to a draw from the
posterior samples (i.e. a row in the posterior data frame).
}
\description{
Convert posterior data frame to a list of array
}
\examples{
fp = system.file("cases", "wine_network", "wine2_normal_first_level2.RDS", package="stom")
m = readRDS(fp)
post = extract(m)
as_posterior_array( post, param="Int", sample_n=5 )
}
