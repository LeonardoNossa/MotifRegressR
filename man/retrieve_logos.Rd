% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve_logos.R
\name{retrieve_logos}
\alias{retrieve_logos}
\title{Retrieve logos}
\usage{
retrieve_logos(PFMs)
}
\arguments{
\item{PFMs}{A named list of PFMs. The names of the list
are informative of the motif ID and the TF it is bound by.}
}
\value{
A named list of plots, each corresponding to a sequence logo
}
\description{
This function takes as input a named list of PFMa and uses it to generate
genomic logos, using the \code{ggseqlogo} package.
}
