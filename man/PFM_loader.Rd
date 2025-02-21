% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PFM_loader.R
\name{PFM_loader}
\alias{PFM_loader}
\title{PFM Loader}
\usage{
PFM_loader(path = NULL, tax_id = NULL)
}
\arguments{
\item{path}{A character string representing the path to the file containing
PFMs.The file should follow the MEME motif format, with motifs identified by
the keyword "MOTIF".}

\item{tax_id}{A numeric or character vector representing the taxonomy ID of
the species for which to retrieve motifs (e.g., 9606 for Homo sapiens).}
}
\value{
A named list of data frames, where each data frame represents the
PFM of a motif. Each PFM contains the nucleotide frequencies
("A", "C", "G", "T") at each position.
}
\description{
This function allows users to load PFMs either from a local file in MEME
format or by specifying a taxonomy ID to retrieve motifs from JASPAR
database.
}
