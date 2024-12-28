#' Create a Table of Filtered Genes and Protein Information
#'
#' This function reads a TSV file of genes and a second file with protein
#' information, filters the genes, matches them to the protein data, and
#' create the resulting table.
#'
#' @param tsv A string specifying the path to the TSV file containing gene
#' information.
#' @param pro A string specifying the path to the file containing protein
#' information.
#'
#' @return A data frame with two columns: `Gene_ID` (the matched protein ID)
#' and `Gene_Name` (the corresponding gene name).
#' @export
table_first_genes <- function(tsv, pro) {
  TUSet <- read.table(tsv,
                      sep = "\t",
                      header = TRUE,
                      quote = "",
                      comment.char = "#",
                      stringsAsFactors = FALSE)

  TUSet <- tuGenes_filter(TUSet)
  tuGenes_filtered <- TUSet$X5.tuGenes

  protein_info <- read.table(pro,
                             sep = "\t",
                             header = FALSE,
                             quote = "",
                             comment.char = "#",
                             stringsAsFactors = FALSE)

  result_table <- match_genes(tuGenes_filtered, protein_info)
  result_table <- rename_first_column(result_table)
  colnames(result_table)[c(1, 2)] <- c('Gene_ID', 'Gene_Name')

  return(result_table)
}

#' Filter the `X5.tuGenes` Column of a Data Frame
#'
#' This function processes the `X5.tuGenes` column of a data frame by
#' extracting the portion before the first semicolon (`;`).
#' If the column does not exist, the function stops with an error.
#'
#' @param x A data frame containing a column named `X5.tuGenes`.
#'
#' @return A data frame with the `X5.tuGenes` column updated to include only
#' the portion before the first semicolon (`;`).
tuGenes_filter <- function(x) {
  if ("X5.tuGenes" %in% colnames(x)) {
    x$X5.tuGenes <- sapply(strsplit(as.character(x$X5.tuGenes), ";"), `[`, 1)
  } else {
    stop("The column 'X5.tuGenes' does not exist")
  }
  return(x)
}

#' Match Genes to Protein Information
#'
#' This function matches a vector of gene names to a protein information
#' dataset and returns the corresponding protein IDs and preferred names.
#'
#' @param tuGenes_vector A character vector containing the gene names to match.
#' @param protein_info_data A data frame where the first column contains
#' protein IDs and the second column contains gene names.
#'
#' @return A data frame containing two columns: the matched protein IDs (from
#' the first column) and the preferred names (from the second column) for the
#' input gene names.
match_genes <- function(tuGenes_vector, protein_info_data) {

  matched_genes <- protein_info_data[protein_info_data[, 2] %in%
                                       tuGenes_vector, ]

  #string_protein_id (colonna 1) e preferred_name (colonna 2)
  result <- matched_genes[, c(1, 2)]
  return(result)
}

#' Rename the First Column of a Data Frame
#'
#' This function renames the values in the first column of a data frame by
#' extracting the portion after the first period (`.`).
#'
#' @param dataframe A data frame where the first column (`V1`) has to be
#' renamed.
#'
#' @return A data frame with the first column (`V1`) updated to include only
#' the part of each value after the first period (`.`).
rename_first_column <- function(dataframe) {
  dataframe$V1 <- lapply(strsplit(dataframe$V1, "\\."), function(x) x[2])
  return(dataframe)
}



