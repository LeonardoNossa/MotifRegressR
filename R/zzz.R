#' @title MotifRegressR: Perform motif regression on gene expression conditions
#'
#' @description MotifRegressR allows its users to perform DNA motifs regression
#' in the context of gene expression studies, allowing the investigation
#' of the most impactful motifs over different samples.
#' This is done by integrating gene upstream sequences (promoters)
#' with Position Weight Matrices (PWMs) of the chosen organism.
#' By sliding the PWMs over each gene, we assign to each gene
#' the maximum score obtained for each DNA motif
#' and then build the matrix of scores S.
#' Users can import PFMs from JASPAR or custom sources (via a MEME file)
#' and obtain the upstream sequences by proving a .gff and a FASTA file as input.
#' Users can also upload gene expression compendia containing
#' gene expression levels across different conditions.
#' If the IDs associated to each condition allow it,
#' metadata can be retrieved from ENA browser to obtain more
#' information regarding the samples. MotifRegressR then performs
#' regression analysis and determines the most impactful motifs for
#' the different tested conditions. Downstream steps, mainly represented
#' by visualization functions finally help the user in the sense-making steps
#' of the analyses.
#' @name MotifRegressR
#' @keywords package
"_PACKAGE"
