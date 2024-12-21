#' Generate DNA Motif logos
#'
#' This function takes as input a named list of PFMa
#' and uses it to generate genomic logos, using the `ggseqlogo` package.
#'
#' @param PFMs A named list of PFMs. The names of the list
#' are informative of the motif ID and the TF it is bound by
#'
#' @return A list of plots, each corresponding to a sequence logo
#' @importFrom ggseqlogo ggseqlogo
#' @importFrom ggplot2 ggtitle
#' @export
retrieve_logos <- function(PFMs){

  logos <- lapply(X = seq_along(1:length(PFMs)),
          FUN = function(x){
                motif_sublist <- PFMs[x]
                PFM <- t(PFMs[[x]])
                colnames(PFM) <- seq_along(1:ncol(PFM))
                logo <- suppressWarnings(ggseqlogo::ggseqlogo(PFM, method = "bits") +
                        ggplot2::ggtitle(paste0("Genomic Logo for ",names(motif_sublist))))

                return(logo)}
          )
  names(logos) <- names(PFMs)
  return(logos)
  }
