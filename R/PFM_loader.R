#' PFM Loader
#'
#' This function allows users to load PFMs either from a local file in MEME 
#' format or by specifying a taxonomy ID to retrieve motifs from an online 
#' database.
#'
#' @param path A character string representing the path to the file containing
#' PFMs.The file should follow the MEME motif format, with motifs identified by
#' the keyword "MOTIF".
#'
#' @param tax_id A numeric or character vector representing the taxonomy ID of
#' the species for which to retrieve motifs (e.g., 9606 for Homo sapiens).
#'
#' @return A named list of data frames, where each data frame represents the
#' PFM of a motif. Each PFM contains the nucleotide frequencies
#' ("A", "C", "G", "T") at each position.
#' @export
PFM_loader <- function(path = NULL, tax_id = NULL){
  if(is.null(tax_id) & !is.null(path)){
    PFMs <- local_PFM_loader(path)
    return(PFMs)
  } else if(is.null(path) & !is.null(tax_id)) {
    PFMs <- online_PFM_loader(tax_id)
    return(PFMs)
  } else if(!is.null(path) & !is.null(tax_id)){
    PFMs <- local_PFM_loader(path)
    return(PFMs)
  } else {
    stop('Either "path" or "tax_id" MUST be defined')
  }
}

#' local_PFM_loader
#'
#' This function reads a file containing position frequency matrices (PFMs) and
#' extracts them into a list of data frames. Each PFM represents the nucleotide
#' frequencies ("A", "C", "G", "T") at each position for a given motif.
#'
#' @param path A character string representing the path to the file containing
#' PFMs.The file should follow the MEME motif format, with motifs identified by
#' the keyword "MOTIF".
#'
#' @return A named list of data frames. Each data frame corresponds to the PFM
#' of a motif and contains the nucleotide frequencies at each position
#' ("A", "C", "G", "T").The names of the list elements are the motif
#' names extracted from the file.
#'
#' @importFrom utils read.table
local_PFM_loader <- function(path){

  Tab <-  utils::read.table(path , skip = 9, fill = T)
  motif_indices <- grep("MOTIF", Tab$V1)
  if (all(nchar(Tab[Tab$V1 == "MOTIF",]$V3) > 0)) {
    motif_names <- paste0(Tab[Tab$V1 == "MOTIF",]$V3,"_",Tab[Tab$V1 ==
                                                               "MOTIF",]$V2)
  } else {
    motif_names <- Tab[Tab$V1 == "MOTIF",]$V2
  }


  motifs <- list()
  for (i in seq_along(motif_indices)) {

    start <- motif_indices[i]
    end <- if (i < length(motif_indices)) motif_indices[i + 1] - 1 else
      nrow(Tab)

    motif_name <- motif_names[i]
    motif_df <- Tab[(start + 2) : (end - 1),1:4]
    colnames(motif_df) <- c("A", "C", "G", "T")
    rownames(motif_df) <- seq(nrow(motif_df))

    if (any(motif_df == "URL")) {
      idx <- which(motif_df == "URL")
      motif_df <- motif_df[-(idx:nrow(motif_df)),]
    }

    motif_df <- as.data.frame(apply(motif_df,2,as.numeric),
                              stringsAsFactors = FALSE)
    motifs[[motif_name]] <- motif_df
  }
  return(motifs)
}

#' online_PFM_loader
#'
#' This function queries the JASPAR database to retrieve position frequency
#' matrices (PFMs)
#' for transcription factor binding motifs of a given species, identified by
#' its taxonomy ID.
#'
#' @param tax_id A numeric or character vector representing the taxonomy ID of
#' the species for which to retrieve motifs (e.g., 9606 for Homo sapiens).
#'
#' @return A named list of data frames, where each data frame represents the
#' PFM of a motif. Each PFM contains the nucleotide frequencies ("A", "C", "G", "T")
#' at each position. The names of the list elements correspond to the transcription
#' factor names concatenated with their motif IDs (e.g., "TF_name_MA0001").
#'
#' @import httr
#' @import jsonlite
#' @import glue
online_PFM_loader <- function(tax_id) {

  motif_id = c()
  url <- glue::glue("https://jaspar.elixir.no/api/v1/species/{tax_id}/")
  query <- list(identifiers = tax_id)

  response <- httr::GET(url, query = query)
  content <- jsonlite::fromJSON(content(response, as = "text", encoding = "UTF-8"))

  while (TRUE){

    content <- jsonlite::fromJSON(content(response, as = "text", encoding = "UTF-8"))
    results <- content$results
    urls <- results$url
    url <- content$`next`
    if (length(url) != 0){
      response <- httr::GET(url, query = query)
      motif_id <- c(motif_id, urls)
    } else {
      motif_id <- c(motif_id, urls)
      break
    }
  }
  motif_id <- vapply(X = motif_id, FUN = function(x, split)
  {return(unlist(strsplit(x, split))[7])} ,
  FUN.VALUE = character(1), split = "/")
  names(motif_id) <- c()

  matrices <- list()
  genes <- c()

  for (motif in motif_id) {
    url <- paste0("https://jaspar.elixir.no/api/v1/matrix/",motif,"/")
    response <- httr::GET(url)
    if (status_code(response) == 200) {
      data <- content(response, "text", encoding = 'UTF-8')

      json_data <- jsonlite::fromJSON(data)

      genes <- c(genes, json_data$name)

      df <- data.frame("A"=json_data$pfm$A, "C"=json_data$pfm$C,
                       "G"=json_data$pfm$G, "T"=json_data$pfm$`T`)
      matrices <- append(matrices, list(df))

    } else {
      print(paste("Errore nella richiesta:", status_code(response)))
    }

  }

  names_df <- paste0(genes, "_", motif_id)
  names(matrices) <- names_df
  return(matrices)

}

