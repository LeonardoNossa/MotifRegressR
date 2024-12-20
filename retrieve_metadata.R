retrieve_metadata <- function(compendium){
  
  SRR_ids <- colnames(compendium)
  SRR_ids_process <- sapply(X = SRR_ids, FUN = function(x){
    if(endsWith(x,".fastq")){
      return(gsub(pattern = ".fastq",replacement = "",x))
    } else {
      return(x)
    }
  })
  
  names(SRR_ids_process) <- NULL
  
  res <- sapply(X = SRR_ids_process,FUN = function(x){
    url <- paste0("https://www.ebi.ac.uk/ena/portal/api/filereport?accession=",
                  x,
                  "&result=read_run&fields=study_accession,secondary_study_accession,",
                  "sample_accession,secondary_sample_accession,experiment_accession,",
                  "run_accession,submission_accession,tax_id,scientific_name,",
                  "instrument_model,library_name,library_layout,library_strategy,",
                  "library_source,library_selection,base_count,center_name,first_public,",
                  "last_updated,experiment_title,study_title,study_alias,experiment_alias,",
                  "run_alias,sample_alias,sample_title,first_created&format=json&download=true&limit=0")
    return(url)
  })
  
  response <- sapply(X = res,FUN = httr::GET,simplify = FALSE)
  data <- sapply(X = response, FUN = function(x){
    entry <- jsonlite::fromJSON(content(x, "text", encoding = "UTF-8"))
    if(!(is.data.frame(entry))){
      entry <- list()
    }
    return(entry)
  },simplify = FALSE)
  
  data <- do.call(rbind,data)
  data_df <- as.data.frame(data)
  
  if (ncol(data_df) == 0) {
    stop("No metadata could be fetched!")
  }
  
  missing_entries <- SRR_ids_process[!(SRR_ids_process%in%rownames(data_df))]
  if (length(missing_entries) != 0) {
    mess <- paste0("Sorry! Could NOT fetch metadata for the following sample: ",
                   missing_entries,"\n")
    message(mess)
  }
  
  return(data_df)
}
