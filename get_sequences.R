get_sequences <- function(gff,fasta,is_circular){
  
  data <- read.table(gff, 
                     sep = "\t", 
                     header = FALSE, 
                     quote = "",
                     comment.char = "#",
                     stringsAsFactors = FALSE)
  
  colnames(data) <- c("seqname",
                      "source",
                      "feature",
                      "start",
                      "end",
                      "score",
                      "strand",
                      "frame",
                      "group")
  
  chrom_size <- data[data$feature == "region",]$end
  names(chrom_size) <- data$seqname[data$feature == "region"]
  
  genes <- data[data$feature == "gene",]
  
  tmp <- strsplit(x=genes$group, split=";")
  GeneName <- unlist(lapply(X = tmp, function(x)x[1])) 
  GeneName <- sub(pattern="ID=gene-",
                  x=GeneName,
                  replacement = "")
  
  regions_complete <- as.data.frame(t(apply(X = genes,
                                            MARGIN = 1,
                                            FUN = get_regions,
                                            chrom_size,
                                            is_circular)))
  
  colnames(regions_complete) <- c("chromosome", "start", "end")
  rownames(regions_complete) <- c(GeneName)
  
  regions_complete$start <- as.integer(regions_complete$start)
  regions_complete$end <- as.integer(regions_complete$end)
  regions_complete <- regions_complete[regions_complete$start <= regions_complete$end,]
  
  FASTA <- Biostrings::readDNAStringSet(fasta)
  FASTA <- as.character(FASTA)
  
  new_names <- sapply(strsplit(names(FASTA), " "), `[`, 1)
  names(FASTA) <- new_names
  
  all_seqs <- t(apply(X = regions_complete, MARGIN = 1, FUN = extract_sequences, FASTA = FASTA))
  colnames(all_seqs) <- c('Chr','Sequence')
  return(all_seqs)}