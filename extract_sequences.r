extract_sequences <- function(FASTA,row){
  chr <- row[1]
  start <- row[2]
  end <- row[3]
  gene_name <- rownames(row)
  sequence <- substr(FASTA[chr], start, end)
  return(c(chromosome = chr, gene_name = gene_name, sequence = sequence))
}
