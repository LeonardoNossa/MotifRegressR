#' Extract DNA Sequences Based on GFF and FASTA Files
#'
#' This function extracts DNA sequences based on gene regions defined in a GFF
#' file and retrieves the corresponding sequences from a FASTA file.
#' The function returns a data frame with the extracted sequences for each gene.
#'
#' @param gff A string specifying the path to the GFF file containing genomic
#' features and regions.
#' @param fasta A string specifying the path to the FASTA file containing the
#' DNA sequences.
#' @param species A character, specifying the type of organism to consider.
#' Can either be "Bacteria" (default), "C. elegans", "D. melanogaster" or
#' "S. cerevisiae".
#' @param upstream An integer specifying the number of base pairs to include
#' upstream of each gene. Default is 300.
#'
#' @return A data frame with two columns:
#' \item{Chr}{The chromosome name for each extracted sequence.}
#' \item{Sequence}{The DNA sequence for each gene, extracted from the specified
#' region in the FASTA file.}
#' @export
get_sequences <- function(gff, fasta, species = "Bacteria", upstream = 300){

  is_circular <- ifelse(species == "Bacteria", TRUE, FALSE)

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

  tmp <- strsplit(x = genes$group, split = ";")

  GeneName <- unlist(lapply(X = tmp, function(x) x[1]))
  if (species == "Bacteria" || species == "S. cerevisiae") {
    GeneName <- sub(pattern = "ID=gene-", x = GeneName, replacement = "")
  } else if (species == "D. melanogaster") {
    GeneName <- sub(pattern = "ID=gene-Dmel_", x = GeneName, replacement = "")
  } else if (species == "C. elegans") {
    GeneName <- sub(pattern = "ID=gene-CELE_", x = GeneName, replacement = "")
  }

  ambigous <- names(table(GeneName)[table(GeneName) > 1])
  GeneNameIdx <- data.frame(name = GeneName, idx = seq_len(length(GeneName)))
  idx_to_remove <- c()

  for (gene in ambigous) {
    gene_repetitions <- GeneNameIdx$idx[GeneNameIdx$name == gene]
    gene_repetitions_to_remove <- sample(gene_repetitions,
                                         length(gene_repetitions)-1,
                                         replace = FALSE)
    idx_to_remove <- c(idx_to_remove, gene_repetitions_to_remove)
  }

  if (species == "D. melanogaster") {
    idx_to_remove <- c(idx_to_remove,
                       which(startsWith(genes$seqname, "NW_")))
  }

  if (length(idx_to_remove)) {
    GeneName <- GeneName[-idx_to_remove]
    genes <- genes[-idx_to_remove,]
  }



  regions_complete <- as.data.frame(t(apply(X = genes,
                                            MARGIN = 1,
                                            FUN = get_regions,
                                            chrom_size,
                                            is_circular,
                                            upstream)))

  colnames(regions_complete) <- c("chromosome", "start", "end")
  rownames(regions_complete) <- c(GeneName)

  regions_complete$start <- as.integer(regions_complete$start)
  regions_complete$end <- as.integer(regions_complete$end)
  regions_complete <- regions_complete[regions_complete$start <=
                                         regions_complete$end,]

  FASTA <- Biostrings::readDNAStringSet(fasta)
  FASTA <- as.character(FASTA)

  new_names <- sapply(strsplit(names(FASTA), " "), `[`, 1)
  names(FASTA) <- new_names

  all_seqs <- t(apply(X = regions_complete, MARGIN = 1,
                      FUN = extract_sequences, FASTA = FASTA))
  colnames(all_seqs) <- c('Chr', 'Sequence')
  return(all_seqs)
}

#' Get Region Coordinates Based on Genomic Position
#'
#' This function calculates the genomic region upstream a given gene,
#' based on its coordinates and the chromosomal boundaries. It handles both
#' circular and linear chromosomes, adjusting the start and end positions for
#' the region accordingly. The function also considers the strand orientation
#' to return the appropriate upstream or downstream region.
#'
#' @param row A named vector containing the genomic feature information,
#' such as `seqname` (chromosome), `start`, `end`, and `strand`
#' (strand orientation).
#' @param chrom_sizes A named vector or list containing the chromosome sizes,
#' where each chromosome name is associated with its length.
#' @param is_circular A logical value indicating whether the chromosome is
#' circular.
#' @param upstream An integer specifying the number of base pairs upstream
#' to include in the region. Default is 300.
#'
#' @return A data frame with two columns: `start_sequence` and `end_sequence`,
#' which represent the start and end coordinates of the genomic region.
#' The row names indicate the chromosome.
get_regions <- function(row, chrom_sizes, is_circular, upstream = 300){

  chrom_name <- row["seqname"][[1]]
  start <- as.integer(row["start"][[1]])
  end <- as.integer(row["end"][[1]])
  strand <- row["strand"][[1]]
  boundary <- chrom_sizes[chrom_name]

  if (is_circular) {
    if (strand == "+") {
      region_fw <- rbind(chr = chrom_name,
                         start_sequence = ifelse(start - upstream >= 1,
                                                 start - upstream,
                                                 (boundary - upstream) + start),
                         end_sequence = start)
      return(region_fw)
    } else {
      region_rw <- rbind(chr = chrom_name,
                         start_sequence = end,
                         end_sequence = ifelse(end + upstream <= boundary,
                                               end + upstream,
                                               (end + upstream) - boundary))
      return(region_rw)
    }

  } else {
    if (strand == "+") {
      region_fw <- rbind(chr = chrom_name,
                         start_sequence = ifelse(start - upstream >= 1,
                                                 start - upstream + 1, 1),
                         end_sequence = start)
      return(region_fw)
    } else {
      region_rw <- rbind(chr = chrom_name,
                         start_sequence = end,
                         end_sequence = ifelse(end + upstream <= boundary,
                                               end + upstream - 1, boundary))
      return(region_rw)
    }
  }
}

#' Extract DNA Sequences from a FASTA Object
#'
#' This function extracts a DNA sequence from a FASTA object based on the
#' provided chromosome, start, and end positions. It returns the sequence along
#' with its associated chromosome and gene name.
#'
#' @param FASTA A named list or vector of DNA sequences, where each element is
#' a sequence.
#' @param row A vector containing the chromosome name, start position, and
#' end position of the sequence to extract. The row should be in the
#' format: c(chr, start, end).
#'
#' @return A character vector with three elements:
#' \item{chromosome}{The name of the chromosome from which the sequence is
#' extracted.}
#' \item{gene_name}{The gene name, extracted from the row names.}
#' \item{sequence}{The extracted DNA sequence.}
extract_sequences <- function(FASTA,row){
  chr <- row[1]
  start <- row[2]
  end <- row[3]
  gene_name <- rownames(row)
  sequence <- substr(FASTA[chr], start, end)
  return(c(chromosome = chr, gene_name = gene_name, sequence = sequence))
}












