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
#'
#' @export
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
