get_regions <- function(row,chrom_sizes,is_circular){
  
  chrom_name <- row["seqname"][[1]]
  start <- as.integer(row["start"][[1]]) 
  end <- as.integer(row["end"][[1]])
  strand <- row["strand"][[1]]
  boundary <- chrom_sizes[chrom_name]
  
  if (is_circular) {
    if (strand == "+") {
      region_fw <- rbind(chr = chrom_name, 
                         start_sequence = ifelse(start-300 >= 1,start-300,(boundary-300)+start),
                         end_sequence = start)
      return(region_fw)
    } else {
      region_rw <- rbind(chr = chrom_name, 
                         start_sequence = end,
                         end_sequence = ifelse(end+300 <= boundary,end+300,(end+300)-boundary))
      return(region_rw)
    }
    
  } else {
    if (strand == "+") {
      region_fw <- rbind(chr = chrom_name, 
                         start_sequence = ifelse(start-300 >= 1,start-300+1,1),
                         end_sequence = start)
      return(region_fw)
    } else {
      region_rw <- rbind(chr = chrom_name, 
                         start_sequence = end,
                         end_sequence = ifelse(end+300 <= boundary,end+300-1,boundary))
      return(region_rw)
    }
  }
}