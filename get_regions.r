get_regions <- function(row,chrom_sizes,is_circular,upstream = 300){
  
  chrom_name <- row["seqname"][[1]]
  start <- as.integer(row["start"][[1]]) 
  end <- as.integer(row["end"][[1]])
  strand <- row["strand"][[1]]
  boundary <- chrom_sizes[chrom_name]
  
  if (is_circular) {
    if (strand == "+") {
      region_fw <- rbind(chr = chrom_name, 
                         start_sequence = ifelse(start-upstream >= 1,start-upstream,(boundary-upstream)+start),
                         end_sequence = start)
      return(region_fw)
    } else {
      region_rw <- rbind(chr = chrom_name, 
                         start_sequence = end,
                         end_sequence = ifelse(end+upstream <= boundary,end+upstream,(end+upstream)-boundary))
      return(region_rw)
    }
    
  } else {
    if (strand == "+") {
      region_fw <- rbind(chr = chrom_name, 
                         start_sequence = ifelse(start-upstream >= 1,start-upstream+1,1),
                         end_sequence = start)
      return(region_fw)
    } else {
      region_rw <- rbind(chr = chrom_name, 
                         start_sequence = end,
                         end_sequence = ifelse(end+upstream <= boundary,end+upstream-1,boundary))
      return(region_rw)
    }
  }
}
