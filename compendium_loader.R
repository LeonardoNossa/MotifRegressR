compendium_loader <- function(path, header = TRUE, row.names){
  data <- utils::read.table(file = path,
                            sep = ",",
                            header = header,
                            row.names = row.names)
  return(data)
}
