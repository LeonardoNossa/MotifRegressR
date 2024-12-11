#rinominare prima colonna
  rename_first_column <- function(dataframe) {
    dataframe$V1 <- lapply(strsplit(dataframe$V1, "\\."), function(x)x[2])
    return(dataframe)
  }