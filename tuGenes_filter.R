# Funzione per filtrare la colonna X5.tuGenes
  tuGenes_filter <- function(x) {
    if ("X5.tuGenes" %in% colnames(x)) {
      x$X5.tuGenes <- sapply(strsplit(as.character(x$X5.tuGenes), ";"), `[`, 1)
    } else {
      stop("La colonna 'X5.tuGenes' non esiste")
    }
    return(x)
  }