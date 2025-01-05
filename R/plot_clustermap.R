#' Plot clustermap
#'
#' This function takes as input a sublist coming from `models2dataframe`,
#' distances metrics and clustering approaches. The data.frame inside the
#' sublist is first pivoted using `pivot_df` function, then a clustermap is
#' built
#'
#' @param df_sublist A sublist coming from `models2dataframe`
#'
#' @param distance a vector of length 2, containing the distance metrics to be
#' used. Each element can be a pre-defined character which is in
#' ("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski",
#' "pearson", "spearman", "kendall"). It can also be a function.
#' If the function has one argument, the input argument should be a matrix
#' and the returned value should be a dist object.
#' If the function has two arguments, the input arguments are two vectors
#' and the function calculates distance between these two vectors.
#'
#' @param method a vector of length 2, containing the clustering method to
#' perform. the agglomeration method to be used. This should be one of "ward.D",
#' "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA),
#' "median" (= WPGMC) or "centroid" (= UPGMC).
#'
#' @return a clustermap, showing the grouping of motifs and conditions, based on
#' the importance values.
#' @importFrom ComplexHeatmap Heatmap
#' @importFrom ComplexHeatmap draw
#' @importFrom circlize colorRamp2
#' @importFrom grid gpar
#' @importFrom grid unit
#' @export
plot_clustermap <- function(df_sublist,distance = c("euclidean","euclidean"), method = c("complete","complete")){

  regression_name <- names(df_sublist)
  df <- df_sublist[[1]]
  matrix <- pivot_df(df)

  distance_row <- distance[1]
  distance_col <- distance[2]
  method_row <- method[1]
  method_col <- method[2]

  color_fun <- circlize::colorRamp2(c(min(matrix), 0, max(matrix)), c("yellow2", "orange", "brown"))

  heatmap <- ComplexHeatmap::Heatmap(
    matrix,
    name = "Importance",
    col = color_fun,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    clustering_distance_rows = distance_row,
    clustering_distance_columns = distance_col,
    clustering_method_rows = method_row,
    clustering_method_columns = method_col,
    row_names_side = "left",
    row_names_gp = grid::gpar(fontsize = 5),
    column_names_gp = grid::gpar(fontsize = 6),
    column_names_rot = 45,
    row_dend_width = grid::unit(4, "cm"),
    column_dend_height = grid::unit(2, "cm"),
    width = grid::unit(17.6, "cm"),
    height = grid::unit(17.8, "cm"),
    row_title = "Motifs",
    row_title_side = "left",
    column_title = "Conditions",
    column_title_side = "bottom"
  )

  ComplexHeatmap::draw(
    heatmap,
    heatmap_legend_side = "right",
    annotation_legend_side = "right",
    column_title = paste0("Clustermap of motif importance across conditions ",
                          "(",regression_name,")"),
    column_title_side = "top",
    column_title_gp = grid::gpar(fontsize = 12, fontface = "bold"),
  )
}

#' Pivot data.frame
#'
#' This function takes as input a data.frame coming
#' from `models2dataframe` call and pivots it, assigning the importance score
#' for each motif, in each condition.
#'
#' @param df the data.frame coming from `models2dataframe`, containing the top
#' k motif in each condition and their importance score
#'
#' @return A data.frame, where rows are motifs, and columns are the conditions.
#' If a motif is not among the top k motifs in that condition, the function will
#' assign 0.
pivot_df <- function(df){
  if (nrow(df) == 1) {
    matrix <- matrix(data = df$coef)
    rownames(matrix) <- df$motifs
    colnames(matrix) <- df$conditions
  } else {
    rownames <- unique(df$motifs)
    colnames <- unique(df$conditions)
    matrix <- sapply(X = colnames, FUN = function(condition){
      sapply(X = rownames,
             FUN = function(x){return(return_score(df,x,condition))})
   })
  }
  return(matrix)
}

#' Retrieve importance score
#'
#' This function takes as input a data.frame coming
#' from `models2dataframe` call, a single motif name and a single condition,
#' retrieving the importance score for that motif in that condition.
#' If the score cannot be retrieved (i.e. the motif is not among the top k
#' motifs in that condition) 0 is assigned
#'
#' @param df the data.frame coming from `models2dataframe`, containing the top
#' k motif in each condition and their importance score
#'
#' @param motif_name the name of a single motif
#'
#' @param condition the name of a single condition
#'
#' @return the importance score for that particular motif, in that particular
#' condition. If the score cannot be retrieved (i.e. the motif is not among the top k
#' motifs in that condition) 0 is assigned
return_score <- function(df,motif_name,condition){
  val <- df[(df$motifs == motif_name & df$conditions == condition),]$coef
  val <- ifelse(identical(val,numeric(0)),0,val)
  return(val)
}
