#' Plot Bipartite Graph of Motifs and Conditions
#'
#' This function generates a bipartite directed graph that visualizes
#' relationships between motifs and conditions. The edges represent the
#' importance of connections (e.g., regression coefficients), and the graph is
#' rendered interactively using Plotly.
#'
#' @param motif_names A character vector containing the names of motifs.
#' @param condition_names A character vector containing the names of conditions.
#' @param edges A data frame containing the edges of the graph.
#'
#' @return A Plotly object displaying an interactive bipartite graph where:
#' \itemize{
#'   \item Motifs and conditions are represented as nodes.
#'   \item Edges connecting nodes are color-coded based on the importance
#'   values (\code{coef}).
#'   \item Node hover labels provide additional information such as type
#'   (motif/condition).
#' }
#'
#' @importFrom igraph graph_from_data_frame V
#' @import ggraph
#' @import ggplot2
#' @importFrom plotly ggplotly layout
#' @importFrom magrittr %>%
#' @export
plot_digraph <- function(motif_names, condition_names, edges){

  `%>%` <- magrittr::`%>%`

  nodes <- data.frame(
    name = c(motif_names, condition_names),
    type = c(rep(FALSE, length(motif_names)), rep(TRUE, length(condition_names)))
  )

  graph <- igraph::graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

  node_positions <- data.frame(
    name = c(motif_names, condition_names),
    x = c(seq(1, 5, length.out = length(motif_names)), seq(1, 5, length.out = length(condition_names))),
    y = c(rep(1, length(motif_names)), rep(3, length(condition_names))),
    type = c(rep(FALSE, length(motif_names)), rep(TRUE, length(condition_names)))
  )

  igraph::V(graph)$x <- node_positions$x
  igraph::V(graph)$y <- node_positions$y

  edge_positions <- data.frame(
    x = igraph::V(graph)$x[match(edges$motifs, igraph::V(graph)$name)],
    y = igraph::V(graph)$y[match(edges$motifs, igraph::V(graph)$name)],
    xend = igraph::V(graph)$x[match(edges$conditions, igraph::V(graph)$name)],
    yend = igraph::V(graph)$y[match(edges$conditions, igraph::V(graph)$name)],
    Importance = edges$coef
  )

  edge_positions$mid_x <- (edge_positions$x + edge_positions$xend) / 2
  edge_positions$mid_y <- (edge_positions$y + edge_positions$yend) / 2
  edge_positions$Info <- paste0(
   "<br>", edges$motifs, " ⇔ ", edges$conditions, "<br>Importance: ", edges$coef
  )

  node_positions$Info <- paste0(
    "<br>","Node: ", node_positions$name, "<br>Type: ", ifelse(node_positions$type, "Condition", "Motif")
  )

  gg <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = edge_positions,
                 ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = Importance),
                 linewidth = 0.5) +
    ggplot2::geom_point(data = node_positions,
               ggplot2::aes(x = x, y = y, fill = type, shape = as.factor(type),
                   size = as.factor(type), group = Info),
               show.legend = FALSE) +
    ggplot2::geom_point(data = edge_positions,
               ggplot2::aes(x = mid_x, y = mid_y, group = Info),
               color = "transparent", alpha = 0.3, size = 3) +

    ggplot2::ggtitle("K most important motifs bipartite graph") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 25, face = "bold", hjust = 0.5)) +

    ggplot2::scale_size_manual(values = c(4,10)) +
    ggplot2::scale_shape_manual(values = c(15, 23)) +
    ggplot2::scale_fill_manual(values = c("brown", "aquamarine2")) +
    ggplot2::scale_color_gradient(low = "darkgoldenrod1", high = "black") +

    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()) +
    ggplot2::guides(fill = "none",
           shape = "none",
           size = "none")

  plotly_graph <- plotly::ggplotly(gg, tooltip = c("Info")) %>%
    plotly::layout(
        title = list(
          text = "K most important motifs bipartite graph",
          x = 0.5,
          xanchor = "center",
          y = 0.99,
          font = list(
            size = 25,
            family = "Arial, sans-serif"
          )
        ),
      hovermode = "closest",
      showlegend = FALSE,
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "black",
        font = list(
          color = "black",
          size = 12
        )
      )
    )

  return(plotly_graph)
}
