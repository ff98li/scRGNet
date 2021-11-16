#' Plot a cell network
#'
#' plot
#'
#' @param net A graph object
#'
#' @export
plotCellNet <- function(net) {
    igraph::plot.igraph(net,
                        vertex.label.cex = 0.1)
}
