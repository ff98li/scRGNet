#' Plot a cell network
#'
#' plot
#'
#' @param graph A graph object
#'
#' @export
plotCellNet <- function(graph) {
    igraph::tkplot(graph)
}
