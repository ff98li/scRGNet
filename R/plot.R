#' Plot a cell network
#'
#' plot
#'
#' @param net A graph object
#' @param node_col Colour code of nodes
#'
#' @export
plotCellNet <- function(net, node_col) {

    clp <- igraph::cluster_label_prop(net)

    plot(clp, net,
         vertex.shape = "none",
         vertex.label.cex = 0.5,
         vertex.size = 9
         )
}
