#' Plot a cell network
#'
#' plot
#'
#' @param net A graph object
#' @param group Whether to show cell community in the network
#'
#' @export
#' @import igraph
plotCellNet <- function(net, group = TRUE) {

    if (group)
        clp <- igraph::cluster_label_prop(net)

    plot(clp, net,
         vertex.shape = "none",
         vertex.label.cex = 0.5,
         vertex.size = 9
         )
}

plotDegree <- function(net) {
    d_dist <- igraph::degree(net)
    hist(d_dist)
}

plotLogRank <- function(net, title = "A log-log Plot of Connectivities for Cell Network") {
    dg       <- igraph::degree(net)
    rankFreq <- table(dg) # get rank and frequency of the graph
    freqRank <- as.integer(names(rankFreq)) # seperate ranks from rankFreq
    freq     <- as.integer(rankFreq) # seperate frequency from rankFreq

    plot(log10(freqRank + 1),
         log10(freq),
         type = "b", # type b means draw for both lines and dots
         pch  = 13,   # shape of points
         bg   = "#A5F5CC",
         xlab = "log(rank)", ylab = "log(frequency)",
         main = title)
}
