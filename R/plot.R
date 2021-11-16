#' Plot a cell network
#'
#' plot
#'
#' @param net A graph object
#' @param group Whether to show cell community in the network
#'
#' @export
#' @import igraph
#' @importFrom graphics plot
plotCellNet <- function(net, group = TRUE) {

    if (group)
        clp <- igraph::cluster_label_prop(net)

    plot(clp, net,
         vertex.shape = "none",
         vertex.label.cex = 0.5,
         vertex.size = 9
         )
}

#' Plot a cell network
#'
#' plot
#'
#' @param net A graph object
#' @param title Title of the histogram
#'
#' @export
#' @importFrom igraph degree
#' @importFrom graphics hist
plotDegree <- function(net, title = "Distribution of Vertices in the Cell Network") {
    dg <- igraph::degree(net)
    brk <- seq(min(dg) - 0.5, max(dg) + 0.5, by = 1)
    graphics::hist(dg,
                   breaks = brk,
                   xlim   = range(dg) + c(-1, 1),
                   main   = title,
                   xlab   = "Degree",
                   ylab   = "Vertices"
    )
}

#' Plot a cell network
#'
#' plot
#'
#' @param net A graph object
#' @param title Title of the log-log plot
#'
#' @export
#' @importFrom igraph degree
#' @importFrom graphics plot
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
