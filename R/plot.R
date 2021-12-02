#' Cell network plotting function
#'
#' A plotting function to plot the inferred cell net-work from generateNetwork.
#'
#' @param net An igraph object generayed by generateNetwork.
#'     It contains the inferred cell-cell relationship.
#' @param group A logical vector indicating whether to highlight cell communities in the network.
#'     If FALSE, they will not be highlighted. Default TRUE.
#' @param title A character vector as the title of the network plot.
#' @param node_label_size Size of node label
#' @param node_size Size of node
#'
#' @references
#' \insertRef{igraph}{scRGNet}
#'
#' @examples
#' # Example 1:
#' # Tested examples. Not run for fast package compiling
#' \dontrun{
#' # Accessing the demo gene_counts_small dataset available with the package
#' inputCountsPath <- system.file("extdata", "GSE138852_small.csv", package = "scRGNet")
#' # Preprocess the raw counts
#' counts <- preprocessCSV(path = inputCountsPath)
#' ltmg <- runLTMG(counts)
#' hyperParams <- setHyperParams(regu_epochs = 5L)
#' hardwareSetup <- setHardware(coresUsage = 1L)
#' z <- runFeatureAE(scDataset = counts, LTMG_mat = ltmg, hyperParams, hardwareSetup)
#' net <- generateNetwork(z)
#' plotCellNet(net)
#'}
#'
#' @export
#' @import igraph
#' @importFrom Rdpack reprompt
#' @importFrom methods is
plotCellNet <- function(net,
                        group           = TRUE,
                        title           = "Inferred Cell Network",
                        node_label_size = 0.5,
                        node_size       = 9) {

    if (!methods::is(net, "igraph"))
        stop("Invalid argument for net. Must be an igraph object.")

    if (!is.logical(group))
        stop("Invalid argument for group. Must be logical.")

    if (!is.character(title))
        stop("Invalid argument for title. Must be a character vector.")

    nodes <- igraph::as_data_frame(net, what = "vertices")
    edges <- igraph::as_data_frame(net, what = "edges")
    nodes <- cbind(id = 1:dim(nodes)[1], nodes)
    colnames(nodes)[1] <- "id"
    colnames(nodes)[2] <- "label"
    edges <- merge(x = edges,y = nodes, by.x = "from", by.y = "label", all.x = TRUE)
    edges <- edges[ , !names(edges) %in% c("from")]
    edges <- merge(x = edges,y = nodes, by.x = "to", by.y = "label", all.x = TRUE)
    edges <- edges[ , !names(edges) %in% c("to")]
    colnames(edges) <- c("from", "to")

    plot_args <- list(
        node_label_size = node_label_size,
        node_size       = node_size,
        title           = title
    )

    if (group) {
        clp                <- igraph::cluster_label_prop(net)
        group_label        <- data.frame(as.list(igraph::membership(clp)))
        group_label        <- as.data.frame(t(group_label))
        group_label$label  <- rownames(group_label)
        nodes              <- merge(x = nodes, y = group_label, by.x = "label", by.y = "label")
        colnames(nodes)[3] <- "group"
    }

    visNetwork::visNetwork(nodes = nodes,
                           edges = edges,
                           main  = title)
}

## Helper function to handle multiple graph object
#' @param ...
#' @NoRd
#' @import magrittr
plotCellNet_output <- function(..., plot_args) {
    graphics::plot(...,
                   vertex.shape     = plot_args$node_shape,
                   vertex.label.cex = plot_args$node_label_size,
                   vertex.size      = plot_args$node_size,
                   main             = plot_args$title
    )
}

#' Show Degree Distribution in Network
#'
#' Plot distribution for degrees of cells in the network.
#'
#' @param net A graph object
#' @param title A character vector as the title of the histogram.
#'
#' @references
#' \insertRef{igraph}{scRGNet}
#'
#' @examples
#' # Example 1:
#' # Tested examples. Not run for fast package compiling
#' \dontrun{
#' # Accessing the demo gene_counts_small dataset available with the package
#' inputCountsPath <- system.file("extdata", "GSE138852_small.csv", package = "scRGNet")
#' # Preprocess the raw counts
#' counts <- preprocessCSV(path = inputCountsPath)
#' ltmg <- runLTMG(counts)
#' hyperParams <- setHyperParams(regu_epochs = 5L)
#' hardwareSetup <- setHardware(coresUsage = 1L)
#' z <- runFeatureAE(scDataset = counts, LTMG_mat = ltmg, hyperParams, hardwareSetup)
#' net <- generateNetwork(z)
#' plotDegree(net)
#'}
#'
#' @export
#' @importFrom igraph degree
#' @importFrom graphics hist
#' @importFrom methods is
plotDegree <- function(net,
                       title = "Distribution of Vertices in the Cell Network") {

    if (!methods::is(net, "igraph"))
        stop("Invalid argument for net. Must be an igraph object.")

    if (!is.character(title))
        stop("Invalid argument for title. Must be a character vector.")

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

#' Log-log plot of Network
#'
#' Generate a A plot of log-frequency against log-rank for the degree distribution. It is useful in examing whether the network has a scale-free topology.
#'
#' @param net A graph object
#' @param title A string vector as the title of the log-log plot
#'
#' @references
#' \insertRef{igraph}{scRGNet}
#'
#' @examples
#' # Example 1:
#' # Tested examples. Not run for fast package compiling
#' \dontrun{
#' # Accessing the demo gene_counts_small dataset available with the package
#' inputCountsPath <- system.file("extdata", "GSE138852_small.csv", package = "scRGNet")
#' # Preprocess the raw counts
#' counts <- preprocessCSV(path = inputCountsPath)
#' ltmg <- runLTMG(counts)
#' hyperParams <- setHyperParams(regu_epochs = 5L)
#' hardwareSetup <- setHardware(coresUsage = 1L)
#' z <- runFeatureAE(scDataset = counts, LTMG_mat = ltmg, hyperParams, hardwareSetup)
#' net <- generateNetwork(z)
#' plotLogRank(net)
#'}
#'
#' @export
#' @importFrom igraph degree
#' @importFrom graphics plot
#' @importFrom methods is
plotLog <- function(net,
                    title = "A log-log Plot of Connectivities for Cell Network") {


    if (!methods::is(net, "igraph"))
        stop("Invalid argument for net. Must be an igraph object.")

    if (!is.character(title))
        stop("Invalid argument for title. Must be a character vector.")

    dg       <- igraph::degree(net)
    rankFreq <- table(dg) # get rank and frequency of the network
    freqRank <- as.integer(names(rankFreq)) # seperate ranks from rankFreq
    freq     <- as.integer(rankFreq) # seperate frequency from rankFreq

    graphics::plot(x    = log10(freqRank + 1),
                   y    = log10(freq),
                   type = "b", # type b means draw for both lines and dots
                   pch  = 13,   # shape of points
                   bg   = "#A5F5CC",
                   xlab = "log(rank)", ylab = "log(frequency)",
                   main = title)
}

# [END]
