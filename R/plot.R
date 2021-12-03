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
#' @param show_node_select Show option to highlight nodes and its neighbours when hovering.
#'     Intended for use in Shiny.
#'
#' @references
#' \insertRef{igraph}{scRGNet}
#' \insertRef{visNetwork}{scRGNet}
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
#' @importFrom igraph cluster_label_prop membership
#' @importFrom visNetwork toVisNetworkData visNetwork visNodes visOptions
#' @importFrom Rdpack reprompt
#' @importFrom methods is
plotCellNet <- function(net,
                        group            = TRUE,
                        title            = "Inferred Cell Network",
                        node_label_size  = NULL,
                        node_size        = NULL, ## default = 25
                        show_node_select = FALSE) {

    if (!methods::is(net, "igraph"))
        stop("Invalid argument for net. Must be an igraph object.")

    if (!is.logical(group))
        stop("Invalid argument for group. Must be logical.")

    if (!is.character(title))
        stop("Invalid argument for title. Must be a character vector.")

    if (is.null(node_size)) {
        ; # Use default setting
    } else {
        if (!is.numeric(node_size) | node_size < 1) {
            stop("Invalid argument for node size. Must be a numerical value greater than 1.")
        }
    }

    if (is.null(node_label_size)) {
        ; # Use default setting
    } else {
        if (!is.numeric(node_label_size) | node_label_size < 1) {
            stop("Invalid argument for node label size. Must be a numerical value greater than 1.")
        }
    }

    nodes <- visNetwork::toVisNetworkData(net)$nodes
    edges <- visNetwork::toVisNetworkData(net)$edges

    if (! is.null(node_label_size))
        nodes$"font.id" <- rep(node_label_size, dim(nodes)[1])

    if (group) {
        clp                <- igraph::cluster_label_prop(net)
        group_label        <- data.frame(as.list(igraph::membership(clp)))
        group_label        <- as.data.frame(t(group_label))
        group_label$label  <- rownames(group_label)
        nodes              <- merge(x = nodes, y = group_label, by.x = "label", by.y = "label")
        colnames(nodes)[which(colnames(nodes) == "V1")] <- "group"
    }

    net_plot <- visNetwork::visNetwork(nodes = nodes,
                                       edges = edges,
                                       main  = title)

    if (! is.null(node_size))
        net_plot <- visNetwork::visNodes(net_plot, size = node_size)

    if (show_node_select) {
        net_plot <- visNetwork::visOptions(net_plot,
                                           highlightNearest = list(enabled = T, hover = T),
                                           nodesIdSelection = TRUE)
    }

    return(net_plot)
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
