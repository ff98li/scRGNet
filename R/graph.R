#' One standard-deviation based KNN graph (Single-thread vversion)
#'
#' Calculate a KNN graph from a feature matrix using stats one-std based methods.
#'
#' @param feature_mat A feature matrix based on which KNN graph is computed
#' @param k Number of neighbours in KNN graph
#'
#' @return edgeList
#'
#' @importFrom stats sd
calculate_knn_graph_distance_matrix_StatsSingleThread <- function(feature_mat, k) {

    cell_list  <- rownames(feature_mat)
    ## For each cell, calculate the distance to save memory
    start_time <- Sys.time()
    edgeList   <- list()

    for (i in seq(nrow(feature_mat))) {
        if ((i %% 10000) == 0)
            message(sprintf("Start pruning %i th cell. Cost %f seconds...", i, Sys.time() - start_time))

        dist_array <- array(
            apply(feature_mat, 1, function(x)
                sqrt(sum((feature_mat[i, ] - x)**2))
                )
            )
        nn         <- order(dist_array)[1:k]
        k_dist     <- dist_array[nn]
        boundary   <- mean(k_dist) + stats::sd(k_dist)
        for (j in seq(k)) {
            if (dist_array[nn[j]] <= boundary) {
                weight <- 1.0
            } else {
                weight <- 0.0
            }
            edgeList[[i]] <- c(i, nn[j], weight)
        }
    }

    edgeList <- do.call(rbind, edgeList)
    edgeList <- as.data.frame(edgeList)
    edgeList[, 1] <- cell_list[edgeList[, 1]]
    edgeList[, 2] <- cell_list[edgeList[, 2]]
    colnames(edgeList) <- c("V1", "V2", "weight")

    return(edgeList)
}



#' Generate a cell network
#'
#' Compute a cell network from a feature matrix. More methods will be added in future.
#'
#' @param feature_mat A feature matrix
#' @param k Number of neighbours
#'
#' @export
#' @importFrom igraph graph_from_data_frame
generateNetwork <- function(feature_mat, k = 7) {

    cell_list <- rownames(feature_mat)
    edgeList <- calculate_knn_graph_distance_matrix_StatsSingleThread(feature_mat, k = k)
    graph <- igraph::graph_from_data_frame(edgeList, directed = FALSE)

    #adj   <- igraph::as_adj(graph, attr = "weight")
    return(graph)
}
