#' One standard-deviation based KNN graph (Single-thread vversion)
#'
#' Calculate a KNN graph from a feature matrix using stats one-std based methods.
#'
#' @param feature_mat A feature matrix based on which KNN graph is computed
#' @param k Number of neighbours in KNN graph
#'
#'
calculate_knn_graph_distance_matrix_StatsSingleThread <- function(feature_mat, k) {

    ## For each cell, calculate the distance to save memory
    start_time <- Sys.time()

    for (i in seq(feature_mat$shape[1])) {
        if ((i %% 10000) == 0)
            message(sprintf("Start pruning %i th cell. Cost %f seconds...", i, start_time))

        flat <- feature_mat[i, ]$reshape(c(1, -1))
        dist_mat
        for (j in seq(k)) {
            edgeList
        }
    }

    return(edgeList)
}



#' Generate a cell graph
#'
#' Compute a cell graph from a feature matrix
#'
#' @param feature_mat A feature matrix
#' @param k Number of neighbours
#'
#' @export
generateGraph <- function(feature_mat, k = 7) {

    edgeList <- calculate_knn_graph_distance_matrix_StatsSingleThread(feature_mat, k = k)

    return(
        list(
            "adj" = NULL,
            "edgeList" = NULL
        )
    )
}
