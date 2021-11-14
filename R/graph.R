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
