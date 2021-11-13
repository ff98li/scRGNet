#' scRNA-seq dataset class
#'
#' An R6 class to store scRNA-seq data for analysis using Torch
#'
#' @export
#' @importFrom torch dataset
scDataset <- torch::dataset(
    name = "scDataset",
    public = list(
        #' @field features A feature matrix with rows as cells and column as genes
        features = NULL,
        #' @field Any function to transform the matrix. Optional.
        transform = NULL
    ),
    #' @description
    #' Create a new scDataset object.
    #' @param data A matrix of pre-processed scRNA-seq data, with genes as rows and samples as columns
    #' @param transform A function to transform the matrix
    #' @return A new `scDataset` object.
    initialize = function(data, transform = NULL) {
        self$feature <- Matrix::t(data)
        self$transform <- transform
    },
    #' @description
    #' Get the number of cells in the dataset
    #' @return number of cells
    len = function() {
        return(dim(self$feature)[1])
    },
    #' @description
    #' Get the expression values of a cell
    get_sample = function(idx) {
        sample <- self$features[idx, ]
        if (!is.null(self$transform))
            sample <- self$transform(sample)

        return(sample)
    }
)
