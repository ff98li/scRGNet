#' scRNA-seq dataset class
#'
#' An R6 class to store scRNA-seq data for analysis using Torch
#'
#'
#' @references
#' \insertRef{scGNN}{scRGNet}
#' \insertRef{torch}{scRGNet}
#' \insertRef{r6}{scRGNet}
#'
#' @export
#' @importFrom torch dataset as_array
#' @importFrom Matrix t
scDataset <- torch::dataset(
    name   = "scDataset",
    #' @description
    #' Create a new scDataset object.
    #' @param data A matrix of pre-processed scRNA-seq data, with genes as rows and samples as columns
    #' @param transform A function to transform the matrix
    #' @return A new `scDataset` object.
    initialize = function(data, transform = NULL) {
        self$features  <- Matrix::t(data)
        self$transform <- transform
    },
    #' @description
    #' Get the number of cells in the dataset
    #' @return number of cells
    .length = function() {
        return(dim(self$features)[1])
    },
    #' @description
    #' Get the expression values of a cell
    #' @return A list of sample information
    #' \itemize{
    #'   \item index  - Index of sample in the feature matrix
    #'   \item sample - Name of the sample
    #'   \item expr   - Gene expression values in the sample
    #' }
    .getitem = function(idx) {
        if (is(idx, "torch_tensor"))
            idx <- torch::as_array(idx)

        sample <- self$features[idx, ]

        if (!is.null(self$transform))
            sample <- self$transform(sample)

        return(
            list(
                "index"  = idx,
                "sample" = self$features@Dimnames[[1]][idx],
                "expr"   = sample
            )
        )
    }
)

# [END]
