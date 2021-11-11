#' Graph Convolutional Neural Network Layer
#'
#' A GCN layer class
#'
#' #' @references
#' \insertRef{GCN}{scRGNet}
#'
#' @export
#' @import R6
#' @import torch
GraphCovolution <- torch::nn_module(
    classname = "GraphCovolution",
    public    = list(
        #' @field in_features in_features
        in_features      = NULL,

        #' @field out_features out_features
        out_features     = NULL,

        #' @field dropout dropout
        dropout          = NULL,

        #' @field act activation function
        act              = NULL,

        #' @field weight weight of the model
        weight           = NULL
    ),
    #' @description
    #' Create a new GCN layer object.
    #' @param in_features in_features.
    #' @param out_features out_features
    #' @param dropout default 0
    #' @param act The activation function used by the neuron. Default ReLU
    #' @return A new `GraphCovolution` object.
    initialize = function(in_features,
                          out_features,
                          dropout = 0,
                          act     = torch::nnf_relu) {
        self$in_features  <- in_features
        self$out_features <- out_features
        self$dropout      <- dropout
        self$act          <- act
        self$weight       <- torch::nn_parameter(torch::torch_tensor(
            ## TODO: test it
            data  = matrix(c(in_features, out_features), nrow = 2, ncol = 1),
            dtype = torch::torch_float()
            )
        )
        self$reset_parameters()
    },
    #' @description
    #' Forward feeding
    # @param input An input matrix
    # @param adj An adjacency matrix of a graph
    #' @return Loss
    forward = function(input, adj) {
        input   <- torch::nnf_dropout(input, self$dropout, self$training)
        support <- torch::torch_mm(input, self$weight)
        output  <- torch::torch_mm(adj, support)
        output  <- self$act(output)
        return(output)
    },
    #' @description
    #' Reset weight
    reset_parameters = function() {
        torch::nn_init_xavier_uniform_(self$weight)
    },
    print = function(...) {
        cat(
            "GraphCovolution",
            " (",
            as.character(self$in_features),
            " -> ",
            as.character(self$out_features),
            ")"
        )
    }
)
