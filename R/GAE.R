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
            ## TODO: test it in model training
            data  = matrix(c(in_features, out_features)),
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


#' GCN Model Auto-enccoder
#'
#' Auto-encoder for GCN
#'
#' #' @references
#' \insertRef{GCN}{scRGNet}
#'
#' @export
#' @import R6
#' @import torch
GCNModelAE <- torch::nn_module(
    classname = "GCNModelAE",
    inherit = GraphCovolution,
    public = list(
        gc1 = NULL,
        gc2 = NULL,
        dc = NULL
    ),
    initialize <- function(input_feat_dim,
                           hidden_dim1,
                           hidden_dim2,
                           dropout) {
        self$gc1 <- GraphCovolution(input_feat_dim,
                                    hidden_dim1,
                                    dropout,
                                    act = torch::nnf_relu)
        self$gc2 <- GraphCovolution(hidden_dim1,
                                    hidden_dim2,
                                    dropout,
                                    act = function(x) return(x))
        self.dc <- .
    }
)
