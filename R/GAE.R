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


#' Inner Product Decoder
#'
#' Decoder using inner product for prediction.
#'
#' #' @references
#' \insertRef{GCN}{scRGNet}
#'
#' @export
#' @import R6
#' @import torch
InnerProductDecoder <- torch::nn_module(
    classname = "InnerProductDecoder",
    public    = list(
        #' @field dropout dropout
        dropout = NULL,

        #' @field act activation function of the neuron
        act     = NULL
    ),

    #' @description
    #' Create an inner product decoder object.
    #' @param dropout dropout
    #' @param act The activation function used by the neuron. Default sigmoid.
    #' @return A new `InnerProductDecoder` object.
    initialize = function(dropout, act = torch::torch_sigmoid) {
        self$dropout <- dropout
        self$act     <- act
    },
    forward = function(z) {
        z   = torch::nnf_dropout(z, self$dropout, training = self$training)
        adj = self$act(torch::torch_mm(z, torch::torch_transpose(z)))
        return(adj)
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
        #' @field gc1 the first GCN layer
        gc1 = NULL,
        #' @field gc2 the second GCN layer
        gc2 = NULL,
        #' @field dc Decoder using inner product for prediction
        dc  = NULL
    ),
    #' @description
    #' Create an GCN auto-encoder object.
    #' @param input_feat_dim dimension of input feature matrix
    #' @param hidden_dim1 dimension of matrix in the first hidden layer
    #' @param hidden_dim2 dimension of matrix in the second hidden layer
    #' @param dropout dropout
    #' @return A new `GCNModelAE` object.
    initialize = function(input_feat_dim,
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
        self.dc  <- InnerProductDecoder(dropout, act = function(x) return(x))
    }
)
## TODO: Add Auto-Encoding Variational Bayes as an better alternative to this

#' GCN Model Variational Auto-enccoder auto-encoding variational Bayes
#'
#' Auto-encoder for GCN using
#'
#' #' @references
#' \insertRef{GCN}{scRGNet}
#'
# @export
#' @import R6
#' @import torch
GCNModelVAE <- torch::nn_module(
    classname = "GCNModelVAE",
    public = list(
        #' @field gc1 the first GCN layer
        gc1 = NULL,
        #' @field gc2 the second GCN layer
        gc2 = NULL,
        #' @field gc3 the third GCN layer
        gc3 = NULL,
        #' @field dc Decoder using inner product for prediction
        dc  = NULL
    ),
    #' @description
    #' Create an GCN auto-encoder object.
    #' @param input_feat_dim dimension of input feature matrix
    #' @param hidden_dim1 dimension of matrix in the first hidden layer
    #' @param hidden_dim2 dimension of matrix in the second hidden layer
    #' @param dropout dropout
    #' @return A new `GCNModelAE` object.
    initialize = function(input_feat_dim,
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
        self$gc3 <- GraphCovolution(hidden_dim1,
                                    hidden_dim2,
                                    dropout,
                                    act = function(x) return(x))
        self.dc  <- InnerProductDecoder(dropout, act = function(x) return(x))
    },
    encode = function(x, adj) {
        hidden1 <- self$gc1(x, adj)
        encoded <- list(
            self$gc2(hidden1, adj),
            self@gc3(hidden1, adj)
        )
    },
    reparameterize = function(mu, logvar) {
        if (self$training) {
            std <- torch::torch_exp(logvar)
            eps <- torch::torch_rand_like(std)
            ## Try addmm if this doesn't work properly
            return(torch::torch_addcmul(mu, eps, std))
        }
    },
    forward =  function(x, adj) {
        params <- self$encode(x, adj)
        mu     <- params[[1]]
        logvar <- params[[2]]
        z      <- self$reparameterize(mu, logvar)
        return(
            list(
                z      = z,
                mu     = mu,
                logvar = logvar
            )
        )
    }
)
