#' Feature auto encoder class
#'
#' An auto-encoder to perform dimensional reeduction on features
#'
#' #' @references
#' \insertRef{GCN}{scRGNet}
#'
#' @importFrom torch nn_module nn_linear
AE <- torch::nn_module(
    classname = "AE",
    public = list(
        #' @field dim Dimension of features of input matrix
        dim = NULL,
        #' @field fc1 First layer of encoder
        fc1 = NULL,
        #' @field fc2 Hidden layer decoder
        fc2 = NULL,
        #' @field fc3 Hidden layer decoder
        fc3 = NULL,
        #' @field fc4 Second layer decoder
        fc4 = NULL
    ),
    #' @description
    #' Create a new feature autoencoder object.
    #' @param dim Dimension of features of input.
    #' @return A new `AE` object.
    initialize = function(dim) {
        self$dim <- dim
        self$fc1 <- torch::nn_linear(dim, 512)
        self$fc2 <- torch::nn_linear(512, 128)
        self$fc3 <- torch::nn_linear(128, 512)
        self$fc4 <- torch::nn_linear(512, dim)
    },
    #' @description
    #' Perform demension reduction to encode feature through the first two layers
    #' @return encoded matrix
    encode = function(x) {
        h1 <- torch::nnf_relu(self$fc1(x))
        return(torch::torch_relu(self$fc2(h1)))
    },
    #' @description
    #' Reconstruct to decode feature through the last two layers
    #' @return reconstructed matrix
    decode = function(z) {
        h3 <- torch::nnf_relu(self$fc3(z))
        return(torch::torch_relu(self$fc4(h3)))
    },
    #' @description
    #' Forward feeding
    forward = function(x) {
        ## If shape invalid, check view here
        z <- self$encode(x$view(c(-1, self$dim)))
        return(
            list(
                "recon" = self$decode(z),
                "z"     = z
            )
        )
    }
)
# TODO: if have time, do a variational auto-encoder

# [END]
