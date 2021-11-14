#' GCN Loss function
#'
#' Loss function for optimising GCN using either AE or VAE
#'
#' @param VAE Whether to use variational auto-encoder or not. Default False.
#' @param logvar Used when VAE is set to TRUE. Default is NULL when using AE.
#' @param preds Predictions made by GCN during training
#' @param labels True values to checked against preds to calculate loss
#' @param mu Mean of the Gaussian for auto-encoder
#' @param n_nodes Number of nodes in the graph
#' @param norm Norm
#' @param pos_weight Weight parameter used to calculate loss
#'
#' @return Logistic-cross-entropy loss
#'
#' #' @references
#' \insertRef{scGNN}{scRGNet}
#'
#' @importFrom torch torch_mean torch_pow torch_sum nnf_binary_cross_entropy_with_logits torch_exp
loss_function_gcn <- function(VAE    = FALSE,
                              logvar = NULL,
                              preds,
                              labels,
                              mu,
                              n_nodes,
                              norm,
                              pos_weight) {

    cost <- norm * torch::nnf_binary_cross_entropy_with_logits(
        input      = preds,
        target     = labels,
        pos_weight = labels * pos_weight
    )

    if (VAE) {
        if (is.null(logvar)) {
            stop("Missing argument logvar: Must provide to use GVAE")
        }
        KLD <- -(0.5) / n_nodes * torch::torch_mean(
            torch::torch_sum(
                1 + 2 * logvar - torch::torch_pow(mu, 2) - torch::torch_pow(torch::torch_exp(logvar), 2)
            ), 1
        )
        return(cost + KLD)
    } else {
        return(cost)
    }
}
