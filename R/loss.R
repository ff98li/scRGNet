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

#' Vallina MSE
#'
#' Calculate vallina mse loss
#'
#' @param input tensor
#' @param target tensor
#' @param reduction Reduction method
#'
#' @return tensor
#'
#' @importFrom torch torch_mean torch_sum torch_broadcast_tensors nnf_mse_loss
vallina_mse_loss_function <- function(input, target, reduction = 'none') {

    if (reduction %in% c('none', 'mean', 'sum')) {
        ;
    } else {
        stop(sprintf("%s is not a valid value for reduction", reduction))
    }

    if (any(input$size() != target$size())) {
        message(
            cat(
                sprintf(
                    "Using a target size (%i) that is different to the input size (%i).
                     This will likely lead to incorrect results due to broadcasting.
                     Please ensure they have the same size.", target$size(), input$size()
                )
            )
        )
    }

    if (target$requires_grad) {
        ret <- (input - target) ** 2
        if (reduction != 'none') {
            if (reduction == "mean") {
                ret <- torch::torch_mean(ret)
            } else if (reduction == "sum") {
                ret <- torch::torch_sum(ret)
            }
        }
    } else {
        expanded_input  <- torch::torch_broadcast_tensors(input)[[1]]
        expanded_target <- torch::torch_broadcast_tensors(target)[[1]]
        ret <- torch::nnf_mse_loss(input     = expanded_input,
                                   target    = expanded_target,
                                   reduction = reduction)
    }

    return(ret)
}

#' Regulation MSE Loss Function
#'
#' Measures the element-wise mean squared error for regulation input, now only support LTMG.
#'
#' @param input tensor
#' @param target tensor
#' @param regu_mat Matrix
#' @param reduction String
#'
#' @return tensor
#'
regulation_mse_loss_function <- function(input, target, regu_mat, reduction = 'none') {

    if (reduction %in% c('none', 'mean', 'sum')) {
        ;
    } else {
        stop(sprintf("%s is not a valid value for reduction", reduction))
    }

    if (any(input$size() != target$size())) {
        message(
            cat(
                sprintf(
                    "Using a target size (%i) that is different to the input size (%i).
                     This will likely lead to incorrect results due to broadcasting.
                     Please ensure they have the same size.", target$size(), input$size()
                )
            )
        )
    }

    ret <- (input - target) ** 2
    ret <- torch::torch_mul(ret, regu_mat)

    if (reduction != 'none') {
        if (reduction == 'mean') {
            ret <- torch::torch_mean(ret)
        } else{
            ret <- torch::torch_sum(ret)
        }
    }

    return(ret)
}

#' Loss function regularised by the graph information
#'
#' Reconstruction + KL divergence losses summed over all elements and batch
#'
#' @param recon_x Reconstructed scRNA-seq matrix
#' @param x Original input scRNA-seq matrix
#' @param mu mu
#' @param logvar logvar
#' @param graph_regu graph_regu
#' @param gamma_param gamma_param
#' @param regu_mat reg_mat
#' @param regu_type Type of regulariser
#' @param regu_param reg_param
#' @param model AE
#' @param reduction sum
#'
#'
#' #' @references
#' \insertRef{scGNN}{scRGNet}
#'
loss_function_graph <- function(recon_x,
                                x,
                                mu          = NULL,
                                logvar      = NULL,
                                graph_regu  = NULL,
                                gamma_param = 1.0,
                                regu_mat    = NULL,
                                regu_type   = NULL,
                                regu_param  = 0.001,
                                model       = "AE",
                                reduction   = "sum") {
    target <- x
    use_regu <- !(is.null(regu_type))
    if (use_regu)
        target$requires_grad <- use_regu

    # Euclidean
    BCE <- gamma_param * vallina_mse_loss_function(recon_x, target, reduction = reduction)

    loss <- 0
    if (is.null(regu_type)) {
        loss <- BCE
    } else if (regu_type == "LTMG") {
        penalty <- regu_param * regulation_mse_loss_function(recon_x, target, regu_mat, reduction)
        loss    <- BCE + penalty
    } ## Other regularisation currently not supported

    return(loss)
}
