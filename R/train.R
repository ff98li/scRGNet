#' Main Training Model
#'
#' Train in main
#'
#' @param epoch epoch
#' @param train_loader A torch dataloader loaded with an scDataset object
#' @param model A feature auto-encoder object
#' @param optimiser Optimiser
#' @param regu_mat LTMG regulation matrix
#' @param hyperParams Hyper-parameters user defined to tune the model
#' @param device hardware to train the model
#' @param EMflag Emflag
#'
#' #' @references
#' \insertRef{scGNN}{scRGNet}
#'
#' @importFrom coro loop
train <- function(epoch,
                  train_loader,
                  model,
                  optimiser,
                  regu_mat = NULL,
                  hyperParams,
                  device,
                  EMflag = FALSE) {
    regu <- !(is.null(regu_mat))
    model$train()
    train_loss <- 0
    loss       <- 0
    batch_idx  <- 0
    coro::loop(
        for (batch in train_loader) {
            batch_idx <- batch_idx + 1

            if (regu) {
                regu_mat_batch <- regu_mat[, batch_idx]
                regu_mat_batch <- regu_mat_batch$to(device)
            }

            optimiser$zero_grad()
            ## if (is(model, "AE")) ## for later VAE option
            train_output <- model(batch$sample)

            if (EMflag & !(hyperParams$EMreguTag)) {
                loss <- loss_function_graph(
                    recon_x    = train_output$recon,
                    x          = batch$sample$view(c(-1, train_output$sample$shape[2])),
                    regu_param = hyperParams$regu_alpha,
                    )
            } else {
                loss <- loss_function_graph(
                    recon_x    = train_output$recon,
                    x          = batch$sample$view(c(-1, train_output$sample$shape[2])),
                    regu_mat   = regu_mat_batch,
                    regu_type  = "LTMG",
                    regu_param = hyperParams$regu_alpha
                )
            }

            l1 <- 0.0
            l2 <- 0.0

            for (param in model$parameters) {
               l1 <- l1 + param$abs()$sum()
               l2 <- l2 + param$pow(2)$sum()
            }

            loss <- loss + hyperParams$L1 * l1 + hyperParams$L2 * l2

            loss$backward()
            train_loss <- loss$item()
            optimiser$step()
            if (batch_idx == 0) {
               recon_batch_all <- train_output$recon
               data_all        <- batch
               z_all           <- train_output$z
            } else {
                recon_batch_all <- torch::torch_cat(tensors = c(recon_batch_all, train_output$recon), dim = 1)
                data_all        <- torch::torch_cat(tensors = c(data_all, batch), dim = 1)
                z_all           <- torch::torch_cat(tensors = c(z_all, train_output$z), dim = 1)
            }
        }
    )

    risk <- train_loss / length(train_loader$dataset)

    message(sprintf("Epoch: %i   Average Loss: %f", epoch, risk))

    output <- list(
        "recon"   = recon_batch_all,
        "orginal" = data_all,
        "z"       = z_all
    )

    return(output)
}
